package application

import (
	"bytes"
	"context"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"log"
	"net/url"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/adrg/xdg"
	"github.com/google/uuid"
	getter "github.com/hashicorp/go-getter/v2"
	"github.com/hashicorp/nomad/jobspec2"
	"github.com/pkg/errors"

	"github.com/input-output-hk/cicero/src/domain"
)

type EvaluationService interface {
	ListActions(src string) ([]string, error)

	// Convenience wrapper around `EvaluateAction`.
	EvaluateActionDefinition(src, name string) (domain.ActionDefinition, error)

	EvaluateAction(src, name string, id uuid.UUID, inputs map[string]interface{}) (domain.ActionDefinition, error)

	// TODO add EvaluateJob() and EvaluateFacts/Outputs() for better performance (less JSON) with lazy languages
}

func parseSource(src string) (fetchUrl *url.URL, evaluator string, err error) {
	fetchUrl, err = url.Parse(src)
	if err != nil {
		return
	}

	evaluator = fetchUrl.Fragment
	fetchUrl.Fragment = ""
	fetchUrl.RawFragment = ""

	return
}

type evaluationService struct {
	Evaluators []string // Default evaluators. Will be tried in order if none is given for a source.
	Env        []string // NAME=VALUE or just NAME to inherit from process environment
	logger     *log.Logger
}

func NewEvaluationService(evaluators, env []string) EvaluationService {
	return &evaluationService{
		Evaluators: evaluators,
		Env:        env,
		logger:     log.New(os.Stderr, "evaluationService: ", log.LstdFlags),
	}
}

func (e *evaluationService) evaluate(src, command string, extraEnv ...string) ([]byte, error) {
	fetchUrl, evaluator, err := parseSource(src)
	if err != nil {
		return nil, err
	}

	cacheDir := os.Getenv("CICERO_CACHE_DIR")
	if cacheDir == "" {
		cacheDir = xdg.CacheHome + "/cicero"
	}
	cacheDir += "/sources"

	dst, err := filepath.Abs(cacheDir + "/" + base64.RawURLEncoding.EncodeToString([]byte(src)))
	if err != nil {
		return nil, err
	}

	result, err := getter.GetAny(context.Background(), dst, fetchUrl.String())
	if err != nil {
		return nil, err
	}
	if result.Dst != dst {
		return nil, errors.WithMessage(err, "go-getter did not download to the given directory. This should never happen™")
	}

	tryEval := func(evaluator string) ([]byte, error) {
		cmd := exec.Command("cicero-evaluator-"+evaluator, command)
		extraEnv = append(extraEnv, "CICERO_ACTION_SRC="+dst)
		cmd.Env = append(os.Environ(), extraEnv...)

		e.logger.Printf("Running %s with env %v", strings.Join(cmd.Args, " "), extraEnv)

		if output, err := cmd.Output(); err != nil {
			message := "Failed to evaluate"

			var errExit *exec.ExitError
			if errors.As(err, &errExit) {
				message += fmt.Sprintf("\nStdout: %s\nStderr: %s", output, errExit.Stderr)
			}

			return nil, errors.WithMessage(err, message)
		} else {
			return output, err
		}
	}

	if evaluator != "" {
		if output, err := tryEval(evaluator); err != nil {
			return nil, errors.WithMessagef(err, `Evaluator "%s" specified in source failed`, evaluator)
		} else {
			return output, nil
		}
	} else {
		e.logger.Println("No evaluator given in source, trying all")
		messages := ""
		for _, evaluator := range e.Evaluators {
			if output, err := tryEval(evaluator); err != nil {
				message := fmt.Sprintf(`Evaluator %q failed: %s`, evaluator, err.Error())
				if messages != "" {
					messages += "\n"
				}
				messages += message
				e.logger.Print(message)
			} else {
				return output, nil
			}
		}
		return nil, errors.New("No evaluator succeeded.\n" + messages)
	}
}

func (e *evaluationService) EvaluateActionDefinition(src, name string) (domain.ActionDefinition, error) {
	return e.EvaluateAction(src, name, uuid.UUID{}, map[string]interface{}{})
}

func (e *evaluationService) EvaluateAction(src, name string, id uuid.UUID, inputs map[string]interface{}) (domain.ActionDefinition, error) {
	var def domain.ActionDefinition

	inputsJson, err := json.Marshal(inputs)
	if err != nil {
		return def, errors.WithMessagef(err, "Could not marshal inputs to JSON: %s", inputs)
	}

	output, err := e.evaluate(src, "eval",
		"CICERO_ACTION_NAME="+name,
		fmt.Sprintf("CICERO_ACTION_ID=%s", id),
		"CICERO_ACTION_INPUTS="+string(inputsJson),
	)
	if err != nil {
		return def, err
	}

	freeformDef := struct {
		domain.ActionDefinition
		Job *interface{} `json:"job"`
	}{}

	err = json.Unmarshal(output, &freeformDef)
	if err != nil {
		e.logger.Println(string(output))
		return def, errors.WithMessage(err, "While unmarshaling evaluator output into freeform definition")
	}

	def.Meta = freeformDef.Meta
	def.Failure = freeformDef.Failure
	def.Success = freeformDef.Success
	def.Inputs = freeformDef.Inputs
	if freeformDef.Job != nil {
		if job, err := json.Marshal(*freeformDef.Job); err != nil {
			return def, err
		} else {
			// escape HCL variable interpolation
			job = bytes.ReplaceAll(job, []byte("${"), []byte("$${"))

			if job, err := jobspec2.ParseWithConfig(&jobspec2.ParseConfig{
				Body:    []byte(`{"job":` + string(job) + "}"),
				AllowFS: false,
				Strict:  true,
			}); err != nil {
				return def, err
			} else {
				def.Job = job
			}
		}
	}

	e.addEnv(&def)

	return def, nil
}

func (e *evaluationService) addEnv(def *domain.ActionDefinition) {
	if def.Job == nil {
		return
	}
	for _, group := range def.Job.TaskGroups {
		for _, task := range group.Tasks {
			for _, envSpec := range e.Env {
				splits := strings.SplitN(envSpec, "=", 2)
				name := splits[0]
				if len(splits) == 2 {
					task.Env[name] = splits[1]
				} else if procEnvVar, exists := os.LookupEnv(name); exists {
					task.Env[name] = procEnvVar
				}
			}
		}
	}
}

func (e *evaluationService) ListActions(src string) ([]string, error) {
	output, err := e.evaluate(src, "list")
	if err != nil {
		return nil, err
	}

	var names []string
	if err := json.Unmarshal(output, &names); err != nil {
		e.logger.Println(string(output))
		return nil, errors.WithMessage(err, "While unmarshaling action names")
	}

	return names, nil
}
