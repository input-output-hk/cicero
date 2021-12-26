package service

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
	EvaluateAction(src, name string) (domain.ActionDefinition, error)
	EvaluateRun(src, name string, id uuid.UUID, inputs map[string]interface{}) (domain.RunDefinition, error)
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

type command struct {
	Command  []string
	ExtraEnv []string
}

func (e *evaluationService) evaluate(src string, command command) ([]byte, error) {
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
		cmd := exec.Command("cicero-evaluator-"+evaluator, command.Command...)
		extraEnv := append(command.ExtraEnv, "CICERO_ACTION_SRC="+dst)
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

func (e *evaluationService) EvaluateAction(src, name string) (domain.ActionDefinition, error) {
	var def domain.ActionDefinition

	if output, err := e.evaluate(src, command{
		Command:  []string{"eval", "meta", "inputs"},
		ExtraEnv: []string{"CICERO_ACTION_NAME=" + name},
	}); err != nil {
		return def, err
	} else if err := json.Unmarshal(output, &def); err != nil {
		e.logger.Println(string(output))
		return def, errors.WithMessage(err, "While unmarshaling evaluator output")
	}

	return def, nil
}

func (e *evaluationService) EvaluateRun(src, name string, id uuid.UUID, inputs map[string]interface{}) (domain.RunDefinition, error) {
	var instance domain.RunDefinition

	inputsJson, err := json.Marshal(inputs)
	if err != nil {
		return instance, errors.WithMessagef(err, "Could not marshal inputs to JSON: %s", inputs)
	}

	output, err := e.evaluate(src, command{
		Command: []string{"eval", "success", "failure", "job"},
		ExtraEnv: []string{
			"CICERO_ACTION_NAME=" + name,
			fmt.Sprintf("CICERO_ACTION_ID=%s", id),
			"CICERO_ACTION_INPUTS=" + string(inputsJson),
		},
	})
	if err != nil {
		return instance, err
	}

	freeformInstance := struct {
		domain.RunDefinition
		Job *interface{} `json:"job"`
	}{}

	err = json.Unmarshal(output, &freeformInstance)
	if err != nil {
		e.logger.Println(string(output))
		return instance, errors.WithMessage(err, "While unmarshaling evaluator output into freeform definition")
	}

	instance.Failure = freeformInstance.Failure
	instance.Success = freeformInstance.Success
	if freeformInstance.Job != nil {
		if job, err := json.Marshal(*freeformInstance.Job); err != nil {
			return instance, err
		} else {
			// escape HCL variable interpolation
			job = bytes.ReplaceAll(job, []byte("${"), []byte("$${"))

			if job, err := jobspec2.ParseWithConfig(&jobspec2.ParseConfig{
				Body:    []byte(`{"job":` + string(job) + "}"),
				AllowFS: false,
				Strict:  true,
			}); err != nil {
				return instance, err
			} else {
				instance.Job = job
			}
		}
	}

	e.addEnv(&instance)

	return instance, nil
}

func (e *evaluationService) addEnv(instance *domain.RunDefinition) {
	if instance.Job == nil {
		return
	}
	for _, group := range instance.Job.TaskGroups {
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
	output, err := e.evaluate(src, command{Command: []string{"list"}})
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
