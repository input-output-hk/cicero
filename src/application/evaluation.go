package application

import (
	"bytes"
	"context"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
	"net/url"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/adrg/xdg"
	getter "github.com/hashicorp/go-getter/v2"
	"github.com/hashicorp/nomad/jobspec2"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/pkg/errors"
)

type EvaluationService interface {
	EvaluateWorkflow(src string, name string, id uint64, inputs domain.Facts) (domain.WorkflowDefinition, error)
	ListWorkflows(src string) ([]string, error)
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
	logger     zerolog.Logger
}

func NewEvaluationService(evaluators, env []string) EvaluationService {
	return &evaluationService{
		Evaluators: evaluators,
		Env:        env,
		logger:     log.With().Str("component", "EvaluationService").Logger(),
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
		extraEnv = append(extraEnv, "CICERO_WORKFLOW_SRC="+dst)
		cmd.Env = append(os.Environ(), extraEnv...)

		e.logger.Info().Msgf("Running %s with env %v", strings.Join(cmd.Args, " "), extraEnv)

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
			return nil, errors.WithMessagef(err, `Evaluator "%s" from workflow source failed`, evaluator)
		} else {
			return output, nil
		}
	} else {
		e.logger.Info().Msg("No evaluator given in source, trying all")
		messages := ""
		for _, evaluator := range e.Evaluators {
			if output, err := tryEval(evaluator); err != nil {
				message := fmt.Sprintf(`Evaluator %q failed: %s`, evaluator, err.Error())
				if messages != "" {
					messages += "\n"
				}
				messages += message
				e.logger.Info().Msg(message)
			} else {
				return output, nil
			}
		}
		return nil, errors.New("No evaluator succeeded.\n" + messages)
	}
}

func (e *evaluationService) EvaluateWorkflow(src, name string, id uint64, inputs domain.Facts) (domain.WorkflowDefinition, error) {
	var def domain.WorkflowDefinition

	inputsJson, err := json.Marshal(inputs)
	if err != nil {
		return def, errors.WithMessagef(err, "Could not marshal workflow inputs to json: %s", inputs)
	}

	output, err := e.evaluate(src, "eval",
		"CICERO_WORKFLOW_NAME="+name,
		"CICERO_WORKFLOW_INSTANCE_ID="+fmt.Sprintf("%d", id),
		"CICERO_WORKFLOW_INPUTS="+string(inputsJson),
	)
	if err != nil {
		return def, err
	}

	freeformDef := struct {
		domain.WorkflowDefinition
		Actions map[string]*struct {
			domain.WorkflowAction
			Job *interface{} `json:"job"`
		} `json:"actions"`
	}{}

	err = json.Unmarshal(output, &freeformDef)
	if err != nil {
		return def, errors.WithMessagef(err, "While unmarshaling evaluator output %s into freeform definition", string(output))
	}

	def.Name = freeformDef.Name
	def.Meta = freeformDef.Meta
	def.Actions = map[string]*domain.WorkflowAction{}
	for actionName, action := range freeformDef.Actions {
		def.Actions[actionName] = &domain.WorkflowAction{
			Failure: action.Failure,
			Success: action.Success,
			Inputs:  action.Inputs,
			When:    action.When,
		}

		if action.Job == nil {
			continue
		}

		if job, err := json.Marshal(*action.Job); err != nil {
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
				def.Actions[actionName].Job = job
			}
		}
	}

	e.addEnv(&def)

	return def, nil
}

func (e *evaluationService) addEnv(def *domain.WorkflowDefinition) {
	for _, action := range def.Actions {
		if action.Job == nil {
			continue
		}
		for _, group := range action.Job.TaskGroups {
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
}

func (e *evaluationService) ListWorkflows(src string) ([]string, error) {
	output, err := e.evaluate(src, "list")
	if err != nil {
		return nil, err
	}

	var names []string
	if err := json.Unmarshal(output, &names); err != nil {
		return nil, errors.WithMessagef(err, "While unmarshaling workflow names %s", string(output))
	}

	return names, nil
}
