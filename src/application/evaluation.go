package application

import (
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

	getter "github.com/hashicorp/go-getter/v2"
	"github.com/hashicorp/nomad/jobspec2"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/pkg/errors"
	"github.com/adrg/xdg"
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

	if evaluator == "" {
		err = errors.New("No evaluator given in workflow source: " + src)
		return
	}

	return
}

type evaluationService struct {
	Env    []string // NAME=VALUE or just NAME to inherit from process environment
	logger *log.Logger
}

func NewEvaluationService(env []string) EvaluationService {
	return &evaluationService{
		Env:    env,
		logger: log.New(os.Stderr, "evaluationService: ", log.LstdFlags),
	}
}

func (e *evaluationService) evaluate(src string, command string, extraEnv ...string) ([]byte, error) {
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

	cmd := exec.Command("cicero-evaluator-"+evaluator, command)
	extraEnv = append(extraEnv, "CICERO_WORKFLOW_SRC="+dst)
	cmd.Env = append(os.Environ(), extraEnv...)

	e.logger.Printf("Running %s with env %v", strings.Join(cmd.Args, " "), extraEnv)

	if output, err := cmd.Output(); err != nil {
		return nil, errors.WithMessagef(err, "Failed to evaluate. Output: %s", output)
	} else {
		return output, err
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
		Actions map[string]struct {
			domain.WorkflowAction
			Job interface{} `json:"job"`
		} `json:"actions"`
	}{}

	err = json.Unmarshal(output, &freeformDef)
	if err != nil {
		e.logger.Println(string(output))
		return def, errors.WithMessage(err, "While unmarshaling evaluator output into freeform definition")
	}

	def.Actions = map[string]*domain.WorkflowAction{}
	for actionName, action := range freeformDef.Actions {
		if job, err := json.Marshal(action.Job); err != nil {
			return def, err
		} else if job, err := jobspec2.ParseWithConfig(&jobspec2.ParseConfig{
			Body:    []byte(`{"job":` + string(job) + "}"),
			AllowFS: false,
			Strict:  true,
		}); err != nil {
			return def, err
		} else {
			def.Name = freeformDef.Name
			def.Source = freeformDef.Source
			def.Meta = freeformDef.Meta
			def.Actions[actionName] = &domain.WorkflowAction{
				Failure: action.Failure,
				Success: action.Success,
				Inputs:  action.Inputs,
				When:    action.When,
				Job:     *job,
			}
		}
	}

	e.addEnv(&def)

	return def, nil
}

func (e *evaluationService) addEnv(def *domain.WorkflowDefinition) {
	for _, action := range def.Actions {
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
		e.logger.Println(string(output))
		return nil, errors.WithMessage(err, "While unmarshaling workflow names")
	}

	return names, nil
}
