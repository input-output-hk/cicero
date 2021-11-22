package service

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
	"os/exec"
	"strings"

	"github.com/hashicorp/nomad/jobspec2"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/pkg/errors"
)

type EvaluationService interface {
	EvaluateWorkflow(src string, name string, id uint64, inputs model.Facts) (model.WorkflowDefinition, error)
	ListWorkflows(src string) ([]string, error)
}

type evaluationService struct {
	Command string
	Env     []string // NAME=VALUE or just NAME to inherit from process environment
	logger  *log.Logger
}

func NewEvaluationService(command string, env []string) EvaluationService {
	return &evaluationService{
		Command: command,
		Env:     env,
		logger:  log.New(os.Stderr, "evaluationService: ", log.LstdFlags),
	}
}

func (e *evaluationService) EvaluateWorkflow(src, name string, id uint64, inputs model.Facts) (model.WorkflowDefinition, error) {
	var def model.WorkflowDefinition

	inputsJson, err := json.Marshal(inputs)
	if err != nil {
		return def, errors.WithMessagef(err, "Could not marshal workflow inputs to json: %s", inputs)
	}

	cmd := exec.Command(
		e.Command,
		"eval",
	)
	extraEnv := []string{
		"CICERO_WORKFLOW_SRC=" + src,
		"CICERO_WORKFLOW_NAME=" + name,
		"CICERO_WORKFLOW_INSTANCE_ID=" + fmt.Sprintf("%d", id),
		"CICERO_WORKFLOW_INPUTS=" + string(inputsJson),
	}
	cmd.Env = append(os.Environ(), extraEnv...)

	e.logger.Printf("running %s with env %v", strings.Join(cmd.Args, " "), extraEnv)
	output, err := cmd.Output()

	if err != nil {
		e.logger.Println(string(output))
		return def, errors.WithMessage(err, "Failed to evaluate workflow")
	}

	freeformDef := struct {
		model.WorkflowDefinition
		Actions map[string]struct {
			model.WorkflowAction
			Job interface{} `json:"job"`
		} `json:"actions"`
	}{}

	err = json.Unmarshal(output, &freeformDef)
	if err != nil {
		e.logger.Println(string(output))
		return def, errors.WithMessage(err, "While unmarshaling evaluator output into freeform definition")
	}

	def.Actions = map[string]*model.WorkflowAction{}
	for actionName, action := range freeformDef.Actions {
		if job, err := json.Marshal(action.Job); err != nil {
			return def, err
		} else if job, err := jobspec2.ParseWithConfig(&jobspec2.ParseConfig{
			Body:     []byte(`{"job":` + string(job) + "}"),
			AllowFS:  false,
			Strict:   true,
		}); err != nil {
			return def, err
		} else {
			def.Name = freeformDef.Name
			def.Source = freeformDef.Source
			def.Meta = freeformDef.Meta
			def.Actions[actionName] = &model.WorkflowAction{
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

func (e *evaluationService) addEnv(def *model.WorkflowDefinition) {
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
	var names []string

	cmd := exec.Command(
		e.Command,
		"list",
	)
	extraEnv := []string{"CICERO_WORKFLOW_SRC=" + src}
	cmd.Env = append(os.Environ(), extraEnv...)

	e.logger.Printf("running %s with env %v", strings.Join(cmd.Args, " "), extraEnv)
	output, err := cmd.Output()

	if err != nil {
		return nil, errors.WithMessage(err, "Failed to list workflows")
	}

	err = json.Unmarshal(output, &names)
	if err != nil {
		e.logger.Println(string(output))
		return nil, errors.WithMessage(err, "While unmarshaling workflow names")
	}

	return names, nil
}
