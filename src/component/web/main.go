package web

import (
	"context"
	"encoding/json"
	"log"
	"net/http"
	"time"

	swagger "github.com/davidebianchi/gswagger"
	"github.com/davidebianchi/gswagger/apirouter"
	"github.com/getkin/kin-openapi/openapi3"
	"github.com/google/uuid"
	"github.com/gorilla/mux"
	"github.com/input-output-hk/cicero/src/application"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/pkg/errors"
)

type Web struct {
	Listen              string
	Logger              *log.Logger
	WorkflowService     application.WorkflowService
	ActionService       application.ActionService
	MessageQueueService application.MessageQueueService
	NomadEventService   application.NomadEventService
	EvaluationService   application.EvaluationService
}

func (self *Web) Start(ctx context.Context) error {
	self.Logger.Println("Starting Web")

	muxRouter := mux.NewRouter().StrictSlash(true)
	r, err := swagger.NewRouter(apirouter.NewGorillaMuxRouter(muxRouter), swagger.Options{
		Context: ctx,
		Openapi: &openapi3.T{
			Info: &openapi3.Info{
				Title:   "Cicero REST API",
				Version: "1.0.0",
			},
		},
	})

	if err != nil {
		self.Logger.Printf("Failed to create swagger router: %s", err.Error())
	}

	// sorted alphabetically, please keep it this way
	r.AddRoute(http.MethodGet, "/api/action/{id}/logs", self.ApiActionIdLogsGet, genApiActionIdLogsGetSwagDef())
	r.AddRoute(http.MethodGet, "/api/action/{id}", self.ApiActionIdGet, genApiActionIdGetSwagDef())
	r.AddRoute(http.MethodGet, "/api/action", self.ApiActionGet, genApiActionGetSwagDef())
	r.AddRoute(http.MethodGet, "/api/workflow/definition/{source}/{name}", self.ApiWorkflowDefinitionSourceNameGet, genApiWorkflowDefinitionSourceNameGetSwagDef())
	r.AddRoute(http.MethodGet, "/api/workflow/definition/{source}", self.ApiWorkflowDefinitionSourceGet, genApiWorkflowDefinitionSourceGetSwagDef())
	r.AddRoute(http.MethodPost, "/api/workflow/instance/{id}/fact", self.ApiWorkflowInstanceIdFactPost, genApiWorkflowInstanceIdFactPostSwagDef())
	r.AddRoute(http.MethodGet, "/api/workflow/instance/{id}", self.ApiWorkflowInstanceIdGet, genApiWorkflowInstanceIdGetSwagDef())
	r.AddRoute(http.MethodGet, "/api/workflow/instance", self.ApiWorkflowInstanceGet, genApiWorkflowInstanceGetSwagDef())
	r.AddRoute(http.MethodPost, "/api/workflow/instance", self.ApiWorkflowInstancePost, genApiWorkflowInstancePostSwagDef())
	muxRouter.HandleFunc("/", self.IndexGet).Methods("GET")
	muxRouter.HandleFunc("/workflow/{id:[0-9]+}/graph", self.WorkflowIdGraphGet).Methods("GET")
	muxRouter.HandleFunc("/workflow/{id:[0-9]+}", self.WorkflowIdGet).Methods("GET")
	muxRouter.HandleFunc("/workflow/new", self.WorkflowNewGet).Methods("GET")
	muxRouter.HandleFunc("/workflow", self.WorkflowGet).Methods("GET")
	muxRouter.HandleFunc("/workflow", self.WorkflowPost).Methods("POST")
	muxRouter.PathPrefix("/static/").Handler(http.StripPrefix("/", http.FileServer(http.FS(staticFs))))

	// creates /documentation/json and /documentation/yaml routes
	err = r.GenerateAndExposeSwagger()
	if err != nil {
		self.Logger.Printf("Failed to generate and expose swagger: %s", err.Error())
	}

	server := &http.Server{Addr: self.Listen, Handler: muxRouter}

	go func() {
		if err := server.ListenAndServe(); err != nil {
			self.Logger.Printf("Failed to start web server: %s", err.Error())
		}
	}()

	<-ctx.Done()

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	if err := server.Shutdown(ctx); err != nil {
		self.Logger.Printf("Failed to stop web server: %s", err.Error())
	}

	return nil
}

func (self *Web) IndexGet(w http.ResponseWriter, req *http.Request) {
	http.Redirect(w, req, "/workflow", 302)
}

func (self *Web) WorkflowGet(w http.ResponseWriter, req *http.Request) {
	name := req.URL.Query().Get("name")

	if name == "" {
		if summary, err := self.WorkflowService.GetSummary(); err != nil {
			self.ServerError(w, errors.WithMessage(err, "Couldn't get summary of workflows"))
		} else {
			render("workflow/index.html", w, summary)
		}
	} else {
		if instances, err := self.WorkflowService.GetAllByName(name); err != nil {
			self.ServerError(w, errors.WithMessagef(err, "Couldn't get workflows by name: %q", name))
		} else {
			render("workflow/index-name.html", w, map[string]interface{}{
				"Name":      name,
				"Instances": instances,
			})
		}
	}
}

func (self *Web) WorkflowNewGet(w http.ResponseWriter, req *http.Request) {
	const templateName = "workflow/new.html"

	query := req.URL.Query()
	source := query.Get("source")
	name := query.Get("name")
	inputsJson := query.Get("inputs")

	// step 1
	if source == "" {
		if render(templateName, w, nil) != nil {
			return
		}
	}

	// step 4
	if len(inputsJson) > 0 {
		facts, err := self.parseFacts([]byte(inputsJson))
		if err != nil {
			self.ClientError(w, err)
			return
		}

		if err := self.WorkflowService.Start(source, name, facts); err != nil {
			self.ServerError(w, errors.WithMessage(err, "While starting workflow"))
			return
		}

		http.Redirect(w, req, "/workflow", 302)
		return
	}

	// step 3
	if len(name) > 0 {
		render(templateName, w, map[string]interface{}{"Source": source, "Name": name})
		return
	}

	// step 2
	if names, err := self.EvaluationService.ListWorkflows(source); err != nil {
		self.ServerError(w, errors.WithMessagef(err, "While listing workflows for %q", source))
	} else {
		render(templateName, w, map[string]interface{}{"Source": source, "Names": names})
	}
}

func (self *Web) WorkflowPost(w http.ResponseWriter, req *http.Request) {
	name := req.PostFormValue("name")
	source := req.PostFormValue("source")

	if err := self.WorkflowService.Start(source, name, domain.Facts{}); err != nil {
		self.ServerError(w, errors.WithMessagef(err, `Could not start workflow %q from source %q`, name, source))
	}

	http.Redirect(w, req, "/workflow", 302)
}

func (self *Web) WorkflowIdGet(w http.ResponseWriter, req *http.Request) {
	id, err := self.parseId(mux.Vars(req)["id"])
	if err != nil {
		self.ClientError(w, err)
		return
	}

	instance, err := self.WorkflowService.GetById(id)
	if err != nil {
		self.NotFound(w, errors.WithMessagef(err, "Failed to find workflow %q", id))
		return
	}

	allocs, err := self.NomadEventService.GetEventAllocByWorkflowId(id)
	if err != nil {
		self.NotFound(w, errors.WithMessagef(err, "Failed to find allocs for workflow id: %q", id))
		return
	}

	render("workflow/[id].html", w, map[string]interface{}{
		"Instance":   instance,
		"graph":      req.URL.Query().Get("graph"),
		"graphTypes": WorkflowGraphTypeStrings(),
		"allocs":     allocs,
	})
}

func (self *Web) WorkflowIdGraphGet(w http.ResponseWriter, req *http.Request) {
	id, err := self.parseId(mux.Vars(req)["id"])
	if err != nil {
		self.ClientError(w, err)
		return
	}

	instance, err := self.WorkflowService.GetById(id)
	if err != nil {
		self.NotFound(w, errors.WithMessagef(err, "Failed to find workflow %q", id))
		return
	}

	def, err := self.EvaluationService.EvaluateWorkflow(instance.Source, instance.Name, instance.ID, instance.Facts)
	if err != nil {
		self.ServerError(w, errors.WithMessagef(err, "Failed to evaluate workflow %q", id))
		return
	}

	var graphType WorkflowGraphType
	graphTypeStr := req.URL.Query().Get("type")
	if len(graphTypeStr) > 0 {
		if gt, err := WorkflowGraphTypeFromString(graphTypeStr); err != nil {
			self.ServerError(w, errors.WithMessage(err, "Failed to find graph type"))
			return
		} else {
			graphType = gt
		}
	}

	switch graphType {
	case WorkflowGraphTypeFlow:
		if err := RenderWorkflowGraphFlow(def, w); err != nil {
			self.ServerError(w, errors.WithMessage(err, "Failed to render flow graph"))
		}
	case WorkflowGraphTypeInputs:
		if err := RenderWorkflowGraphInputs(def, &instance, w); err != nil {
			self.ServerError(w, errors.WithMessage(err, "Failed to render input graph"))
		}
	default:
		self.ClientError(w, errors.WithMessagef(err, "Unknown graph type: %q", graphTypeStr))
	}
}

func (self *Web) ApiWorkflowDefinitionSourceGet(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	source := vars["source"]

	if wfs, err := self.EvaluationService.ListWorkflows(source); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to list workflows"))
	} else {
		self.json(w, wfs, 200)
	}
}

func genApiWorkflowDefinitionSourceGetSwagDef() swagger.Definitions {
	return swagger.Definitions{
		PathParams: swagger.ParameterValue{
			"source": {
				Content:     swagger.Content{},
				Description: "source of workflow definition",
			},
		},
		Responses: map[int]swagger.ContentValue{
			200: {
				Content: swagger.Content{
					"text/html": {Value: []string{}},
				},
				Description: "OK",
			},
			500: {
				Content: swagger.Content{
					"application/json": {Value: &errorResponse{}},
				},
				Description: "ServerError",
			},
		},
	}
}

func (self *Web) ApiWorkflowDefinitionSourceNameGet(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	id, err := self.parseId(vars["id"])
	if err != nil {
		self.ClientError(w, err)
		return
	}

	facts, err := self.parseFacts([]byte(vars["inputs"]))
	if err != nil {
		self.ClientError(w, err)
		return
	}

	query := req.URL.Query()
	source := query.Get("source")
	name := query.Get("name")

	if wf, err := self.EvaluationService.EvaluateWorkflow(source, name, id, facts); err != nil {
		self.ServerError(w, err)
	} else {
		self.json(w, wf, 200)
	}
}

func genApiWorkflowDefinitionSourceNameGetSwagDef() swagger.Definitions {
	return swagger.Definitions{
		PathParams: swagger.ParameterValue{
			"source": {
				Content:     swagger.Content{},
				Description: "source of workflow definition",
			},
			"name": {
				Content:     swagger.Content{},
				Description: "name of workflow definition",
			},
		},
		Responses: map[int]swagger.ContentValue{
			200: {
				Content: swagger.Content{
					"text/html": {Value: domain.WorkflowDefinition{}},
				},
				Description: "OK",
			},
			412: {
				Content: swagger.Content{
					"application/json": {Value: &errorResponse{}},
				},
				Description: "ClientError",
			},
			500: {
				Content: swagger.Content{
					"application/json": {Value: &errorResponse{}},
				},
				Description: "ServerError",
			},
		},
	}
}

func (self *Web) ApiWorkflowInstanceGet(w http.ResponseWriter, req *http.Request) {
	if instances, err := self.WorkflowService.GetAll(); err != nil {
		self.ServerError(w, errors.WithMessage(err, "failed to fetch workflows"))
	} else {
		self.json(w, instances, 200)
	}
}

func genApiWorkflowInstanceGetSwagDef() swagger.Definitions {
	return swagger.Definitions{
		Responses: map[int]swagger.ContentValue{
			200: {
				Content: swagger.Content{
					"text/html": {Value: []domain.WorkflowInstance{}},
				},
				Description: "OK",
			},
			500: {
				Content: swagger.Content{
					"application/json": {Value: &errorResponse{}},
				},
				Description: "ServerError",
			},
		},
	}
}

var workflowParam struct {
	Source string
	Name   *string
	Inputs domain.Facts
}

func (self *Web) ApiWorkflowInstancePost(w http.ResponseWriter, req *http.Request) {

	workflowParam.Inputs = domain.Facts{}
	if err := json.NewDecoder(req.Body).Decode(&workflowParam); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Could not unmarshal params from request body"))
		return
	}

	if workflowParam.Name != nil {
		if err := self.WorkflowService.Start(workflowParam.Source, *workflowParam.Name, workflowParam.Inputs); err != nil {
			self.ServerError(w, errors.WithMessage(err, "Failed to start workflow"))
			return
		}
	}

	if wfNames, err := self.EvaluationService.ListWorkflows(workflowParam.Source); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to list workflows"))
		return
	} else {
		for _, name := range wfNames {
			if err := self.WorkflowService.Start(workflowParam.Source, name, workflowParam.Inputs); err != nil {
				self.ServerError(w, errors.WithMessage(err, "Failed to start workflow"))
				return
			}
		}
	}

	w.WriteHeader(http.StatusNoContent)
}

func genApiWorkflowInstancePostSwagDef() swagger.Definitions {
	return swagger.Definitions{
		RequestBody: &swagger.ContentValue{
			Content: swagger.Content{
				"application/json": {Value: workflowParam},
			},
		},
		Responses: map[int]swagger.ContentValue{
			204: {
				Description: "NoContent",
			},
			412: {
				Content: swagger.Content{
					"application/json": {Value: &errorResponse{}},
				},
				Description: "ClientError",
			},
			500: {
				Content: swagger.Content{
					"application/json": {Value: &errorResponse{}},
				},
				Description: "ServerError",
			},
		},
	}
}

func (self *Web) WorkflowInstance(req *http.Request) (*domain.WorkflowInstance, error) {
	vars := mux.Vars(req)
	id, err := self.parseId(vars["id"])
	if err != nil {
		return nil, err
	}

	if instance, err := self.WorkflowService.GetById(id); err != nil {
		return nil, err
	} else {
		return &instance, nil
	}
}

func (self *Web) ApiWorkflowInstanceIdGet(w http.ResponseWriter, req *http.Request) {
	instance, err := self.WorkflowInstance(req)
	if err != nil {
		self.NotFound(w, errors.WithMessage(err, "Couldn't find instance"))
	}
	self.json(w, instance, 200)
}

func genApiWorkflowInstanceIdGetSwagDef() swagger.Definitions {
	return swagger.Definitions{
		PathParams: swagger.ParameterValue{
			"id": {
				Content:     swagger.Content{},
				Description: "id of an workflow instance",
			},
		},
		Responses: map[int]swagger.ContentValue{
			200: {
				Content: swagger.Content{
					"text/html": {Value: domain.WorkflowInstance{}},
				},
				Description: "OK",
			},
			404: {
				Content: swagger.Content{
					"application/json": {Value: &errorResponse{}},
				},
				Description: "NotFound",
			},
		},
	}
}

func (self *Web) ApiWorkflowInstanceIdFactPost(w http.ResponseWriter, req *http.Request) {
	instance, err := self.WorkflowInstance(req)
	if err != nil {
		self.ServerError(w, err)
	}

	facts := domain.Facts{}
	if err := json.NewDecoder(req.Body).Decode(&facts); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Could not unmarshal facts from request body"))
	}

	if err := self.MessageQueueService.Publish(
		domain.FactStreamName.Fmt(instance.Name, instance.ID),
		domain.FactStreamName,
		facts,
	); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Could not publish fact"))
		return
	}

	w.WriteHeader(http.StatusNoContent)
}

func genApiWorkflowInstanceIdFactPostSwagDef() swagger.Definitions {
	return swagger.Definitions{
		PathParams: swagger.ParameterValue{
			"id": {
				Content:     swagger.Content{},
				Description: "id of an workflow instance",
			},
		},
		RequestBody: &swagger.ContentValue{
			Content: swagger.Content{
				"application/json": {Value: domain.Facts{}},
			},
		},
		Responses: map[int]swagger.ContentValue{
			204: {
				Description: "NoContent",
			},
			412: {
				Content: swagger.Content{
					"application/json": {Value: &errorResponse{}},
				},
				Description: "ClientError",
			},
			500: {
				Content: swagger.Content{
					"application/json": {Value: &errorResponse{}},
				},
				Description: "ServerError",
			},
		},
	}
}

func (self *Web) ApiActionGet(w http.ResponseWriter, req *http.Request) {
	if actions, err := self.ActionService.GetAll(); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to get all actions"))
	} else {
		self.json(w, actions, 200)
	}
}

func genApiActionGetSwagDef() swagger.Definitions {
	return swagger.Definitions{
		Responses: map[int]swagger.ContentValue{
			200: {
				Content: swagger.Content{
					"text/html": {Value: []domain.ActionInstance{}},
				},
				Description: "OK",
			},
			500: {
				Content: swagger.Content{
					"application/json": {Value: &errorResponse{}},
				},
				Description: "ServerError",
			},
		},
	}
}

func (self *Web) ApiActionIdGet(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	if id, err := uuid.Parse(vars["id"]); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Failed to parse id"))
	} else if action, err := self.ActionService.GetById(id); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to get action"))
	} else {
		self.json(w, action, 200)
	}
}

func genApiActionIdGetSwagDef() swagger.Definitions {
	return swagger.Definitions{
		PathParams: swagger.ParameterValue{
			"id": {
				Content:     swagger.Content{},
				Description: "id of an action",
			},
		},
		Responses: map[int]swagger.ContentValue{
			200: {
				Content: swagger.Content{
					"text/html": {Value: domain.ActionInstance{}},
				},
				Description: "OK",
			},
			412: {
				Content: swagger.Content{
					"application/json": {Value: &errorResponse{}},
				},
				Description: "ClientError",
			},
			500: {
				Content: swagger.Content{
					"application/json": {Value: &errorResponse{}},
				},
				Description: "ServerError",
			},
		},
	}
}

func (self *Web) ApiActionIdLogsGet(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	if id, err := uuid.Parse(vars["id"]); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Failed to parse id"))
	} else if logs, err := self.ActionService.JobLogs(id); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to get logs"))
	} else {
		self.json(w, map[string]*application.LokiOutput{"logs": logs}, 200)
	}
}

func genApiActionIdLogsGetSwagDef() swagger.Definitions {
	return swagger.Definitions{
		PathParams: swagger.ParameterValue{
			"id": {
				Content:     swagger.Content{},
				Description: "id of an action",
			},
		},
		Responses: map[int]swagger.ContentValue{
			200: {
				Content: swagger.Content{
					"text/html": {Value: map[string]*application.LokiOutput{"logs": {}}},
				},
				Description: "OK",
			},
			412: {
				Content: swagger.Content{
					"application/json": {Value: &errorResponse{}},
				},
				Description: "ClientError",
			},
			500: {
				Content: swagger.Content{
					"application/json": {Value: &errorResponse{}},
				},
				Description: "ServerError",
			},
		},
	}
}

type errorResponse struct {
	Message string `json:"message"`
}
