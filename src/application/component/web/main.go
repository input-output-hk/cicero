package web

import (
	"context"
	"encoding/json"
	"net/http"
	"net/url"
	"time"

	swagger "github.com/davidebianchi/gswagger"
	"github.com/davidebianchi/gswagger/apirouter"
	"github.com/getkin/kin-openapi/openapi3"
	"github.com/google/uuid"
	"github.com/gorilla/mux"
	"github.com/jackc/pgx/v4"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"

	"github.com/input-output-hk/cicero/src/application/service"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
)

type Web struct {
	Listen              string
	Logger              zerolog.Logger
	RunService          service.RunService
	ActionService       service.ActionService
	FactService         service.FactService
	MessageQueueService service.MessageQueueService
	NomadEventService   service.NomadEventService
	EvaluationService   service.EvaluationService
	Db                  config.PgxIface
}

func (self *Web) Start(ctx context.Context) error {
	self.Logger.Info().Msg("Starting")

	muxRouter := mux.NewRouter().StrictSlash(true).UseEncodedPath()
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
		return errors.WithMessage(err, "Failed to create swagger router")
	}

	// sorted alphabetically, please keep it this way
	if _, err := r.AddRoute(http.MethodGet, "/api/action/current/{name}/definition", self.ApiActionCurrentNameDefinitionGet, genApiActionCurrentNameDefinitionGetSwagDef()); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet, "/api/action/current/{name}", self.ApiActionCurrentNameGet, genApiActionCurrentNameGetSwagDef()); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet, "/api/action/current", self.ApiActionCurrentGet, genApiActionCurrentGetSwagDef()); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet, "/api/action/definition/{source}/{name}/{id}", self.ApiActionDefinitionSourceNameIdGet, genApiActionDefinitionSourceNameIdGetSwagDef()); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet, "/api/action/definition/{source}", self.ApiActionDefinitionSourceGet, genApiActionDefinitionSourceGetSwagDef()); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet, "/api/action/{id}/definition", self.ApiActionIdDefinitionGet, genApiActionIdDefinitionGetSwagDef()); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet, "/api/action/{id}", self.ApiActionIdGet, genApiActionIdGetSwagDef()); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet, "/api/action", self.ApiActionGet, genApiActionGetSwagDef()); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodPost, "/api/action", self.ApiActionPost, genApiActionPostSwagDef()); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodPost, "/api/run/{id}/fact", self.ApiRunIdFactPost, genApiRunIdFactPostSwagDef()); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet, "/api/run/{id}/logs", self.ApiRunIdLogsGet, genApiRunIdLogsGetSwagDef()); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet, "/api/run/{id}", self.ApiRunIdGet, genApiRunIdGetSwagDef()); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet, "/api/run", self.ApiRunGet, genApiRunGetSwagDef()); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet, "/api/fact/{id}", self.ApiFactIdGet, genApiFactIdGetSwagDef()); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodPost, "/api/fact", self.ApiFactPost, genApiFactPostSwagDef()); err != nil {
		return err
	}
	muxRouter.HandleFunc("/", self.IndexGet).Methods("GET")
	muxRouter.HandleFunc("/run/{id}", self.RunIdGet).Methods("GET")
	muxRouter.HandleFunc("/run", self.RunGet).Methods("GET")
	muxRouter.HandleFunc("/action/current", self.ActionCurrentGet).Methods("GET")
	muxRouter.HandleFunc("/action/new", self.ActionNewGet).Methods("GET")
	muxRouter.HandleFunc("/action/{id}", self.ActionIdGet).Methods("GET")
	muxRouter.PathPrefix("/static/").Handler(http.StripPrefix("/", http.FileServer(http.FS(staticFs))))

	// creates /documentation/json and /documentation/yaml routes
	err = r.GenerateAndExposeSwagger()
	if err != nil {
		return errors.WithMessage(err, "Failed to generate and expose swagger: %s")
	}

	server := &http.Server{Addr: self.Listen, Handler: muxRouter}

	go func() {
		if err := server.ListenAndServe(); err != nil {
			self.Logger.Err(err).Msg("Failed to start web server")
		}
	}()

	<-ctx.Done()

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	if err := server.Shutdown(ctx); err != nil {
		self.Logger.Err(err).Msg("Failed to stop web server")
	}

	return nil
}

func (self *Web) IndexGet(w http.ResponseWriter, req *http.Request) {
	http.Redirect(w, req, "/action/current", 302)
}

func (self *Web) ActionCurrentGet(w http.ResponseWriter, req *http.Request) {
	if summary, err := self.ActionService.GetCurrent(); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Could not get current Actions"))
		return
	} else if err := render("action/current.html", w, summary); err != nil {
		self.ServerError(w, err)
		return
	}
}

func (self *Web) ActionIdGet(w http.ResponseWriter, req *http.Request) {
	if id, err := uuid.Parse(mux.Vars(req)["id"]); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Could not parse Action ID"))
	} else if action, err := self.ActionService.GetById(id); err != nil {
		self.ServerError(w, errors.WithMessagef(err, "Could not get Action by ID: %q", id))
		return
	} else if runs, err := self.RunService.GetByActionId(id); err != nil {
		self.ServerError(w, errors.WithMessagef(err, "Could not get Runs by Action ID: %q", id))
		return
	} else if err := render("action/[id].html", w, map[string]interface{}{
		"Action": action,
		"Runs":   runs,
	}); err != nil {
		self.ServerError(w, err)
		return
	}
}

func (self *Web) ActionNewGet(w http.ResponseWriter, req *http.Request) {
	const templateName = "action/new.html"

	query := req.URL.Query()
	source := query.Get("source")
	name := query.Get("name")

	// step 1
	if source == "" {
		if err := render(templateName, w, nil); err != nil {
			self.ServerError(w, err)
		}
		return
	}

	// step 2
	if name == "" {
		if names, err := self.EvaluationService.ListActions(source); err != nil {
			self.ServerError(w, errors.WithMessagef(err, "While listing Actions in %q", source))
		} else if err := render(templateName, w, map[string]interface{}{"Source": source, "Names": names}); err != nil {
			self.ServerError(w, err)
		}
		return
	}

	if err := self.MessageQueueService.Publish(
		domain.ActionCreateStream(name),
		domain.ActionCreateStreamName,
		[]byte{},
		liftbridge.Header("source", []byte(source)),
	); err != nil {
		self.ServerError(w, err)
		return
	} else {
		http.Redirect(w, req, "/action/current", 302)
		return
	}
}

func (self *Web) RunIdGet(w http.ResponseWriter, req *http.Request) {
	id, err := uuid.Parse(mux.Vars(req)["id"])
	if err != nil {
		self.ClientError(w, err)
		return
	}

	run, err := self.RunService.GetByNomadJobId(id)
	if err != nil {
		self.NotFound(w, errors.WithMessagef(err, "Failed to find Run %q", id))
		return
	}

	allocs, err := self.NomadEventService.GetEventAllocByNomadJobId(id)
	if err != nil {
		self.NotFound(w, errors.WithMessagef(err, "Failed to find allocs for Nomad job %q", id))
		return
	}

	if err := render("run/[id].html", w, map[string]interface{}{
		"Run":    run,
		"allocs": allocs,
	}); err != nil {
		self.ServerError(w, err)
		return
	}
}

func (self *Web) RunGet(w http.ResponseWriter, req *http.Request) {
	if runs, err := self.RunService.GetAll(); err != nil {
		self.ServerError(w, err)
	} else if err := render("run/index.html", w, runs); err != nil {
		self.ServerError(w, err)
	}
}

func (self *Web) ApiActionDefinitionSourceGet(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	source, err := url.PathUnescape(vars["source"])
	if err != nil {
		self.ClientError(w, errors.WithMessage(err, "Invalid escaping of action definition source"))
		return
	}

	if wfs, err := self.EvaluationService.ListActions(source); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to list actions"))
		return
	} else {
		self.json(w, wfs, 200)
	}
}

func genApiActionDefinitionSourceGetSwagDef() swagger.Definitions {
	return swagger.Definitions{
		PathParams: swagger.ParameterValue{
			"source": {
				Content:     swagger.Content{},
				Description: "source of one or more action definitions",
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

func (self *Web) ApiActionDefinitionSourceNameIdGet(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	if source, err := url.PathUnescape(vars["source"]); err != nil {
		self.ClientError(w, errors.WithMessagef(err, "Invalid escaping of action definition source: %q", vars["source"]))
	} else if name, err := url.PathUnescape(vars["name"]); err != nil {
		self.ClientError(w, errors.WithMessagef(err, "Invalid escaping of action name: %q", vars["name"]))
	} else if idStr, err := url.PathUnescape(vars["id"]); err != nil {
		self.ClientError(w, errors.WithMessagef(err, "Invalid escaping of action ID: %q", vars["id"]))
	} else if id, err := uuid.Parse(idStr); err != nil {
		self.ClientError(w, errors.WithMessagef(err, "Invalid UUID given as action ID: %q", idStr))
	} else if def, err := self.EvaluationService.EvaluateAction(source, name, id); err != nil {
		self.ServerError(w, err)
	} else {
		self.json(w, def, 200)
	}
}

func genApiActionDefinitionSourceNameIdGetSwagDef() swagger.Definitions {
	return swagger.Definitions{
		PathParams: swagger.ParameterValue{
			"source": {
				Content:     swagger.Content{},
				Description: "source of one or more action definitions",
			},
			"name": {
				Content:     swagger.Content{},
				Description: "name of an action in the source",
			},
			"id": {
				Content:     swagger.Content{},
				Description: "id to evaluate the action's source with",
			},
		},
		Responses: map[int]swagger.ContentValue{
			200: {
				Content: swagger.Content{
					"text/html": {Value: domain.ActionDefinition{}},
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

func (self *Web) ApiRunGet(w http.ResponseWriter, req *http.Request) {
	if runs, err := self.RunService.GetAll(); err != nil {
		self.ServerError(w, errors.WithMessage(err, "failed to fetch actions"))
	} else {
		self.json(w, runs, 200)
	}
}

func genApiRunGetSwagDef() swagger.Definitions {
	return swagger.Definitions{
		Responses: map[int]swagger.ContentValue{
			200: {
				Content: swagger.Content{
					"text/html": {Value: []domain.Run{}},
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

type apiActionPostBody struct {
	Source string  `json:"source"`
	Name   *string `json:"name"`
}

func (self *Web) ApiActionPost(w http.ResponseWriter, req *http.Request) {
	params := apiActionPostBody{}
	if err := json.NewDecoder(req.Body).Decode(&params); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Could not unmarshal params from request body"))
		return
	}

	if params.Name != nil {
		if err := self.MessageQueueService.Publish(
			domain.ActionCreateStream(*params.Name),
			domain.ActionCreateStreamName,
			[]byte{},
			liftbridge.Header("source", []byte(params.Source)),
		); err != nil {
			self.ServerError(w, err)
			return
		}
	} else {
		if actionNames, err := self.EvaluationService.ListActions(params.Source); err != nil {
			self.ServerError(w, errors.WithMessage(err, "Failed to list actions"))
			return
		} else {
			for _, actionName := range actionNames {
				if err := self.MessageQueueService.Publish(
					domain.ActionCreateStream(actionName),
					domain.ActionCreateStreamName,
					[]byte{},
					liftbridge.Header("source", []byte(params.Source)),
				); err != nil {
					self.ServerError(w, err)
					return
				}
			}
		}
	}

	w.WriteHeader(http.StatusNoContent)
}

func genApiActionPostSwagDef() swagger.Definitions {
	return swagger.Definitions{
		RequestBody: &swagger.ContentValue{
			Content: swagger.Content{
				"application/json": {Value: apiActionPostBody{}},
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

func (self *Web) getRun(req *http.Request) (*domain.Run, error) {
	vars := mux.Vars(req)
	id, err := uuid.Parse(vars["id"])
	if err != nil {
		return nil, err
	}

	if run, err := self.RunService.GetByNomadJobId(id); err != nil {
		return nil, err
	} else {
		return &run, nil
	}
}

func (self *Web) ApiRunIdGet(w http.ResponseWriter, req *http.Request) {
	run, err := self.getRun(req)
	if err != nil {
		self.NotFound(w, errors.WithMessage(err, "Could not find run"))
	}
	self.json(w, run, 200)
}

func genApiRunIdGetSwagDef() swagger.Definitions {
	return swagger.Definitions{
		PathParams: swagger.ParameterValue{
			"id": {
				Content:     swagger.Content{},
				Description: "id of an action run",
			},
		},
		Responses: map[int]swagger.ContentValue{
			200: {
				Content: swagger.Content{
					"text/html": {Value: domain.Run{}},
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

func (self *Web) ApiRunIdFactPost(w http.ResponseWriter, req *http.Request) {
	run, err := self.getRun(req)
	if err != nil {
		self.ServerError(w, err)
		return
	}

	var value interface{}
	if err := json.NewDecoder(req.Body).Decode(&value); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Could not unmarshal fact value from request body"))
		return
	}

	factJson, err := json.Marshal(domain.Fact{
		RunId: &run.NomadJobID,
		Value: value,
	})
	if err != nil {
		self.ServerError(w, err)
		return
	}

	if err := self.MessageQueueService.Publish(
		domain.FactCreateStreamName.String(),
		domain.FactCreateStreamName,
		factJson,
	); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Could not publish fact"))
		return
	}

	w.WriteHeader(http.StatusNoContent)
}

func genApiRunIdFactPostSwagDef() swagger.Definitions {
	var value interface{}
	return swagger.Definitions{
		PathParams: swagger.ParameterValue{
			"id": {
				Content:     swagger.Content{},
				Description: "id of an action",
			},
		},
		RequestBody: &swagger.ContentValue{
			Content: swagger.Content{
				"application/json": {Value: value},
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
					"text/html": {Value: []domain.Action{}},
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

// XXX respond with map[string]Action instead of []Action?
func (self *Web) ApiActionCurrentGet(w http.ResponseWriter, req *http.Request) {
	if actions, err := self.ActionService.GetCurrent(); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to get current actions"))
	} else {
		self.json(w, actions, 200)
	}
}

func genApiActionCurrentGetSwagDef() swagger.Definitions {
	return genApiActionGetSwagDef()
}

func (self *Web) ApiActionCurrentNameGet(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	if name, err := url.PathUnescape(vars["name"]); err != nil {
		self.ClientError(w, errors.WithMessagef(err, "Invalid escaping of action name: %q", vars["name"]))
	} else if actions, err := self.ActionService.GetLatestByName(name); err != nil {
		self.ServerError(w, errors.WithMessagef(err, "Failed to get current action named %q", name))
	} else {
		self.json(w, actions, 200)
	}
}

func genApiActionCurrentNameGetSwagDef() swagger.Definitions {
	return swagger.Definitions{
		PathParams: swagger.ParameterValue{
			"name": {
				Content:     swagger.Content{},
				Description: "name of an action",
			},
		},
		RequestBody: &swagger.ContentValue{
			Content: swagger.Content{
				"application/json": {Value: &domain.Action{}},
			},
		},
		Responses: map[int]swagger.ContentValue{
			204: {
				Description: "NoContent",
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

func (self *Web) ApiActionCurrentNameDefinitionGet(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	if name, err := url.PathUnescape(vars["name"]); err != nil {
		self.ClientError(w, errors.WithMessagef(err, "Invalid escaping of action name: %q", vars["name"]))
	} else if action, err := self.ActionService.GetLatestByName(name); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to get action"))
	} else if actionDef, err := self.EvaluationService.EvaluateAction(action.Source, action.Name, action.ID); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to evaluate action"))
	} else {
		self.json(w, actionDef, 200)
	}
}

func genApiActionCurrentNameDefinitionGetSwagDef() swagger.Definitions {
	return genApiActionIdDefinitionGetSwagDef()
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
					"text/html": {Value: domain.Action{}},
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

func (self *Web) ApiActionIdDefinitionGet(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	if id, err := uuid.Parse(vars["id"]); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Failed to parse id"))
	} else if action, err := self.ActionService.GetById(id); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to get action"))
	} else if actionDef, err := self.EvaluationService.EvaluateAction(action.Source, action.Name, action.ID); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to evaluate action"))
	} else {
		self.json(w, actionDef, 200)
	}
}

func genApiActionIdDefinitionGetSwagDef() swagger.Definitions {
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
					"text/html": {Value: domain.ActionDefinition{}},
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

func (self *Web) ApiRunIdLogsGet(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	if id, err := uuid.Parse(vars["id"]); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Failed to parse id"))
	} else if logs, err := self.RunService.JobLogs(id); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to get logs"))
	} else {
		self.json(w, map[string]*domain.LokiOutput{"logs": logs}, 200)
	}
}

func genApiRunIdLogsGetSwagDef() swagger.Definitions {
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
					"text/html": {Value: map[string]*domain.LokiOutput{"logs": {}}},
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

func (self *Web) ApiFactIdGet(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	if id, err := uuid.Parse(vars["id"]); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Failed to parse id"))
	} else if fact, err := self.FactService.GetById(id); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to get Fact"))
	} else {
		self.json(w, fact, 200)
	}
}

func genApiFactIdGetSwagDef() swagger.Definitions {
	return swagger.Definitions{
		PathParams: swagger.ParameterValue{
			"id": {
				Content:     swagger.Content{},
				Description: "id of a fact",
			},
		},
		Responses: map[int]swagger.ContentValue{
			200: {
				Content: swagger.Content{
					"text/html": {Value: domain.Fact{}},
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

func (self *Web) ApiFactPost(w http.ResponseWriter, req *http.Request) {
	fact := domain.Fact{}
	if err := json.NewDecoder(req.Body).Decode(&fact.Value); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Could not unmarshal json body"))
		return
	}

	if err := self.Db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		if err := self.FactService.Save(tx, &fact); err != nil {
			return errors.WithMessage(err, "Failed to save Fact")
		}

		factJson, err := json.Marshal(&fact)
		if err != nil {
			return errors.WithMessage(err, "Could not marshal Fact")
		}

		if err := self.MessageQueueService.Publish(
			domain.FactCreateStreamName.String(),
			domain.FactCreateStreamName,
			factJson,
		); err != nil {
			return err
		}

		self.json(w, fact, 200)
		return nil
	}); err != nil {
		self.ServerError(w, err)
		return
	}
}

func genApiFactPostSwagDef() swagger.Definitions {
	return swagger.Definitions{
		Responses: map[int]swagger.ContentValue{
			200: {
				Content: swagger.Content{
					"text/html": {Value: domain.Fact{}},
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
