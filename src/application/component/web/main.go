package web

import (
	"context"
	"encoding/json"
	"io"
	"net/http"
	"net/url"
	"strconv"
	"time"

	"github.com/davidebianchi/gswagger/apirouter"
	"github.com/georgysavva/scany/pgxscan"
	"github.com/google/uuid"
	"github.com/gorilla/mux"
	"github.com/jackc/pgx/v4"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"

	"github.com/input-output-hk/cicero/src/application/component/web/apidoc"
	"github.com/input-output-hk/cicero/src/application/service"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
)

type Web struct {
	Listen            string
	Logger            zerolog.Logger
	RunService        service.RunService
	ActionService     service.ActionService
	FactService       service.FactService
	NomadEventService service.NomadEventService
	EvaluationService service.EvaluationService
	Db                config.PgxIface
}

func (self *Web) Start(ctx context.Context) error {
	self.Logger.Info().Msg("Starting")

	muxRouter := mux.NewRouter().StrictSlash(true).UseEncodedPath()
	r, err := apidoc.NewRouterDocumented(apirouter.NewGorillaMuxRouter(muxRouter), "Cicero REST API", "1.0.0", "cicero", ctx)
	if err != nil {
		return errors.WithMessage(err, "Failed to create swagger router")
	}

	// sorted alphabetically, please keep it this way
	if _, err := r.AddRoute(http.MethodGet,
		"/api/action/current/{name}/definition",
		self.ApiActionCurrentNameDefinitionGet,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "name", Description: "name of an action", Value: "actionName"}}),
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, domain.ActionDefinition{}, "OK")),
	); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet,
		"/api/action/current/{name}",
		self.ApiActionCurrentNameGet,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "name", Description: "name of an action", Value: "actionName"}}),
			apidoc.BuildBodyRequest(domain.Action{}),
			apidoc.BuildResponseSuccessfully(http.StatusNoContent, nil, "NoContent")),
	); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet,
		"/api/action/current",
		self.ApiActionCurrentGet,
		apidoc.BuildSwaggerDef(
			nil,
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, []domain.Action{}, "Ok")),
	); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet,
		"/api/action/definition/{source}/{name}/{id}",
		self.ApiActionDefinitionSourceNameIdGet,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{
				{Name: "source", Description: "source of one or more action definitions", Value: "source"},
				{Name: "name", Description: "name of an action in the source", Value: "name"},
				{Name: "id", Description: "id to evaluate the action's source with", Value: "UUID"},
			}),
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, domain.ActionDefinition{}, "Ok")),
	); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet,
		"/api/action/definition/{source}",
		self.ApiActionDefinitionSourceGet,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "source", Description: "source of one or more action definitions", Value: "source"}}),
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, []string{}, "Ok")),
	); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet,
		"/api/action/{id}/definition",
		self.ApiActionIdDefinitionGet,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "id", Description: "id to evaluate the action's source with", Value: "UUID"}}),
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, domain.ActionDefinition{}, "Ok")),
	); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet,
		"/api/action/{id}",
		self.ApiActionIdGet,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "id", Description: "id to evaluate the action's source with", Value: "UUID"}}),
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, domain.Action{}, "Ok")),
	); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet,
		"/api/action",
		self.ApiActionGet,
		apidoc.BuildSwaggerDef(
			nil,
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, []domain.Action{}, "Ok")),
	); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodPost,
		"/api/action",
		self.ApiActionPost,
		apidoc.BuildSwaggerDef(
			nil,
			apidoc.BuildBodyRequest(apiActionPostBody{}), //TODO: move to domain
			apidoc.BuildResponseSuccessfully(http.StatusNoContent, nil, "NoContent")),
	); err != nil {
		return err
	}
	var value interface{} //TODO: WIP
	if _, err := r.AddRoute(http.MethodPost,
		"/api/run/{id}/fact",
		self.ApiRunIdFactPost,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "id", Description: "id of an action", Value: "UUID"}}),
			apidoc.BuildBodyRequest(value),
			apidoc.BuildResponseSuccessfully(http.StatusNoContent, nil, "NoContent")),
	); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet,
		"/api/run/{id}/logs",
		self.ApiRunIdLogsGet,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "id", Description: "id of an action", Value: "UUID"}}),
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, map[string]*domain.LokiOutput{"logs": {}}, "OK")),
	); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet,
		"/api/run/{id}",
		self.ApiRunIdGet,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "id", Description: "id of an action", Value: "UUID"}}),
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, domain.Run{}, "OK")),
	); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet,
		"/api/run",
		self.ApiRunGet,
		apidoc.BuildSwaggerDef(
			nil,
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, []domain.Run{}, "OK")),
	); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet,
		"/api/fact/{id}/binary",
		self.ApiFactIdBinaryGet,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "id", Description: "id of a fact", Value: "UUID"}}),
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, []byte{}, "OK"),
		),
	); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet,
		"/api/fact/{id}",
		self.ApiFactIdGet,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "id", Description: "id of a fact", Value: "UUID"}}),
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, domain.Fact{}, "OK")),
	); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodPost,
		"/api/fact",
		self.ApiFactPost,
		apidoc.BuildSwaggerDef(
			nil,
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, domain.Fact{}, "OK")),
	); err != nil {
		return err
	}
	muxRouter.HandleFunc("/", self.IndexGet).Methods(http.MethodGet)
	muxRouter.HandleFunc("/run/{id}/cancel", self.RunIdCancelGet).Methods(http.MethodGet)
	muxRouter.HandleFunc("/run/{id}", self.RunIdGet).Methods(http.MethodGet)
	muxRouter.HandleFunc("/run", self.RunGet).Methods(http.MethodGet)
	muxRouter.HandleFunc("/action/current", self.ActionCurrentGet).Methods(http.MethodGet)
	muxRouter.HandleFunc("/action/new", self.ActionNewGet).Methods(http.MethodGet)
	muxRouter.HandleFunc("/action/{id}", self.ActionIdGet).Methods(http.MethodGet)
	muxRouter.HandleFunc("/action/{id}/run", self.ActionIdRunGet).Methods(http.MethodGet)
	muxRouter.PathPrefix("/static/").Handler(http.StripPrefix("/", http.FileServer(http.FS(staticFs))))

	// creates /documentation/cicero.json and /documentation/cicero.yaml routes
	err = r.GenerateAndExposeSwagger()
	if err != nil {
		return errors.WithMessage(err, "Failed to generate and expose swagger: %s")
	}

	server := &http.Server{Addr: self.Listen, Handler: muxRouter}

	go func() {
		if err := server.ListenAndServe(); err != nil {
			self.Logger.Err(err).Msgf("Failed to start web server on %s", self.Listen)
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
	http.Redirect(w, req, "/action/current", http.StatusFound)
}

func (self *Web) ActionCurrentGet(w http.ResponseWriter, req *http.Request) {
	var actions []*domain.Action
	if err := self.Db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		var err error
		actions, err = self.ActionService.GetCurrent(tx)
		return errors.WithMessage(err, "Could not get current Actions")
	}); err != nil {
		self.ServerError(w, err)
		return
	} else if err := render("action/current.html", w, actions); err != nil {
		self.ServerError(w, err)
		return
	}
}

func (self *Web) ActionIdRunGet(w http.ResponseWriter, req *http.Request) {
	if id, err := uuid.Parse(mux.Vars(req)["id"]); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Could not parse Action ID"))
	} else if page, err := getPage(req); err != nil {
		self.BadRequest(w, err)
		return
	} else if runs, err := self.RunService.GetByActionId(id, page); err != nil {
		self.ServerError(w, errors.WithMessagef(err, "Could not get Runs by Action ID: %q", id))
		return
	} else if err := render("action/runs.html", w, struct {
		Runs []*domain.Run
		*repository.Page
	}{
		Runs: runs,
		Page: page,
	}); err != nil {
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
	} else if err := render("action/[id].html", w, action); err != nil {
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

	var action *domain.Action
	if err := self.Db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		var err error
		action, err = self.ActionService.Create(tx, source, name)
		return err
	}); err != nil {
		self.ServerError(w, err)
		return
	} else {
		http.Redirect(w, req, "/action/"+action.ID.String(), http.StatusFound)
		return
	}
}

func (self *Web) RunIdCancelGet(w http.ResponseWriter, req *http.Request) {
	id, err := uuid.Parse(mux.Vars(req)["id"])
	if err != nil {
		self.ClientError(w, err)
		return
	}

	if run, err := self.RunService.GetByNomadJobId(id); err != nil {
		self.ClientError(w, err)
		return
	} else if err := self.RunService.Cancel(&run); err != nil {
		self.ServerError(w, errors.WithMessagef(err, "Failed to cancel Run %q", id))
		return
	}

	if referer := req.Header.Get("Referer"); referer != "" {
		http.Redirect(w, req, referer, http.StatusFound)
	} else {
		http.Redirect(w, req, "/run/"+id.String(), http.StatusFound)
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

	output, err := self.RunService.GetOutputByNomadJobId(id)
	if err != nil && !pgxscan.NotFound(err) {
		self.ServerError(w, err)
		return
	}

	facts, err := self.FactService.GetByRunId(id)
	if err != nil && !pgxscan.NotFound(err) {
		self.ServerError(w, err)
		return
	}

	if err := render("run/[id].html", w, map[string]interface{}{
		"Run":    run,
		"output": output,
		"facts":  facts,
		"allocs": allocs,
	}); err != nil {
		self.ServerError(w, err)
		return
	}
}

func getPage(req *http.Request) (*repository.Page, error) {
	page := repository.Page{}

	if offsetStr := req.FormValue("offset"); offsetStr == "" {
		page.Offset = 0
	} else if offset, err := strconv.Atoi(offsetStr); err != nil {
		return nil, errors.WithMessage(err, "offset parameter is invalid, should be positive integer")
	} else {
		page.Offset = offset
	}

	if limitStr := req.FormValue("limit"); limitStr == "" {
		page.Limit = 10
	} else if limit, err := strconv.Atoi(limitStr); err != nil {
		return nil, errors.WithMessage(err, "limit parameter is invalid, should be positive integer")
	} else {
		page.Limit = limit
	}

	return &page, nil
}

func (self *Web) RunGet(w http.ResponseWriter, req *http.Request) {
	if page, err := getPage(req); err != nil {
		self.BadRequest(w, err)
		return
	} else if runs, err := self.RunService.GetAll(page); err != nil {
		self.ServerError(w, err)
		return
	} else {
		type RunWrapper struct {
			*domain.Run
			Action *domain.Action
		}

		runWrappers := make([]RunWrapper, len(runs))
		for i, run := range runs {
			if action, err := self.ActionService.GetById(run.ActionId); err != nil {
				self.ServerError(w, err)
				return
			} else {
				runWrappers[i] = RunWrapper{
					Run:    run,
					Action: &action,
				}
			}
		}

		if err := render("run/index.html", w, struct {
			Runs []RunWrapper
			*repository.Page
		}{
			Runs: runWrappers,
			Page: page,
		}); err != nil {
			self.ServerError(w, err)
			return
		}
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
		self.ClientError(w, errors.WithMessage(err, "Failed to list actions"))
		return
	} else {
		self.json(w, wfs, http.StatusOK)
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
		self.json(w, def, http.StatusOK)
	}
}

func (self *Web) ApiRunGet(w http.ResponseWriter, req *http.Request) {
	if page, err := getPage(req); err != nil {
		self.ServerError(w, err)
	} else {
		if runs, err := self.RunService.GetAll(page); err != nil {
			self.ServerError(w, errors.WithMessage(err, "failed to fetch actions"))
		} else {
			self.json(w, runs, http.StatusOK)
		}
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

	var action *domain.Action
	var err error

	if err := self.Db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		if params.Name != nil {
			action, err = self.ActionService.Create(tx, params.Source, *params.Name)
			if err != nil {
				return err
			}
		} else {
			if actionNames, err := self.EvaluationService.ListActions(params.Source); err != nil {
				return errors.WithMessage(err, "Failed to list actions")
			} else {
				for _, actionName := range actionNames {
					action, err = self.ActionService.Create(tx, params.Source, actionName)
					if err != nil {
						return err
					}
				}
			}
		}
		return nil
	}); err != nil {
		self.ClientError(w, err) //TODO: checking
		return
	}

	self.json(w, action, http.StatusOK)
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
	self.json(w, run, http.StatusOK)
}

func (self *Web) ApiRunIdFactPost(w http.ResponseWriter, req *http.Request) {
	run, err := self.getRun(req)
	if err != nil {
		self.ClientError(w, err) //TODO: review 5XX error in openAPi documentation
		return
	}

	var value interface{}
	if err := json.NewDecoder(req.Body).Decode(&value); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Could not unmarshal fact value from request body"))
		return
	}

	fact := domain.Fact{
		RunId: &run.NomadJobID,
		Value: value,
	}

	if err := self.Db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		return self.FactService.Save(tx, &fact, nil)
	}); err != nil {
		self.ServerError(w, err)
		return
	}

	self.json(w, fact, http.StatusOK)
}

func (self *Web) ApiActionGet(w http.ResponseWriter, req *http.Request) {
	if actions, err := self.ActionService.GetAll(); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to get all actions"))
	} else {
		self.json(w, actions, http.StatusOK)
	}
}

// XXX respond with map[string]Action instead of []Action?
func (self *Web) ApiActionCurrentGet(w http.ResponseWriter, req *http.Request) {
	var actions []*domain.Action

	if err := self.Db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		var err error
		actions, err = self.ActionService.GetCurrent(tx)
		return errors.WithMessage(err, "Failed to get current actions")
	}); err != nil {
		self.ServerError(w, err)
		return
	}

	self.json(w, actions, http.StatusOK)
}

func (self *Web) ApiActionCurrentNameGet(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	if name, err := url.PathUnescape(vars["name"]); err != nil {
		self.ClientError(w, errors.WithMessagef(err, "Invalid escaping of action name: %q", vars["name"]))
	} else if actions, err := self.ActionService.GetLatestByName(name); err != nil {
		self.ClientError(w, errors.WithMessagef(err, "Failed to get current action named %q", name))
	} else {
		self.json(w, actions, http.StatusOK)
	}
}

func (self *Web) ApiActionCurrentNameDefinitionGet(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	if name, err := url.PathUnescape(vars["name"]); err != nil {
		self.ClientError(w, errors.WithMessagef(err, "Invalid escaping of action name: %q", vars["name"]))
	} else if action, err := self.ActionService.GetLatestByName(name); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Failed to get action"))
	} else if actionDef, err := self.EvaluationService.EvaluateAction(action.Source, action.Name, action.ID); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to evaluate action"))
	} else {
		self.json(w, actionDef, http.StatusOK)
	}
}

func (self *Web) ApiActionIdGet(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	if id, err := uuid.Parse(vars["id"]); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Failed to parse id"))
	} else if action, err := self.ActionService.GetById(id); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to get action"))
	} else {
		self.json(w, action, http.StatusOK)
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
		self.json(w, actionDef, http.StatusOK)
	}
}

func (self *Web) ApiRunIdLogsGet(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	if id, err := uuid.Parse(vars["id"]); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Failed to parse id"))
	} else {
		run, err := self.RunService.GetByNomadJobId(id)
		if err != nil {
			self.ClientError(w, errors.WithMessage(err, "Failed to fetch job"))
			return
		}

		if logs, err := self.RunService.JobLogs(id, run.CreatedAt, run.FinishedAt); err != nil {
			self.ServerError(w, errors.WithMessage(err, "Failed to get logs"))
		} else {
			self.json(w, map[string]*domain.LokiOutput{"logs": logs}, http.StatusOK)
		}
	}
}

func (self *Web) ApiFactIdGet(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	if id, err := uuid.Parse(vars["id"]); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Failed to parse id"))
	} else if fact, err := self.FactService.GetById(id); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to get Fact"))
	} else {
		self.json(w, fact, http.StatusOK)
	}
}

func (self *Web) ApiFactIdBinaryGet(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	if id, err := uuid.Parse(vars["id"]); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Failed to parse id"))
	} else if err := self.Db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		if binary, err := self.FactService.GetBinaryById(tx, id); err != nil {
			return errors.WithMessage(err, "Failed to get binary")
		} else {
			http.ServeContent(w, req, "", time.Time{}, binary)
			if err := binary.Close(); err != nil {
				return errors.WithMessage(err, "Failed to close binary")
			}
		}
		return nil
	}); err != nil {
		self.ServerError(w, errors.WithMessage(err, "While fetching and writing binary"))
	}
}

func (self *Web) ApiFactPost(w http.ResponseWriter, req *http.Request) {
	fact := domain.Fact{}
	factDecoder := json.NewDecoder(req.Body)
	if err := factDecoder.Decode(&fact.Value); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Could not unmarshal json body"))
		return
	}

	if err := self.Db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		err := self.FactService.Save(tx, &fact, io.MultiReader(factDecoder.Buffered(), req.Body))
		return errors.WithMessage(err, "Failed to save Fact")
	}); err != nil {
		self.ServerError(w, err)
		return
	}

	self.json(w, fact, http.StatusOK)
}
