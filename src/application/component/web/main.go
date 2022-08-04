package web

import (
	"context"
	"encoding/json"
	"io"
	"net/http"
	"net/url"
	"strconv"
	"sync"
	"time"

	"github.com/davidebianchi/gswagger/apirouter"
	"github.com/google/uuid"
	"github.com/gorilla/mux"
	nomad "github.com/hashicorp/nomad/api"
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
	InvocationService service.InvocationService
	RunService        service.RunService
	ActionService     service.ActionService
	FactService       service.FactService
	NomadEventService service.NomadEventService
	EvaluationService service.EvaluationService
	Db                config.PgxIface
}

func (self *Web) Start(ctx context.Context) error {
	self.Logger.Info().Str("listen", self.Listen).Msg("Starting")

	muxRouter := mux.NewRouter().StrictSlash(true).UseEncodedPath()
	muxRouter.NotFoundHandler = http.NotFoundHandler()

	r, err := apidoc.NewRouterDocumented(apirouter.NewGorillaMuxRouter(muxRouter), "Cicero REST API", "1.0.0", "cicero", ctx)
	if err != nil {
		return errors.WithMessage(err, "Failed to create swagger router")
	}

	// sorted alphabetically, please keep it this way
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
		"/api/action/{id}",
		self.ApiActionIdGet,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "id", Description: "id of the action", Value: "UUID"}}),
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, domain.Action{}, "Ok")),
	); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodPatch,
		"/api/action/{id}",
		self.ApiActionIdPatch,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "id", Description: "id of the action", Value: "UUID"}}),
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
	if _, err := r.AddRoute(http.MethodGet,
		"/api/invocation/{id}/inputs",
		self.ApiInvocationIdInputsGet,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "id", Description: "id of an Invocation", Value: "UUID"}}),
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, domain.Run{}, "OK")),
	); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet,
		"/api/invocation/{id}",
		self.ApiInvocationIdGet,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "id", Description: "id of an Invocation", Value: "UUID"}}),
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, domain.Invocation{}, "OK")),
	); err != nil {
		return err
	}
	if route, err := r.AddRoute(http.MethodGet,
		"/api/invocation",
		self.ApiInvocationByInputGet,
		apidoc.BuildSwaggerDef(
			nil,
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, []domain.Invocation{}, "OK")),
	); err != nil {
		return err
	} else {
		route.(*mux.Route).Queries("input", "")
	}
	if _, err := r.AddRoute(http.MethodGet,
		"/api/invocation",
		self.ApiInvocationGet,
		apidoc.BuildSwaggerDef(
			nil,
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, []domain.Invocation{}, "OK")),
	); err != nil {
		return err
	}
	var value interface{} //TODO: WIP
	if _, err := r.AddRoute(http.MethodPost,
		"/api/run/{id}/fact",
		self.ApiRunIdFactPost,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "id", Description: "id of a run", Value: "UUID"}}),
			apidoc.BuildBodyRequest(value),
			apidoc.BuildResponseSuccessfully(http.StatusNoContent, nil, "NoContent")),
	); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet,
		"/api/run/{id}/log",
		self.ApiRunIdLogGet,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "id", Description: "id of a run", Value: "UUID"}}),
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, service.LokiLog{}, "OK")),
	); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet,
		"/api/run/{id}",
		self.ApiRunIdGet,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "id", Description: "id of a run", Value: "UUID"}}),
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, domain.Run{}, "OK")),
	); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet,
		"/api/run/{id}/inputs",
		self.ApiRunIdInputsGet,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "id", Description: "id of a run", Value: "UUID"}}),
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, domain.Run{}, "OK")),
	); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet,
		"/api/run/{id}/output",
		self.ApiRunIdOutputGet,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "id", Description: "id of a run", Value: "UUID"}}),
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, domain.Run{}, "OK")),
	); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodDelete,
		"/api/run/{id}",
		self.ApiRunIdDelete,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "id", Description: "id of a run", Value: "UUID"}}),
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusNoContent, domain.Run{}, "OK")),
	); err != nil {
		return err
	}
	if route, err := r.AddRoute(http.MethodGet,
		"/api/run",
		self.ApiRunByInputGet,
		apidoc.BuildSwaggerDef(
			nil,
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, []domain.Run{}, "OK")),
	); err != nil {
		return err
	} else {
		route.(*mux.Route).Queries("input", "")
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
	if route, err := r.AddRoute(http.MethodGet,
		"/api/fact",
		self.ApiFactByRunGet,
		apidoc.BuildSwaggerDef(
			nil,
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, domain.Fact{}, "OK")),
	); err != nil {
		return err
	} else {
		route.(*mux.Route).Queries("run", "")
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
	muxRouter.HandleFunc("/invocation/{id}", self.InvocationIdGet).Methods(http.MethodGet)
	muxRouter.HandleFunc("/run/{id}", self.RunIdDelete).Methods(http.MethodDelete)
	muxRouter.HandleFunc("/run/{id}", self.RunIdGet).Methods(http.MethodGet)
	muxRouter.HandleFunc("/run", self.RunGet).Methods(http.MethodGet)
	muxRouter.HandleFunc("/action/current", self.ActionCurrentGet).Methods(http.MethodGet)
	muxRouter.HandleFunc("/action/new", self.ActionNewGet).Methods(http.MethodGet)
	muxRouter.HandleFunc("/action/{id}", self.ActionIdGet).Methods(http.MethodGet)
	muxRouter.HandleFunc("/action/{id}", self.ActionIdPatch).Methods(http.MethodPatch)
	muxRouter.HandleFunc("/action/{id}/run", self.ActionIdRunGet).Methods(http.MethodGet)
	muxRouter.HandleFunc("/action/{id}/version", self.ActionIdVersionGet).Methods(http.MethodGet)
	muxRouter.PathPrefix("/static/").Handler(http.StripPrefix("/", http.FileServer(http.FS(staticFs))))

	muxRouter.PathPrefix("/_dispatch/method/{method}/").HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
		req.Method = mux.Vars(req)["method"]
		http.StripPrefix("/_dispatch/method/"+req.Method, muxRouter).ServeHTTP(w, req)
	})

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
	http.Redirect(w, req, "/action/current?active", http.StatusFound)
}

func (self *Web) ActionCurrentGet(w http.ResponseWriter, req *http.Request) {
	var actions []domain.Action
	var err error

	_, active := req.URL.Query()["active"]
	if active {
		actions, err = self.ActionService.GetCurrentActive()
	} else {
		actions, err = self.ActionService.GetCurrent()
	}

	if err != nil {
		self.ServerError(w, err)
		return
	}

	if err := render("action/current.html", w, map[string]interface{}{
		"Actions": actions,
		"active":  active,
	}); err != nil {
		self.ServerError(w, err)
		return
	}
}

func (self *Web) ActionIdRunGet(w http.ResponseWriter, req *http.Request) {
	if id, err := uuid.Parse(mux.Vars(req)["id"]); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Could not parse Action ID"))
		return
	} else if page, err := getPage(req); err != nil {
		self.BadRequest(w, err)
		return
	} else if invocations, err := self.InvocationService.GetByActionId(id, page); err != nil {
		self.ServerError(w, errors.WithMessagef(err, "Could not get Invocations by Action ID: %q", id))
		return
	} else {
		type entry struct {
			Invocation *domain.Invocation
			Run        *domain.Run
		}

		entries := make([]entry, len(invocations))

		{
			errChan := make(chan error, len(invocations))

			wg := &sync.WaitGroup{}

			wg.Add(len(invocations))
			for i, invocation := range invocations {
				// copy so we don't point to loop variable
				invocation := invocation
				entries[i].Invocation = &invocation

				go func(i int, id uuid.UUID) {
					defer wg.Done()

					run, err := self.RunService.GetByInvocationId(id)
					if err != nil {
						errChan <- err
					} else {
						entries[i].Run = run
					}
				}(i, invocation.Id)
			}

			wg.Wait()

			select {
			case err := <-errChan:
				self.ServerError(w, err)
				return
			default:
			}
		}

		if err := render("action/runs.html", w, struct {
			Entries []entry
			*repository.Page
		}{entries, page}); err != nil {
			self.ServerError(w, err)
			return
		}
	}
}

func (self *Web) ActionIdVersionGet(w http.ResponseWriter, req *http.Request) {
	if id, err := uuid.Parse(mux.Vars(req)["id"]); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Could not parse Action ID"))
		return
	} else if action, err := self.ActionService.GetById(id); err != nil {
		self.ServerError(w, errors.WithMessagef(err, "Could not get Action by ID: %q", id))
		return
	} else if page, err := getPage(req); err != nil {
		self.BadRequest(w, err)
		return
	} else if actions, err := self.ActionService.GetByName(action.Name, page); err != nil {
		self.ServerError(w, errors.WithMessagef(err, "Could not get Action by name: %q", action.Name))
		return
	} else if err := render("action/version.html", w, struct {
		ActionID uuid.UUID
		Actions  []domain.Action
		*repository.Page
	}{
		ActionID: id,
		Actions:  actions,
		Page:     page,
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
	} else if action == nil {
		self.NotFound(w, nil)
	} else if _, inputs, err := self.ActionService.IsRunnable(action); err != nil {
		self.ServerError(w, errors.WithMessagef(err, "Could not get facts that satisfy inputs for Action with ID %q", id))
	} else if err := render("action/[id].html", w, map[string]interface{}{
		"Action": action,
		"inputs": inputs,
	}); err != nil {
		self.ServerError(w, err)
	}
}

func (self *Web) ActionIdPatch(w http.ResponseWriter, req *http.Request) {
	self.ApiActionIdPatch(NopResponseWriter{w}, req)

	if referer := req.Header.Get("Referer"); referer != "" {
		http.Redirect(w, req, referer, http.StatusFound)
	} else {
		http.Redirect(w, req, "/action/current?active", http.StatusFound)
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

	if action, err := self.ActionService.Create(source, name); err != nil {
		self.ServerError(w, err)
		return
	} else {
		http.Redirect(w, req, "/action/"+action.ID.String(), http.StatusFound)
		return
	}
}

func (self *Web) InvocationIdGet(w http.ResponseWriter, req *http.Request) {
	id, err := uuid.Parse(mux.Vars(req)["id"])
	if err != nil {
		self.ClientError(w, err)
		return
	}

	invocation, err := self.InvocationService.GetById(id)
	if err != nil {
		self.ServerError(w, err)
		return
	}
	if invocation == nil {
		w.WriteHeader(http.StatusNotFound)
		return
	}

	log, err := self.InvocationService.GetLog(*invocation)
	if err != nil {
		self.ServerError(w, err)
		return
	}
	log.Deduplicate()

	var run *domain.Run
	if run_, err := self.RunService.GetByInvocationId(id); err != nil {
		self.ServerError(w, err)
		return
	} else if run_ != nil {
		run = run_
	}

	var inputs map[string]domain.Fact
	if inputFactIds, err := self.InvocationService.GetInputFactIdsById(id); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to fetch input facts IDs"))
		return
	} else if inputs_, err := self.FactService.GetInvocationInputFacts(inputFactIds); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to fetch input facts"))
		return
	} else {
		inputs = inputs_
	}

	if err := render("invocation/[id].html", w, map[string]interface{}{
		"Invocation": invocation,
		"Run":        run,
		"inputs":     inputs,
		"log":        log,
	}); err != nil {
		self.ServerError(w, err)
		return
	}
}

func (self *Web) RunIdDelete(w http.ResponseWriter, req *http.Request) {
	id, err := uuid.Parse(mux.Vars(req)["id"])
	if err != nil {
		self.ClientError(w, err)
		return
	}

	self.ApiRunIdDelete(NopResponseWriter{w}, req)

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
		self.ServerError(w, errors.WithMessagef(err, "Failed to find Run %q", id))
		return
	}
	if run == nil {
		w.WriteHeader(http.StatusNotFound)
		return
	}

	invocation, err := self.InvocationService.GetById(run.InvocationId)
	if err != nil {
		self.ServerError(w, err)
		return
	}

	action, err := self.ActionService.GetById(invocation.ActionId)
	if err != nil {
		self.ServerError(w, err)
		return
	}

	allocsWithLogs, err := self.RunService.GetRunAllocationsWithLogs(*run)
	if err != nil {
		self.ServerError(w, err)
		return
	}

	allocs := []*nomad.Allocation{}
	allocsWithLogsByGroup := map[string][]service.AllocationWithLogs{}
	for _, alloc := range allocsWithLogs {
		allocs = append(allocs, alloc.Allocation)
		allocsWithLogsByGroup[alloc.TaskGroup] = append(allocsWithLogsByGroup[alloc.TaskGroup], alloc)
	}

	var inputs map[string]domain.Fact
	if inputFactIds, err := self.InvocationService.GetInputFactIdsById(run.InvocationId); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to fetch input facts IDs"))
		return
	} else if inputs_, err := self.FactService.GetInvocationInputFacts(inputFactIds); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to fetch input facts"))
		return
	} else {
		inputs = inputs_
	}

	output, err := self.InvocationService.GetOutputById(run.InvocationId)
	if err != nil {
		self.ServerError(w, err)
		return
	}

	facts, err := self.FactService.GetByRunId(id)
	if err != nil {
		self.ServerError(w, err)
		return
	}

	grafanaUrls, err := self.RunService.GrafanaUrls(allocs, run.FinishedAt)
	if err != nil {
		self.ServerError(w, err)
		return
	}

	cpuMetrics, err := self.RunService.CPUMetrics(allocs, run.FinishedAt)
	if err != nil {
		self.ServerError(w, err)
		return
	}

	memMetrics, err := self.RunService.MemMetrics(allocs, run.FinishedAt)
	if err != nil {
		self.ServerError(w, err)
		return
	}

	if err := render("run/[id].html", w, map[string]interface{}{
		"Run": struct {
			domain.Run
			Action domain.Action
		}{*run, *action},
		"inputs":                inputs,
		"output":                output,
		"facts":                 facts,
		"allocsWithLogsByGroup": allocsWithLogsByGroup,
		"metrics":               service.GroupMetrics(cpuMetrics, memMetrics),
		"grafanaUrls":           grafanaUrls,
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
	} else if invocations, err := self.InvocationService.GetAll(page); err != nil {
		self.ServerError(w, err)
		return
	} else {
		type entry struct {
			Run        *domain.Run
			Invocation *domain.Invocation
			Action     *domain.Action
		}

		entries := make([]entry, len(invocations))

		{
			errChan := make(chan error, len(invocations)*2)

			wg := &sync.WaitGroup{}

			wg.Add(len(invocations) * 2)
			for i, invocation := range invocations {
				// copy so we don't point to loop variable
				invocation := invocation
				entries[i].Invocation = &invocation

				go func(i int, id uuid.UUID) {
					defer wg.Done()
					if action, err := self.ActionService.GetByInvocationId(id); err != nil {
						errChan <- err
					} else {
						entries[i].Action = action
					}
				}(i, invocation.Id)

				go func(i int, id uuid.UUID) {
					defer wg.Done()
					if run, err := self.RunService.GetByInvocationId(id); err != nil {
						errChan <- err
					} else {
						entries[i].Run = run
					}
				}(i, invocation.Id)
			}

			wg.Wait()

			select {
			case err := <-errChan:
				self.ServerError(w, err)
				return
			default:
			}
		}

		if err := render("run/index.html", w, struct {
			Entries []entry
			*repository.Page
		}{entries, page}); err != nil {
			self.ServerError(w, err)
			return
		}
	}
}

// Use this to call API request handlers from UI request handlers.
type NopResponseWriter struct{ http.ResponseWriter }

func (w NopResponseWriter) WriteHeader(int) {}

func (w NopResponseWriter) Write(b []byte) (int, error) {
	return io.Discard.Write(b)
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

func (self *Web) ApiInvocationGet(w http.ResponseWriter, req *http.Request) {
	if page, err := getPage(req); err != nil {
		self.ServerError(w, err)
	} else if invocations, err := self.InvocationService.GetAll(page); err != nil {
		self.ServerError(w, errors.WithMessage(err, "failed to fetch Invocations"))
	} else {
		self.json(w, invocations, http.StatusOK)
	}
}

func (self *Web) ApiRunGet(w http.ResponseWriter, req *http.Request) {
	if page, err := getPage(req); err != nil {
		self.ServerError(w, err)
	} else if runs, err := self.RunService.GetAll(page); err != nil {
		self.ServerError(w, errors.WithMessage(err, "failed to fetch Runs"))
	} else {
		self.json(w, runs, http.StatusOK)
	}
}

func getByInputParams(req *http.Request) (bool, *bool, []*uuid.UUID, error) {
	query := req.URL.Query()

	_, recursive := query["recursive"]

	var ok *bool
	if okStr := query.Get("ok"); okStr != "" {
		if ok_, err := strconv.ParseBool(okStr); err != nil {
			return false, nil, nil, err
		} else {
			ok = &ok_
		}
	}

	factIds := make([]*uuid.UUID, len(query["input"]))
	for i, str := range query["input"] {
		if id, err := uuid.Parse(str); err != nil {
			return false, nil, nil, err
		} else {
			factIds[i] = &id
		}
	}

	return recursive, ok, factIds, nil
}

func (self *Web) ApiRunByInputGet(w http.ResponseWriter, req *http.Request) {
	ok := true
	if recursive, _, factIds, err := getByInputParams(req); err != nil {
		self.ClientError(w, err)
	} else if page, err := getPage(req); err != nil {
		self.ServerError(w, err)
	} else if invocations, err := self.InvocationService.GetByInputFactIds(factIds, recursive, &ok, page); err != nil {
		self.ServerError(w, errors.WithMessage(err, "failed to fetch Invocations"))
	} else {
		runs := []*domain.Run{}
		for _, invocation := range invocations {
			if run, err := self.RunService.GetByInvocationId(invocation.Id); err != nil {
				self.ServerError(w, err)
				return
			} else if run != nil {
				runs = append(runs, run)
			}
		}

		self.json(w, runs, http.StatusOK)
	}
}

func (self *Web) ApiInvocationByInputGet(w http.ResponseWriter, req *http.Request) {
	if recursive, ok, factIds, err := getByInputParams(req); err != nil {
		self.ClientError(w, err)
	} else if page, err := getPage(req); err != nil {
		self.ServerError(w, err)
	} else if invocations, err := self.InvocationService.GetByInputFactIds(factIds, recursive, ok, page); err != nil {
		self.ServerError(w, errors.WithMessage(err, "failed to fetch Invocations"))
	} else {
		self.json(w, invocations, http.StatusOK)
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
		if action, err := self.ActionService.Create(params.Source, *params.Name); err != nil {
			self.ClientError(w, err) //TODO: checking
			return
		} else {
			self.json(w, action, http.StatusOK)
		}
	} else {
		if actionNames, err := self.EvaluationService.ListActions(params.Source); err != nil {
			self.ClientError(w, errors.WithMessage(err, "Failed to list actions")) //TODO: checking
			return
		} else {
			actions := make([]*domain.Action, len(actionNames))
			for i, actionName := range actionNames {
				if action, err := self.ActionService.Create(params.Source, actionName); err != nil {
					self.ClientError(w, err) //TODO: checking
					return
				} else {
					actions[i] = action
				}
			}
			self.json(w, actions, http.StatusOK)
		}
	}
}

// Returns (_, false) if an error occurred.
// The error is already sent to the client.
func (self *Web) getRun(w http.ResponseWriter, req *http.Request) (*domain.Run, bool) {
	if id, err := uuid.Parse(mux.Vars(req)["id"]); err != nil {
		self.ClientError(w, err)
		return nil, false
	} else if run, err := self.RunService.GetByNomadJobId(id); err != nil {
		self.ServerError(w, err)
		return run, false
	} else {
		return run, true
	}
}

// Returns (_, false) if an error occurred.
// The error is already sent to the client.
func (self *Web) getInvocation(w http.ResponseWriter, req *http.Request) (*domain.Invocation, bool) {
	if id, err := uuid.Parse(mux.Vars(req)["id"]); err != nil {
		self.ClientError(w, err)
		return nil, false
	} else if invocation, err := self.InvocationService.GetById(id); err != nil {
		self.ServerError(w, err)
		return invocation, false
	} else {
		return invocation, true
	}
}

func (self *Web) ApiRunIdGet(w http.ResponseWriter, req *http.Request) {
	switch run, ok := self.getRun(w, req); {
	case !ok:
	case run == nil:
		w.WriteHeader(http.StatusNotFound)
	default:
		self.json(w, run, http.StatusOK)
	}
}

func (self *Web) ApiInvocationIdGet(w http.ResponseWriter, req *http.Request) {
	switch invocation, ok := self.getInvocation(w, req); {
	case !ok:
	case invocation == nil:
		w.WriteHeader(http.StatusNotFound)
	default:
		self.json(w, invocation, http.StatusOK)
	}
}

func (self *Web) ApiInvocationIdInputsGet(w http.ResponseWriter, req *http.Request) {
	if id, err := uuid.Parse(mux.Vars(req)["id"]); err != nil {
		self.ClientError(w, err)
	} else if inputs, err := self.InvocationService.GetInputFactIdsById(id); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Could not get Invocation's inputs"))
	} else {
		self.json(w, inputs, http.StatusOK)
	}
}

func (self *Web) ApiInvocationIdOutputGet(w http.ResponseWriter, req *http.Request) {
	//nolint:gocritic // IMHO if-else chain is better than switch here
	if id, err := uuid.Parse(mux.Vars(req)["id"]); err != nil {
		self.ClientError(w, err)
	} else if output, err := self.InvocationService.GetOutputById(id); err != nil {
		self.ServerError(w, err)
	} else if output == nil {
		w.WriteHeader(http.StatusNotFound)
	} else {
		self.json(w, output, http.StatusOK)
	}
}

func (self *Web) ApiRunIdInputsGet(w http.ResponseWriter, req *http.Request) {
	//nolint:gocritic // IMHO if-else chain is better than switch here
	if id, err := uuid.Parse(mux.Vars(req)["id"]); err != nil {
		self.ClientError(w, err)
	} else if run, err := self.RunService.GetByNomadJobId(id); err != nil {
		self.ServerError(w, err)
	} else if run == nil {
		w.WriteHeader(http.StatusNotFound)
	} else if inputs, err := self.InvocationService.GetInputFactIdsById(run.InvocationId); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Could not get Run's Invocation's inputs"))
	} else {
		self.json(w, inputs, http.StatusOK)
	}
}

func (self *Web) ApiRunIdOutputGet(w http.ResponseWriter, req *http.Request) {
	//nolint:gocritic // IMHO if-else chain is better than switch here
	if id, err := uuid.Parse(mux.Vars(req)["id"]); err != nil {
		self.ClientError(w, err)
	} else if run, err := self.RunService.GetByNomadJobId(id); err != nil {
		self.ServerError(w, err)
	} else if run == nil {
		w.WriteHeader(http.StatusNotFound)
	} else if output, err := self.InvocationService.GetOutputById(run.InvocationId); err != nil {
		self.ServerError(w, err)
	} else if output == nil {
		w.WriteHeader(http.StatusNotFound)
	} else {
		self.json(w, output, http.StatusOK)
	}
}

func (self *Web) ApiRunIdDelete(w http.ResponseWriter, req *http.Request) {
	if run, ok := self.getRun(w, req); !ok {
		return
	} else if run == nil {
		w.WriteHeader(http.StatusNotFound)
		return
	} else if err := self.RunService.Cancel(run); err != nil {
		self.ServerError(w, errors.WithMessagef(err, "Failed to cancel Run %q", run.NomadJobID))
		return
	}

	w.WriteHeader(http.StatusNoContent)
}

func (self *Web) ApiRunIdFactPost(w http.ResponseWriter, req *http.Request) {
	run, ok := self.getRun(w, req)
	if !ok {
		return
	}
	if run == nil {
		w.WriteHeader(http.StatusNotFound)
		return
	}

	fact, binary, err := self.getFact(w, req)
	defer func() {
		if err := binary.Close(); err != nil {
			self.ServerError(w, err)
		}
	}()
	if err != nil {
		self.Error(w, err)
		return
	}

	fact.RunId = &run.NomadJobID

	if err := self.FactService.Save(&fact, binary); err != nil {
		self.ServerError(w, err)
	} else {
		self.json(w, fact, http.StatusOK)
	}
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
	var actions []domain.Action
	var err error

	if _, active := req.URL.Query()["active"]; active {
		actions, err = self.ActionService.GetCurrentActive()
	} else {
		actions, err = self.ActionService.GetCurrent()
	}

	if err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to get current actions"))
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

func (self *Web) ApiActionIdPatch(w http.ResponseWriter, req *http.Request) {
	if id, err := uuid.Parse(mux.Vars(req)["id"]); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Could not parse Action ID"))
		return
	} else if action, err := self.ActionService.GetById(id); err != nil {
		self.ServerError(w, errors.WithMessagef(err, "Could not get Action by ID: %q", id))
		return
	} else {
		if active, err := strconv.ParseBool(req.PostFormValue("active")); err == nil {
			action.Active = active
		}

		if err := self.ActionService.Update(action); err != nil {
			self.ServerError(w, err)
			return
		}
	}

	w.WriteHeader(http.StatusNoContent)
}

func (self *Web) ApiRunIdLogGet(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	if id, err := uuid.Parse(vars["id"]); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Failed to parse id"))
	} else if run, err := self.RunService.GetByNomadJobId(id); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Failed to fetch job"))
	} else if run == nil {
		w.WriteHeader(http.StatusNotFound)
	} else if log, err := self.RunService.JobLog(id, run.CreatedAt, run.FinishedAt); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to get logs"))
	} else {
		self.json(w, log, http.StatusOK)
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

func (self *Web) ApiFactByRunGet(w http.ResponseWriter, req *http.Request) {
	if id, err := uuid.Parse(req.URL.Query().Get("run")); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Failed to parse Run ID"))
	} else if fact, err := self.FactService.GetByRunId(id); err != nil {
		self.ServerError(w, err)
	} else {
		self.json(w, fact, http.StatusOK)
	}
}

func (self *Web) ApiFactPost(w http.ResponseWriter, req *http.Request) {
	fact, binary, err := self.getFact(w, req)
	defer func() {
		if err := binary.Close(); err != nil {
			self.ServerError(w, err)
		}
	}()
	if err != nil {
		self.Error(w, err)
		return
	}

	if err := self.FactService.Save(&fact, binary); err != nil {
		self.ServerError(w, err)
	} else {
		self.json(w, fact, http.StatusOK)
	}
}

func (self *Web) getFact(w http.ResponseWriter, req *http.Request) (fact domain.Fact, binary io.ReadCloser, fErr error) {
	binary = io.NopCloser(io.LimitReader(nil, 0))
	if reader, err := req.MultipartReader(); err == nil {
		for i := 0; i < 2; i++ {
			if part, err := reader.NextPart(); err != nil {
				if err == io.EOF {
					if i == 0 {
						fErr = HandlerError{
							errors.New("No part for the Fact value received"),
							http.StatusPreconditionFailed,
						}
						return
					}
					break
				}
				fErr = HandlerError{err, http.StatusInternalServerError}
				return
			} else {
				switch i {
				case 0:
					factDecoder := json.NewDecoder(part)
					if err := factDecoder.Decode(&fact.Value); err != nil {
						fErr = HandlerError{
							errors.WithMessage(err, "Could not unmarshal json body"),
							http.StatusPreconditionFailed,
						}
						return
					}
				case 1:
					binary = part
				}
			}
		}
	} else if binaryReader, err := fact.FromReader(req.Body, true); err != nil {
		fErr = HandlerError{err, http.StatusPreconditionFailed}
	} else {
		binary = io.NopCloser(binaryReader)
	}
	return
}

type HandlerError struct {
	error
	StatusCode int
}

func (self HandlerError) HasError() bool {
	return self.error != nil
}

func (self *Web) ServerError(w http.ResponseWriter, err error) {
	self.Error(w, HandlerError{err, http.StatusInternalServerError})
}

func (self *Web) ClientError(w http.ResponseWriter, err error) {
	self.Error(w, HandlerError{err, http.StatusPreconditionFailed})
}

func (self *Web) NotFound(w http.ResponseWriter, err error) {
	self.Error(w, HandlerError{err, http.StatusNotFound})
}

func (self *Web) BadRequest(w http.ResponseWriter, err error) {
	self.Error(w, HandlerError{err, http.StatusBadRequest})
}

func (self *Web) Error(w http.ResponseWriter, err error) {
	status := 500
	e := self.Logger.Error()

	if handlerErr, ok := err.(HandlerError); ok {
		status = err.(HandlerError).StatusCode
		if handlerErr.HasError() {
			e = e.Err(err)
		} else {
			err = nil
		}
	}

	e.Int("status", status).Msg("Handler error")

	var msg string
	if err != nil {
		msg = err.Error()
	}

	http.Error(w, msg, status)
}

func (self *Web) json(w http.ResponseWriter, obj interface{}, status int) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	if err := json.NewEncoder(w).Encode(obj); err != nil {
		self.ServerError(w, err)
		return
	}
}
