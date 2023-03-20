package web

import (
	"bytes"
	"context"
	"encoding/json"
	"io"
	"net/http"
	"net/url"
	"strconv"
	"strings"
	"sync"
	"time"

	"cuelang.org/go/cue"
	cueerrors "cuelang.org/go/cue/errors"
	cueformat "cuelang.org/go/cue/format"
	"github.com/davidebianchi/gswagger/apirouter"
	"github.com/google/uuid"
	"github.com/gorilla/mux"
	"github.com/gorilla/sessions"
	"github.com/gorilla/websocket"
	"github.com/jackc/pgx/v4"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"
	"github.com/zitadel/oidc/pkg/client/rp"
	"github.com/zitadel/oidc/pkg/oidc"

	"github.com/input-output-hk/cicero/src/application/component/web/apidoc"
	"github.com/input-output-hk/cicero/src/application/service"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/input-output-hk/cicero/src/util"
)

type Web struct {
	Config config.WebConfig

	Logger            zerolog.Logger
	InvocationService service.InvocationService
	RunService        service.RunService
	ActionService     service.ActionService
	FactService       service.FactService
	NomadEventService service.NomadEventService
	EvaluationService service.EvaluationService
	Db                config.PgxIface

	sessions sessions.Store
}

const loginOidcPath = "/login/oidc"

func (self *Web) Start(ctx context.Context) error {
	self.Logger.Info().Str("listen", self.Config.Listen).Msg("Starting")

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
	if _, err := r.AddRoute(http.MethodPatch,
		"/api/action/current/{name}",
		self.ApiActionCurrentNamePatch,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "name", Description: "action name", Value: "string"}}),
			nil,
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
	if _, err := r.AddRoute(http.MethodPost,
		"/api/action/match",
		self.ApiActionMatchPost,
		apidoc.BuildSwaggerDef(
			nil,
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, apiActionMatchResponse{}, "OK")),
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
		"/api/invocation/{id}/log/evaluation",
		self.ApiInvocationIdLogEvaluationGet,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "id", Description: "id of an Invocation", Value: "UUID"}}),
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, service.LokiLog{}, "OK")),
	); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet,
		"/api/invocation/{id}/log/transformation",
		self.ApiInvocationIdLogTransformationGet,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "id", Description: "id of an Invocation", Value: "UUID"}}),
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, service.LokiLog{}, "OK")),
	); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodGet,
		"/api/invocation/{id}/log",
		self.ApiInvocationIdLogGet,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "id", Description: "id of an Invocation", Value: "UUID"}}),
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, service.LokiLog{}, "OK")),
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
		"/api/invocation/{id}/output",
		self.ApiInvocationIdOutputGet,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "id", Description: "id of an Invocation", Value: "UUID"}}),
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, domain.Run{}, "OK")),
	); err != nil {
		return err
	}
	if _, err := r.AddRoute(http.MethodPost,
		"/api/invocation/{id}",
		self.ApiInvocationIdPost,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{{Name: "id", Description: "id of an Invocation", Value: "UUID"}}),
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, domain.Invocation{}, "OK")),
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
	if _, err := r.AddRoute(http.MethodGet,
		"/api/run/log/{allocation}/{task}",
		self.ApiRunLogIdIdGet,
		apidoc.BuildSwaggerDef(
			apidoc.BuildSwaggerPathParams([]apidoc.PathParams{
				{Name: "allocation", Description: "id of an allocation", Value: "UUID"},
				{Name: "task", Description: "id of a task", Value: "UUID"},
			}),
			nil,
			apidoc.BuildResponseSuccessfully(http.StatusOK, service.LokiLog{}, "OK")),
	); err != nil {
		return err
	}
	var value any //TODO: WIP
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
	muxRouter.HandleFunc("/invocation/{id}", self.InvocationIdPost).Methods(http.MethodPost)
	muxRouter.HandleFunc("/invocation/{id}", self.InvocationIdGet).Methods(http.MethodGet)
	muxRouter.HandleFunc("/run/{id}", self.RunIdDelete).Methods(http.MethodDelete)
	muxRouter.HandleFunc("/run/{id}", self.RunIdGet).Methods(http.MethodGet)
	muxRouter.HandleFunc("/run", self.RunGet).Methods(http.MethodGet)
	muxRouter.HandleFunc("/action/current", self.ActionCurrentGet).Methods(http.MethodGet)
	muxRouter.HandleFunc("/action/current/{name}", self.ActionCurrentNameGet).Methods(http.MethodGet)
	muxRouter.HandleFunc("/action/current/{name}", self.ActionCurrentNamePatch).Methods(http.MethodPatch)
	muxRouter.HandleFunc("/action/new", self.ActionNewGet).Methods(http.MethodGet)
	muxRouter.HandleFunc("/action/{id}", self.ActionIdGet).Methods(http.MethodGet)
	muxRouter.HandleFunc("/action/{id}/run", self.ActionIdRunGet).Methods(http.MethodGet)
	muxRouter.HandleFunc("/action/{id}/version", self.ActionIdVersionGet).Methods(http.MethodGet)
	muxRouter.PathPrefix("/static/").Handler(http.StripPrefix("/", http.FileServer(http.FS(staticFs))))

	muxRouter.PathPrefix("/_dispatch/method/{method}/").HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
		req.Method = mux.Vars(req)["method"]
		http.StripPrefix("/_dispatch/method/"+req.Method, muxRouter).ServeHTTP(w, req)
	})

	self.sessions = sessions.NewCookieStore(self.Config.CookieAuth, self.Config.CookieEnc)

	{ // OIDC
		muxRouter.HandleFunc(loginOidcPath, self.LoginOidcGet).Methods(http.MethodGet)

		type State struct {
			Forward string
			Id      string
		}

		muxRouter.HandleFunc(loginOidcPath+"/{provider}", func(w http.ResponseWriter, req *http.Request) {
			provider, exists := self.Config.OidcProviders[mux.Vars(req)["provider"]]
			if !exists {
				self.NotFound(w, nil)
				return
			}

			if state, err := json.Marshal(State{
				req.URL.Query().Get("forward"),
				uuid.New().String(),
			}); err != nil {
				self.ClientError(w, errors.WithMessage(err, "While marshaling the `forward` parameter to JSON"))
				return
			} else {
				rp.AuthURLHandler(func() string { return string(state) }, provider)(w, req)
			}
		}).Methods(http.MethodGet)

		muxRouter.HandleFunc(loginOidcPath+"/{provider}/callback", func(w http.ResponseWriter, req *http.Request) {
			providerName := mux.Vars(req)["provider"]
			provider, exists := self.Config.OidcProviders[providerName]
			if !exists {
				self.NotFound(w, nil)
				return
			}

			rp.CodeExchangeHandler(
				rp.UserinfoCallback(func(w http.ResponseWriter, req *http.Request, tokens *oidc.Tokens, stateJson string, provider rp.RelyingParty, info oidc.UserInfo) {
					session, err := self.sessions.New(req, sessionOidc)
					if err != nil {
						self.ServerError(w, err)
						return
					}

					if infoJson, err := json.Marshal(info); err != nil {
						self.ServerError(w, err)
						return
					} else {
						session.Values[sessionOidcUserinfo] = infoJson
					}

					session.Values[sessionOidcProvider] = providerName
					session.Values[sessionOidcIdToken] = tokens.IDToken

					if err := session.Save(req, w); err != nil {
						self.ServerError(w, err)
						return
					}

					state := State{}
					if err := json.Unmarshal([]byte(stateJson), &state); err != nil {
						self.ClientError(w, err)
						return
					}

					http.Redirect(w, req, state.Forward, http.StatusFound)
				}),
				provider,
			)(w, req)
		})

		muxRouter.HandleFunc("/logout", func(w http.ResponseWriter, req *http.Request) {
			session := self.sessionOidc(w, req, true)
			if session == nil {
				return
			}

			session.Session.Options.MaxAge = -1
			if err := session.Session.Save(req, w); err != nil {
				self.ServerError(w, err)
				return
			}

			http.Redirect(w, req, loginOidcPath, http.StatusFound)
		})
	}

	// creates /documentation/cicero.json and /documentation/cicero.yaml routes
	err = r.GenerateAndExposeSwagger()
	if err != nil {
		return errors.WithMessage(err, "Failed to generate and expose swagger: %s")
	}

	server := &http.Server{Addr: self.Config.Listen, Handler: muxRouter}

	go func() {
		if err := server.ListenAndServe(); err != nil {
			self.Logger.Err(err).Msgf("Failed to start web server on %s", self.Config.Listen)
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

func (self *Web) LoginOidcGet(w http.ResponseWriter, req *http.Request) {
	session := self.sessionOidc(w, req, false)

	providers := make([]string, 0, len(self.Config.OidcProviders))
	for name := range self.Config.OidcProviders {
		providers = append(providers, name)
	}

	var provider string
	if session != nil {
		provider = session.Provider()
	}

	if err := self.render("login/oidc.html", w, session, map[string]any{
		"Providers": providers,
		"Forward":   req.URL.Query().Get("forward"),

		// only non-empty if already logged in
		"Provider": provider,
	}); err != nil {
		self.ServerError(w, err)
		return
	}
}

func (self *Web) ActionCurrentGet(w http.ResponseWriter, req *http.Request) {
	session := self.sessionOidc(w, req, false)

	var actions []domain.Action
	var err error

	_, active := req.URL.Query()["active"]
	actions, err = self.ActionService.GetCurrentByActive(active)
	if err != nil {
		self.ServerError(w, err)
		return
	}

	if err := self.render("action/current.html", w, session, map[string]any{
		"Actions": actions,
		"active":  active,
	}); err != nil {
		self.ServerError(w, err)
		return
	}
}

func (self *Web) ActionCurrentNameGet(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	if name, err := url.PathUnescape(vars["name"]); err != nil {
		self.ClientError(w, errors.WithMessagef(err, "Invalid escaping of action name: %q", vars["name"]))
		return
	} else if action, err := self.ActionService.GetLatestByName(name); err != nil {
		self.NotFound(w, err)
		return
	} else if action == nil {
		self.NotFound(w, nil)
		return
	} else {
		self.actionIdGet(w, req, action.ID)
	}
}

func (self *Web) ActionIdRunGet(w http.ResponseWriter, req *http.Request) {
	session := self.sessionOidc(w, req, true)
	if session == nil {
		return
	}

	if id, err := uuid.Parse(mux.Vars(req)["id"]); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Could not parse Action ID"))
		return
	} else if page, err := getPage(req); err != nil {
		self.ClientError(w, err)
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

		if err := self.render("action/runs.html", w, session, map[string]any{
			"Entries": entries,
			"Page":    page,
		}); err != nil {
			self.ServerError(w, err)
			return
		}
	}
}

func (self *Web) ActionIdVersionGet(w http.ResponseWriter, req *http.Request) {
	session := self.sessionOidc(w, req, true)
	if session == nil {
		return
	}

	if id, err := uuid.Parse(mux.Vars(req)["id"]); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Could not parse Action ID"))
		return
	} else if action, err := self.ActionService.GetById(id); err != nil {
		self.ServerError(w, errors.WithMessagef(err, "Could not get Action by ID: %q", id))
		return
	} else if page, err := getPage(req); err != nil {
		self.ClientError(w, err)
		return
	} else if actions, err := self.ActionService.GetByName(action.Name, page); err != nil {
		self.ServerError(w, errors.WithMessagef(err, "Could not get Action by name: %q", action.Name))
		return
	} else if err := self.render("action/version.html", w, session, map[string]any{
		"ActionID": id,
		"Actions":  actions,
		"Page":     page,
	}); err != nil {
		self.ServerError(w, err)
		return
	}
}

func (self *Web) ActionIdGet(w http.ResponseWriter, req *http.Request) {
	if id, err := uuid.Parse(mux.Vars(req)["id"]); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Could not parse Action ID"))
		return
	} else {
		self.actionIdGet(w, req, id)
	}
}

func (self *Web) actionIdGet(w http.ResponseWriter, req *http.Request, id uuid.UUID) {
	session := self.sessionOidc(w, req, true)
	if session == nil {
		return
	}

	if action, err := self.ActionService.GetById(id); err != nil {
		self.ServerError(w, errors.WithMessagef(err, "Could not get Action by ID: %q", id))
		return
	} else if action == nil {
		self.NotFound(w, nil)
		return
	} else if inputs, err := self.ActionService.GetSatisfiedInputs(action); err != nil {
		self.ServerError(w, errors.WithMessagef(err, "Could not get facts that satisfy inputs for Action with ID %q", id))
		return
	} else if err := self.render("action/[id].html", w, session, map[string]any{
		"Action": action,
		"inputs": inputs,
	}); err != nil {
		self.ServerError(w, err)
		return
	}
}

func (self *Web) ActionCurrentNamePatch(w http.ResponseWriter, req *http.Request) {
	self.ApiActionCurrentNamePatch(NopResponseWriter{w}, req)

	if referer := req.Header.Get("Referer"); referer != "" {
		http.Redirect(w, req, referer, http.StatusFound)
	} else {
		http.Redirect(w, req, "/action/current?active", http.StatusFound)
	}
}

func (self *Web) ActionNewGet(w http.ResponseWriter, req *http.Request) {
	const templateName = "action/new.html"

	session := self.sessionOidc(w, req, false)

	query := req.URL.Query()
	source := query.Get("source")
	name := query.Get("name")

	// step 1
	if source == "" {
		if err := self.render(templateName, w, session, nil); err != nil {
			self.ServerError(w, err)
			return
		}
		return
	}

	// step 2
	if name == "" {
		if names, err := self.EvaluationService.ListActions(source); err != nil {
			self.ServerError(w, errors.WithMessagef(err, "While listing Actions in %q", source))
			return
		} else if err := self.render(templateName, w, session, map[string]any{"Source": source, "Names": names}); err != nil {
			self.ServerError(w, err)
			return
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

func (self *Web) InvocationIdPost(w http.ResponseWriter, req *http.Request) {
	id, err := uuid.Parse(mux.Vars(req)["id"])
	if err != nil {
		self.ClientError(w, err)
		return
	}

	invocation, runFunc, err := self.InvocationService.Retry(id)
	if err != nil {
		self.ServerError(w, err)
		return
	}
	http.Redirect(w, req, "/invocation/"+invocation.Id.String(), http.StatusFound)

	go func() {
		if runs, registerFunc, err := runFunc(self.Db); err != nil {
			self.Logger.Err(err).Stringer("invocation", invocation.Id).Msg("While invoking")
		} else if err := registerFunc(); err != nil {
			self.Logger.Err(err).Interface("runs", runs).Msg("While registering job(s) for run(s)")
		}
	}()
}

func (self *Web) InvocationIdGet(w http.ResponseWriter, req *http.Request) {
	session := self.sessionOidc(w, req, true)
	if session == nil {
		return
	}

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
		self.NotFound(w, nil)
		return
	}

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
	} else if inputs_, err := self.FactService.GetByIds(inputFactIds); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to fetch input facts"))
		return
	} else {
		inputs = inputs_
	}

	if err := self.render("invocation/[id].html", w, session, map[string]any{
		"Invocation": invocation,
		"Run":        run,
		"inputs":     inputs,
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
	session := self.sessionOidc(w, req, true)
	if session == nil {
		return
	}

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
		self.NotFound(w, nil)
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

	allocs, err := self.RunService.GetRunAllocations(*run)
	if err != nil {
		self.ServerError(w, err)
		return
	}

	allocsByGroup := allocs.ByGroup()

	var inputs map[string]domain.Fact
	if inputFactIds, err := self.InvocationService.GetInputFactIdsById(run.InvocationId); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to fetch input facts IDs"))
		return
	} else if inputs_, err := self.FactService.GetByIds(inputFactIds); err != nil {
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

	grafanaLokiUrls, err := self.RunService.GrafanaLokiUrls(allocs, run.FinishedAt)
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

	if err := self.render("run/[id].html", w, session, map[string]any{
		"Run": struct {
			domain.Run
			Action domain.Action
		}{*run, *action},
		"inputs":          inputs,
		"output":          output,
		"facts":           facts,
		"allocsByGroup":   allocsByGroup,
		"metrics":         service.GroupMetrics(cpuMetrics, memMetrics),
		"grafanaUrls":     grafanaUrls,
		"grafanaLokiUrls": grafanaLokiUrls,
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
	session := self.sessionOidc(w, req, true)
	if session == nil {
		return
	}

	if page, err := getPage(req); err != nil {
		self.ClientError(w, err)
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

		if err := self.render("run/index.html", w, session, map[string]any{
			"Entries": entries,
			"Page":    page,
		}); err != nil {
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
		return
	} else if name, err := url.PathUnescape(vars["name"]); err != nil {
		self.ClientError(w, errors.WithMessagef(err, "Invalid escaping of action name: %q", vars["name"]))
		return
	} else if idStr, err := url.PathUnescape(vars["id"]); err != nil {
		self.ClientError(w, errors.WithMessagef(err, "Invalid escaping of action ID: %q", vars["id"]))
		return
	} else if id, err := uuid.Parse(idStr); err != nil {
		self.ClientError(w, errors.WithMessagef(err, "Invalid UUID given as action ID: %q", idStr))
		return
	} else if def, err := self.EvaluationService.EvaluateAction(source, name, id); err != nil {
		self.ServerError(w, err)
		return
	} else {
		self.json(w, def, http.StatusOK)
	}
}

func (self *Web) ApiInvocationGet(w http.ResponseWriter, req *http.Request) {
	if page, err := getPage(req); err != nil {
		self.ServerError(w, err)
		return
	} else if invocations, err := self.InvocationService.GetAll(page); err != nil {
		self.ServerError(w, errors.WithMessage(err, "failed to fetch Invocations"))
		return
	} else {
		self.json(w, invocations, http.StatusOK)
	}
}

func (self *Web) ApiRunGet(w http.ResponseWriter, req *http.Request) {
	if page, err := getPage(req); err != nil {
		self.ServerError(w, err)
		return
	} else if runs, err := self.RunService.GetAll(page); err != nil {
		self.ServerError(w, errors.WithMessage(err, "failed to fetch Runs"))
		return
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
		return
	} else if page, err := getPage(req); err != nil {
		self.ServerError(w, err)
		return
	} else if invocations, err := self.InvocationService.GetByInputFactIds(factIds, recursive, &ok, page); err != nil {
		self.ServerError(w, errors.WithMessage(err, "failed to fetch Invocations"))
		return
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
		return
	} else if page, err := getPage(req); err != nil {
		self.ServerError(w, err)
		return
	} else if invocations, err := self.InvocationService.GetByInputFactIds(factIds, recursive, ok, page); err != nil {
		self.ServerError(w, errors.WithMessage(err, "failed to fetch Invocations"))
		return
	} else {
		self.json(w, invocations, http.StatusOK)
	}
}

// XXX take form body instead of json?
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
			self.ClientError(w, err)
			return
		} else {
			self.json(w, action, http.StatusOK)
		}
	} else {
		if actionNames, err := self.EvaluationService.ListActions(params.Source); err != nil {
			self.ClientError(w, errors.WithMessage(err, "Failed to list actions"))
			return
		} else {
			actions := make([]*domain.Action, len(actionNames))
			for i, actionName := range actionNames {
				if action, err := self.ActionService.Create(params.Source, actionName); err != nil {
					self.ClientError(w, err)
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
		self.NotFound(w, nil)
	default:
		self.json(w, run, http.StatusOK)
	}
}

func (self *Web) ApiInvocationIdGet(w http.ResponseWriter, req *http.Request) {
	switch invocation, ok := self.getInvocation(w, req); {
	case !ok:
	case invocation == nil:
		self.NotFound(w, nil)
	default:
		self.json(w, invocation, http.StatusOK)
	}
}

func (self *Web) ApiInvocationIdLogGet(w http.ResponseWriter, req *http.Request) {
	self.apiInvocationIdLogGet(self.InvocationService.GetLog, w, req)
}

func (self *Web) ApiInvocationIdLogEvaluationGet(w http.ResponseWriter, req *http.Request) {
	self.apiInvocationIdLogGet(self.InvocationService.GetEvalLog, w, req)
}

func (self *Web) ApiInvocationIdLogTransformationGet(w http.ResponseWriter, req *http.Request) {
	self.apiInvocationIdLogGet(self.InvocationService.GetTransformLog, w, req)
}

func (self *Web) apiInvocationIdLogGet(getLog func(context.Context, domain.Invocation) service.LokiLineChan, w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	if id, err := uuid.Parse(vars["id"]); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Failed to parse id"))
		return
	} else if invocation, err := self.InvocationService.GetById(id); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Failed to fetch invocation"))
		return
	} else if invocation == nil {
		self.NotFound(w, nil)
		return
	} else {
		self.log(func(ctx context.Context) service.LokiLineChan {
			return getLog(ctx, *invocation)
		}, w, req)
	}
}

func (self *Web) ApiInvocationIdInputsGet(w http.ResponseWriter, req *http.Request) {
	if id, err := uuid.Parse(mux.Vars(req)["id"]); err != nil {
		self.ClientError(w, err)
		return
	} else if inputs, err := self.InvocationService.GetInputFactIdsById(id); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Could not get Invocation's inputs"))
		return
	} else {
		self.json(w, inputs, http.StatusOK)
	}
}

func (self *Web) ApiInvocationIdOutputGet(w http.ResponseWriter, req *http.Request) {
	if id, err := uuid.Parse(mux.Vars(req)["id"]); err != nil {
		self.ClientError(w, err)
		return
	} else if output, err := self.InvocationService.GetOutputById(id); err != nil {
		self.ServerError(w, err)
		return
	} else if output == nil {
		self.NotFound(w, nil)
	} else {
		self.json(w, output, http.StatusOK)
	}
}

// Retries the invocation by duplicating it.
func (self *Web) ApiInvocationIdPost(w http.ResponseWriter, req *http.Request) {
	id, err := uuid.Parse(mux.Vars(req)["id"])
	if err != nil {
		self.ClientError(w, err)
		return
	}

	invocation, runFunc, err := self.InvocationService.Retry(id)
	if err != nil {
		self.ServerError(w, err)
		return
	}
	self.json(w, invocation, http.StatusOK)

	go func() {
		if runs, registerFunc, err := runFunc(self.Db); err != nil {
			self.Logger.Err(err).Stringer("invocation", invocation.Id).Msg("While invoking")
		} else if err := registerFunc(); err != nil {
			self.Logger.Err(err).Interface("runs", runs).Msg("While registering job(s) for run(s)")
		}
	}()
}

func (self *Web) ApiRunIdInputsGet(w http.ResponseWriter, req *http.Request) {
	if id, err := uuid.Parse(mux.Vars(req)["id"]); err != nil {
		self.ClientError(w, err)
		return
	} else if run, err := self.RunService.GetByNomadJobId(id); err != nil {
		self.ServerError(w, err)
		return
	} else if run == nil {
		self.NotFound(w, nil)
	} else if inputs, err := self.InvocationService.GetInputFactIdsById(run.InvocationId); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Could not get Run's Invocation's inputs"))
		return
	} else {
		self.json(w, inputs, http.StatusOK)
	}
}

func (self *Web) ApiRunIdOutputGet(w http.ResponseWriter, req *http.Request) {
	if id, err := uuid.Parse(mux.Vars(req)["id"]); err != nil {
		self.ClientError(w, err)
		return
	} else if run, err := self.RunService.GetByNomadJobId(id); err != nil {
		self.ServerError(w, err)
		return
	} else if run == nil {
		self.NotFound(w, nil)
	} else if output, err := self.InvocationService.GetOutputById(run.InvocationId); err != nil {
		self.ServerError(w, err)
		return
	} else if output == nil {
		self.NotFound(w, nil)
	} else {
		self.json(w, output, http.StatusOK)
	}
}

func (self *Web) ApiRunIdDelete(w http.ResponseWriter, req *http.Request) {
	if run, ok := self.getRun(w, req); !ok {
		return
	} else if run == nil {
		self.NotFound(w, nil)
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
		self.NotFound(w, nil)
		return
	}

	fact, binary, err := self.getFact(w, req)
	defer func() {
		if err := binary.Close(); err != nil {
			self.ServerError(w, err)
			return
		}
	}()
	if err != nil {
		self.Error(w, err)
		return
	}

	fact.RunId = &run.NomadJobID

	if _, runFunc, err := self.FactService.Save(&fact, binary); err != nil {
		self.ServerError(w, err)
		return
	} else if _, registerFunc, err := runFunc(self.Db); err != nil {
		self.ServerError(w, err)
		return
	} else if err := registerFunc(); err != nil {
		self.ServerError(w, err)
		return
	} else {
		self.json(w, fact, http.StatusOK)
	}
}

func (self *Web) ApiActionGet(w http.ResponseWriter, req *http.Request) {
	if actions, err := self.ActionService.GetAll(); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to get all actions"))
		return
	} else {
		self.json(w, actions, http.StatusOK)
	}
}

// XXX respond with map[string]Action instead of []Action?
func (self *Web) ApiActionCurrentGet(w http.ResponseWriter, req *http.Request) {
	var actions []domain.Action
	var err error

	if _, active := req.URL.Query()["active"]; active {
		actions, err = self.ActionService.GetCurrentByActive(true)
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
		return
	} else if actions, err := self.ActionService.GetLatestByName(name); err != nil {
		self.ClientError(w, errors.WithMessagef(err, "Failed to get current action named %q", name))
		return
	} else {
		self.json(w, actions, http.StatusOK)
	}
}

func (self *Web) ApiActionCurrentNamePatch(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	if active, err := strconv.ParseBool(req.PostFormValue("active")); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Could not parse active status"))
		return
	} else if name, err := url.PathUnescape(vars["name"]); err != nil {
		self.ClientError(w, errors.WithMessagef(err, "Invalid escaping of action name: %q", vars["name"]))
		return
	} else if err := self.ActionService.SetActive(name, active); err != nil {
		self.ServerError(w, err)
		return
	}

	// At the moment we only support changing active status
	// but this is a bit misleading with regards to what the endpoint is called,
	// so we should at least give a proper warning instead of ignoring all other fields silently.
	if len(req.PostForm) > 1 {
		self.ClientError(w, errors.New("Only active status can be change with this endpoint"))
		return
	}

	w.WriteHeader(http.StatusNoContent)
}

func (self *Web) ApiActionIdGet(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	if id, err := uuid.Parse(vars["id"]); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Failed to parse id"))
		return
	} else if action, err := self.ActionService.GetById(id); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to get action"))
		return
	} else {
		self.json(w, action, http.StatusOK)
	}
}

func (self *Web) ApiRunIdLogGet(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	if id, err := uuid.Parse(vars["id"]); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Failed to parse id"))
		return
	} else if run, err := self.RunService.GetByNomadJobId(id); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Failed to fetch job"))
		return
	} else if run == nil {
		self.NotFound(w, nil)
		return
	} else {
		self.log(func(ctx context.Context) service.LokiLineChan {
			return self.RunService.JobLog(ctx, *run)
		}, w, req)
	}
}

func (self *Web) ApiRunLogIdIdGet(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	if allocId, err := uuid.Parse(vars["allocation"]); err != nil {
		self.ClientError(w, errors.New("cannot parse allocation UUID"))
		return
	} else if task, exists := vars["task"]; !exists {
		self.ClientError(w, errors.New("no task name given"))
		return
	} else if alloc, err := self.NomadEventService.GetLatestEventAllocationById(allocId); err != nil {
		self.ServerError(w, err)
		return
	} else if alloc == nil {
		self.NotFound(w, errors.New("no such allocation"))
		return
	} else {
		self.log(func(ctx context.Context) service.LokiLineChan {
			return self.RunService.TaskLog(ctx, *alloc, task)
		}, w, req)
	}
}

func (self *Web) ApiFactIdGet(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	if id, err := uuid.Parse(vars["id"]); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Failed to parse id"))
		return
	} else if fact, err := self.FactService.GetById(id); err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to get Fact"))
		return
	} else {
		self.json(w, fact, http.StatusOK)
	}
}

type apiActionMatchResponse struct {
	Runnable bool                                   `json:"runnable"`
	Inputs   map[string]apiActionMatchResponseInput `json:"inputs"`
	Output   domain.OutputDefinition                `json:"output"`
}

func (self apiActionMatchResponse) MarshalJSON() ([]byte, error) {
	type Self apiActionMatchResponse

	type output struct {
		Success apiActionIoMatchResponse `json:"success"`
		Failure apiActionIoMatchResponse `json:"failure"`
	}

	result := struct {
		Self
		Output output `json:"output"`
	}{
		Self: Self(self),
		Output: output{
			Success: apiActionIoMatchResponse{
				Unified: self.Output.Success,
				Error:   self.Output.Success.Validate(cue.Concrete(true)),
			},
			Failure: apiActionIoMatchResponse{
				Unified: self.Output.Failure,
				Error:   self.Output.Failure.Validate(cue.Concrete(true)),
			},
		},
	}
	return json.Marshal(result)
}

type apiActionMatchResponseInput struct {
	SatisfiedByFact *string `json:"satisfiedByFact"`

	MatchWithDeps *cue.Value `json:"matchWithDeps"`

	MatchedAgainstFact         map[string]apiActionIoMatchResponse `json:"matchedAgainstFact"`
	MatchedAgainstFactWithDeps map[string]apiActionIoMatchResponse `json:"matchedAgainstFactWithDeps"`

	Dependencies []string `json:"dependencies"`
}

func (self apiActionMatchResponseInput) MarshalJSON() ([]byte, error) {
	type Self apiActionMatchResponseInput
	result := struct {
		Self
		MatchWithDeps *string `json:"matchWithDeps"`
	}{Self: Self(self)}

	if self.MatchWithDeps != nil {
		matchWithDeps := util.CUEString("")
		if err := matchWithDeps.FromValue(self.MatchWithDeps.Eval(), cueformat.Simplify()); err != nil {
			return nil, err
		}
		matchWithDepsOnHeap := string(matchWithDeps)
		result.MatchWithDeps = &matchWithDepsOnHeap
	}

	return json.Marshal(result)
}

type apiActionIoMatchResponse struct {
	Unified cue.Value
	Error   error
}

func (self apiActionIoMatchResponse) MarshalJSON() ([]byte, error) {
	unified := util.CUEString("")
	if err := unified.FromValue(self.Unified.Eval(), cueformat.Simplify()); err != nil {
		return nil, err
	}

	result := struct {
		Unified util.CUEString `json:"unified"`
		Errors  []string       `json:"errors"`
	}{Unified: unified}

	if errors := cueerrors.Errors(self.Error); errors != nil {
		result.Errors = make([]string, len(errors))
		for i, err := range errors {
			result.Errors[i] = err.Error()
		}
	}

	return json.Marshal(result)
}

func (self *Web) ApiActionMatchPost(w http.ResponseWriter, req *http.Request) {
	if err := req.ParseMultipartForm(1024 * 1024); err != nil { // 1 MiB
		self.ClientError(w, err)
		return
	}

	io := domain.InOutCUEString(req.PostFormValue("io"))

	formFacts := map[string]domain.Fact{}
	for formName := range req.PostForm {
		const factFormPrefix = "fact:"
		if !strings.HasPrefix(formName, factFormPrefix) {
			continue
		}

		fact := domain.Fact{}
		if err := json.Unmarshal([]byte(req.PostFormValue(formName)), &fact.Value); err != nil {
			self.ClientError(w, err)
			return
		}

		formFacts[strings.TrimPrefix(formName, factFormPrefix)] = fact
	}

	action := domain.Action{
		ID:               uuid.New(),
		Name:             "(none)",
		Source:           "(none)",
		ActionDefinition: domain.ActionDefinition{InOut: io},
	}

	response := apiActionMatchResponse{
		Inputs: map[string]apiActionMatchResponseInput{},
	}

	// Compute DAG of dependencies between inputs.
	if flow, err := action.InOut.InputsFlow(nil); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Error computing DAG of input dependencies"))
		return
	} else {
		for _, t := range flow.Tasks() {
			name := domain.InputName(t)
			responseInput := response.Inputs[name]
			for _, dep := range t.Dependencies() {
				responseInput.Dependencies = append(responseInput.Dependencies, domain.InputName(dep))
			}
			response.Inputs[name] = responseInput
		}
	}

	// Match every fact against every input in isolation.
	if inputs, err := action.InOut.Inputs(nil); err != nil {
		self.ClientError(w, err)
		return
	} else {
		for inputName, input := range inputs {
			for factName, fact := range formFacts {
				matchErr, err := self.FactService.Match(&fact, input.Match)
				if err != nil {
					self.ServerError(w, err)
					return
				}

				responseInput := response.Inputs[inputName]
				if responseInput.MatchedAgainstFact == nil {
					responseInput.MatchedAgainstFact = map[string]apiActionIoMatchResponse{}
				}
				unified := input.Match.Context().Encode(fact.Value).Unify(input.Match)
				responseInput.MatchedAgainstFact[factName] = apiActionIoMatchResponse{unified, matchErr}
				response.Inputs[inputName] = responseInput
			}
		}
	}

	okErr := errors.New("ok")
	if err := self.Db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		if _, err := tx.Exec(context.Background(), `TRUNCATE action, fact CASCADE`); err != nil {
			return err
		}

		actionService := self.ActionService.WithQuerier(tx)
		factService := self.FactService.WithQuerier(tx)

		// Create action.
		if err := actionService.Save(&action); err != nil {
			return err
		}

		// Mark action inactive to avoid triggering an invocation upon fact creation.
		if err := actionService.SetActive(action.Name, false); err != nil {
			return err
		}

		// Create all facts.
		factIdToName := map[uuid.UUID]string{}
		for name, fact := range formFacts {
			if invocations, _, err := factService.Save(&fact, nil); err != nil {
				return err
			} else if len(invocations) > 0 {
				// Just ignore the returned InvokeRunFunc because we know
				// that there is only our inactive action in the DB.
				panic("This should never happen™")
			}
			factIdToName[fact.ID] = name
		}

		// Fetch action from DB to make sure serialization works.
		if dbAction, err := actionService.GetById(action.ID); err != nil {
			return err
		} else {
			action = *dbAction
		}

		if runnable, satisfied, err := actionService.IsRunnable(&action); err != nil {
			return err
		} else {
			response.Runnable = runnable
			response.Output = action.InOut.Output(satisfied)

			if inputs, err := action.InOut.Inputs(satisfied); err != nil {
				return err
			} else {
				for inputName, input := range inputs {
					responseInput := response.Inputs[inputName]

					{
						matchWithDepsOnHeap := input.Match.Eval()
						responseInput.MatchWithDeps = &matchWithDepsOnHeap
					}

					if fact, isSatisfied := satisfied[inputName]; isSatisfied {
						factNameOnHeap := factIdToName[fact.ID]
						responseInput.SatisfiedByFact = &factNameOnHeap
					}

					// Match every fact against this input with its dependencies.
					for factName, fact := range formFacts {
						matchErr, err := self.FactService.Match(&fact, input.Match)
						if err != nil {
							return err
						}

						if responseInput.MatchedAgainstFactWithDeps == nil {
							responseInput.MatchedAgainstFactWithDeps = map[string]apiActionIoMatchResponse{}
						}
						unified := input.Match.Context().Encode(fact.Value).Unify(input.Match)
						responseInput.MatchedAgainstFactWithDeps[factName] = apiActionIoMatchResponse{unified, matchErr}
					}

					response.Inputs[inputName] = responseInput
				}
			}
		}

		// IMPORTANT
		// Return an error to roll back the transaction.
		// We do not actually want to truncate any tables!
		return okErr
	}); !errors.Is(err, okErr) {
		self.ServerError(w, err)
		return
	}

	self.json(w, response, http.StatusOK)
}

func (self *Web) ApiFactIdBinaryGet(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	if id, err := uuid.Parse(vars["id"]); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Failed to parse id"))
		return
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
		return
	}
}

func (self *Web) ApiFactByRunGet(w http.ResponseWriter, req *http.Request) {
	if id, err := uuid.Parse(req.URL.Query().Get("run")); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Failed to parse Run ID"))
		return
	} else if fact, err := self.FactService.GetByRunId(id); err != nil {
		self.ServerError(w, err)
		return
	} else {
		self.json(w, fact, http.StatusOK)
	}
}

func (self *Web) ApiFactPost(w http.ResponseWriter, req *http.Request) {
	fact, binary, err := self.getFact(w, req)
	defer func() {
		if err := binary.Close(); err != nil {
			self.ServerError(w, err)
			return
		}
	}()
	if err != nil {
		self.Error(w, err)
		return
	}

	if _, runFunc, err := self.FactService.Save(&fact, binary); err != nil {
		self.ServerError(w, err)
		return
	} else if _, registerFunc, err := runFunc(self.Db); err != nil {
		self.ServerError(w, err)
		return
	} else if err := registerFunc(); err != nil {
		self.ServerError(w, err)
		return
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
	self.Error(w, HandlerError{err, http.StatusBadRequest})
}

func (self *Web) NotFound(w http.ResponseWriter, err error) {
	self.Error(w, HandlerError{err, http.StatusNotFound})
}

func (self *Web) Error(w http.ResponseWriter, err error) {
	status := 500

	if handlerErr, ok := err.(HandlerError); ok {
		status = handlerErr.StatusCode
		if !handlerErr.HasError() {
			err = nil
		}
	}

	var e *zerolog.Event
	if status >= 500 {
		e = self.Logger.Error()
	} else {
		e = self.Logger.Debug()
	}
	e.Int("status", status).Msg("Handler error")

	var msg string
	if err != nil {
		msg = err.Error()
	}

	http.Error(w, msg, status)
}

func (self *Web) json(w http.ResponseWriter, obj any, status int) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	if err := json.NewEncoder(w).Encode(obj); err != nil {
		self.ServerError(w, err)
		return
	}
}

func (self *Web) log(logFunc func(context.Context) service.LokiLineChan, w http.ResponseWriter, req *http.Request) {
	messagesFunc := func(ctx context.Context) <-chan []byte {
		messages := make(chan []byte, 1)

		go func() {
			defer close(messages)

			log := logFunc(ctx)

			query := req.URL.Query()
			_, raw := query["raw"]
			if _, stripAnsi := query["strip-ansi"]; stripAnsi {
				log = log.StripAnsi()
			}

			for line := range log {
				if raw {
					if line.Err != nil {
						self.Logger.Err(line.Err).Msg("Received error instead of log line")
					} else {
						messages <- []byte(line.Text)
					}
				} else if buf, err := json.Marshal(line); err != nil {
					self.Logger.Err(err).Msg("While marshaling log line to JSON")
				} else {
					messages <- buf
				}
			}
		}()

		return messages
	}

	if req.Header.Get("Upgrade") == "websocket" {
		self.logWS(messagesFunc, w, req)
	} else {
		self.logHTTP(messagesFunc, w, req)
	}
}

// Closes the connection after no more lines have been found after a timeout.
// We do not notice when the client stops listening so the websocket connection to Loki stays open until the timeout expires.
func (self *Web) logHTTP(messagesFunc func(context.Context) <-chan []byte, w http.ResponseWriter, req *http.Request) {
	const timeout = 15 * time.Second

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	timer := time.AfterFunc(timeout, cancel)

	messages := messagesFunc(ctx)

	/*
		You might think why not just flush after every line like this?

		```
		for line := range log {
			// … write …

			if f, ok := w.(http.Flusher); ok {
				f.Flush()
			}
		}
		```

		But it is inefficient to flush after every line.
		Instead we write as long as values are available on the channel,
		then flush, and then do the next read in a blocking fashion
		so that we don't enter a busy loop.
	*/

	flushed := false
	var message []byte
Line:
	for {
		if message != nil {
			if _, err := io.Copy(w, bytes.NewReader(message)); err != nil {
				self.ServerError(w, errors.WithMessage(err, "Error writing log line"))
				return
			}
			if _, err := w.Write([]byte("\n")); err != nil {
				self.ServerError(w, errors.WithMessage(err, "Error writing newline after log line"))
				return
			}

			message = nil
		}

		if flushed {
			// this blocks so that we don't flush in a busy loop
			msg, ok := <-messages

			if !timer.Stop() {
				timer.Reset(timeout)
			}

			if !ok {
				break
			}

			message = msg
			flushed = false
			continue
		}

		select {
		case msg, ok := <-messages:
			if !ok {
				break Line
			}
			message = msg
		default:
			// flush if there is no value to read yet
			if f, ok := w.(http.Flusher); ok {
				f.Flush()
			}
			flushed = true
		}
	}
}

var websocketUpgrader = websocket.Upgrader{}

func (self *Web) logWS(messagesFunc func(context.Context) <-chan []byte, w http.ResponseWriter, req *http.Request) {
	conn, err := websocketUpgrader.Upgrade(w, req, nil)
	if err != nil {
		self.ClientError(w, err)
		return
	}

	go func() {
		defer func() {
			if err := conn.Close(); err != nil {
				self.Logger.Err(err).Msg("While closing websocket")
			}
		}()

		noMoreMessages := false
		ctx, cancel := context.WithCancel(context.Background())
		defer func() {
			noMoreMessages = true
			cancel()
		}()

		/* XXX send heartbeats to keep connection healty through proxies if unstable
		go func() {
			defer cancel()
			const d = 5 * time.Second
			for {
				if err := conn.WriteControl(websocket.PingMessage, nil, time.Now().Add(d)); err != nil {
					break
				}
				time.Sleep(d)
			}
		}()
		*/

		// Cancel context to disconnect from Loki when the connection is closed.
		go func() {
			defer cancel()
			for {
				if _, _, err := conn.NextReader(); err != nil {
					if _, ok := err.(*websocket.CloseError); ok || noMoreMessages {
						break
					}
				}
			}
		}()

		for message := range messagesFunc(ctx) {
			if err := conn.WriteMessage(websocket.TextMessage, message); err != nil {
				self.Logger.Err(err).Msg("While writing message to websocket")
			}
		}
	}()
}

const (
	sessionOidc         = "oidc"
	sessionOidcUserinfo = "userinfo"
	sessionOidcProvider = "provider"
	sessionOidcIdToken  = "id-token"
)

// For protected endpoints do this (before sending the first byte):
//
// session := self.sessionOidc(w, req, true)
// if session == nil { return }
func (self *Web) sessionOidc(w http.ResponseWriter, req *http.Request, private bool) *SessionOidc {
	session, err := self.sessions.Get(req, sessionOidc)
	sessionOidc := SessionOidc{Session: session}
	if err != nil || sessionOidc.UserInfo() == nil {
		if private {
			forward := req.URL.RequestURI()
			if req.URL.RawFragment != "" {
				forward += "#" + req.URL.RawFragment
			}

			loginUriQuery := url.Values{}
			loginUriQuery.Add("forward", forward)

			loginUri := url.URL{
				Path:     loginOidcPath,
				RawQuery: loginUriQuery.Encode(),
			}

			http.Redirect(w, req, loginUri.RequestURI(), http.StatusFound)
		}
		return nil
	}
	return &sessionOidc
}

type SessionOidc struct {
	Session *sessions.Session

	userinfo *oidc.UserInfo // for caching
}

func (self SessionOidc) UserInfo() *oidc.UserInfo {
	if self.userinfo != nil {
		return self.userinfo
	}

	info := oidc.NewUserInfo()
	if infoJsonIf, exists := self.Session.Values[sessionOidcUserinfo]; !exists {
		return nil
	} else if infoJson, ok := infoJsonIf.([]byte); !ok {
		return nil
	} else if err := json.Unmarshal(infoJson, info); err != nil {
		return nil
	}

	infoRo := oidc.UserInfo(info)
	self.userinfo = &infoRo
	return self.userinfo
}

func (self SessionOidc) Provider() string {
	return self.getString(sessionOidcProvider)
}

func (self SessionOidc) IdToken() string {
	return self.getString(sessionOidcIdToken)
}

func (self SessionOidc) getString(key string) string {
	if v, exists := self.Session.Values[key]; !exists {
		panic("This should never happen™")
	} else {
		return v.(string)
	}
}

func (self *Web) render(route string, w http.ResponseWriter, session *SessionOidc, data map[string]any) error {
	if data == nil {
		data = make(map[string]any, 1)
	} else if _, exists := data["Session"]; exists {
		panic(`Render data must not contain key "Session"`)
	}
	data["Session"] = session
	return render(route, w, data)
}
