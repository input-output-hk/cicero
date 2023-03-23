package web

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
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
	"github.com/google/uuid"
	"github.com/gorilla/mux"
	"github.com/gorilla/securecookie"
	"github.com/gorilla/sessions"
	"github.com/gorilla/websocket"
	"github.com/jackc/pgx/v5"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"
	"github.com/zitadel/oidc/v2/pkg/client/rp"
	"github.com/zitadel/oidc/v2/pkg/client/rs"
	"github.com/zitadel/oidc/v2/pkg/oidc"

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
	ActionNameService service.ActionNameService
	FactService       service.FactService
	NomadEventService service.NomadEventService
	EvaluationService service.EvaluationService
	SessionService    service.SessionService
	Db                config.PgxIface
}

const loginOidcPath = "/login/oidc"

func (self *Web) Start(ctx context.Context) error {
	self.Logger.Info().Str("listen", self.Config.Listen).Msg("Starting")

	router := mux.NewRouter().StrictSlash(true).UseEncodedPath()
	router.NotFoundHandler = http.NotFoundHandler()

	// sorted alphabetically, please keep it this way
	router.HandleFunc("/api/action/current/{name}", self.ApiActionCurrentNameGet).Methods(http.MethodGet)
	router.HandleFunc("/api/action/current/{name}", self.ApiActionCurrentNamePatch).Methods(http.MethodPatch)
	router.HandleFunc("/api/action/current", self.ApiActionCurrentGet).Methods(http.MethodGet)
	router.HandleFunc("/api/action/definition/{source}/{name}/{id}", self.ApiActionDefinitionSourceNameIdGet).Methods(http.MethodGet)
	router.HandleFunc("/api/action/definition/{source}", self.ApiActionDefinitionSourceGet).Methods(http.MethodGet)
	router.HandleFunc("/api/action/match", self.ApiActionMatchPost).Methods(http.MethodPost)
	router.HandleFunc("/api/action/{id}", self.ApiActionIdGet).Methods(http.MethodGet)
	router.HandleFunc("/api/action", self.ApiActionGet).Methods(http.MethodGet)
	router.HandleFunc("/api/action", self.ApiActionPost).Methods(http.MethodPost)
	router.HandleFunc("/api/invocation/{id}/log/evaluation", self.ApiInvocationIdLogEvaluationGet).Methods(http.MethodGet)
	router.HandleFunc("/api/invocation/{id}/log/transformation", self.ApiInvocationIdLogTransformationGet).Methods(http.MethodGet)
	router.HandleFunc("/api/invocation/{id}/log", self.ApiInvocationIdLogGet).Methods(http.MethodGet)
	router.HandleFunc("/api/invocation/{id}/inputs", self.ApiInvocationIdInputsGet).Methods(http.MethodGet)
	router.HandleFunc("/api/invocation/{id}/output", self.ApiInvocationIdOutputGet).Methods(http.MethodGet)
	router.HandleFunc("/api/invocation/{id}", self.ApiInvocationIdPost).Methods(http.MethodPost)
	router.HandleFunc("/api/invocation/{id}", self.ApiInvocationIdGet).Methods(http.MethodGet)
	router.HandleFunc("/api/invocation", self.ApiInvocationByInputGet).Methods(http.MethodGet).Queries("input", "")
	router.HandleFunc("/api/invocation", self.ApiInvocationGet).Methods(http.MethodGet)
	router.HandleFunc("/api/run/log/{allocation}/{task}", self.ApiRunLogIdIdGet).Methods(http.MethodGet)
	router.HandleFunc("/api/run/{id}/fact", self.ApiRunIdFactPost).Methods(http.MethodPost)
	router.HandleFunc("/api/run/{id}/log", self.ApiRunIdLogGet).Methods(http.MethodGet)
	router.HandleFunc("/api/run/{id}", self.ApiRunIdGet).Methods(http.MethodGet)
	router.HandleFunc("/api/run/{id}/inputs", self.ApiRunIdInputsGet).Methods(http.MethodGet)
	router.HandleFunc("/api/run/{id}/output", self.ApiRunIdOutputGet).Methods(http.MethodGet)
	router.HandleFunc("/api/run/{id}", self.ApiRunIdDelete).Methods(http.MethodDelete)
	router.HandleFunc("/api/run", self.ApiRunByInputGet).Methods(http.MethodGet).Queries("input", "")
	router.HandleFunc("/api/run", self.ApiRunGet).Methods(http.MethodGet)
	router.HandleFunc("/api/fact/{id}/binary", self.ApiFactIdBinaryGet).Methods(http.MethodGet)
	router.HandleFunc("/api/fact/{id}", self.ApiFactIdGet).Methods(http.MethodGet)
	router.HandleFunc("/api/fact", self.ApiFactByRunGet).Methods(http.MethodGet).Queries("run", "")
	router.HandleFunc("/api/fact", self.ApiFactPost).Methods(http.MethodPost)

	router.HandleFunc("/", self.IndexGet).Methods(http.MethodGet)
	router.HandleFunc("/invocation/{id}", self.InvocationIdPost).Methods(http.MethodPost)
	router.HandleFunc("/invocation/{id}", self.InvocationIdGet).Methods(http.MethodGet)
	router.HandleFunc("/run/{id}", self.RunIdDelete).Methods(http.MethodDelete)
	router.HandleFunc("/run/{id}", self.RunIdGet).Methods(http.MethodGet)
	router.HandleFunc("/run", self.RunGet).Methods(http.MethodGet)
	router.HandleFunc("/action/current", self.ActionCurrentGet).Methods(http.MethodGet)
	router.HandleFunc("/action/current/{name}", self.ActionCurrentNameGet).Methods(http.MethodGet)
	router.HandleFunc("/action/current/{name}", self.ActionCurrentNamePatch).Methods(http.MethodPatch)
	router.HandleFunc("/action/new", self.ActionNewGet).Methods(http.MethodGet)
	router.HandleFunc("/action/{id}", self.ActionIdGet).Methods(http.MethodGet)
	router.HandleFunc("/action/{id}/run", self.ActionIdRunGet).Methods(http.MethodGet)
	router.HandleFunc("/action/{id}/version", self.ActionIdVersionGet).Methods(http.MethodGet)
	router.PathPrefix("/static/").Handler(http.StripPrefix("/", http.FileServer(http.FS(staticFs))))

	router.PathPrefix("/_dispatch/method/{method}/").HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
		req.Method = mux.Vars(req)["method"]
		http.StripPrefix("/_dispatch/method/"+req.Method, router).ServeHTTP(w, req)
	})

	{ // OIDC
		defer self.Config.Sessions.StopCleanup(self.Config.Sessions.Cleanup(5 * time.Minute))

		go func() {
			for {
				if err, ok := <-self.refreshTokens(ctx, 5*time.Minute); ok {
					self.Logger.Err(err).Msg("While refreshing session OIDC tokens")
				} else {
					break
				}
			}
		}()

		router.HandleFunc(loginOidcPath, self.LoginOidcGet).Methods(http.MethodGet)

		type State struct {
			Forward string
			Id      string
		}

		router.HandleFunc(loginOidcPath+"/{provider}", func(w http.ResponseWriter, req *http.Request) {
			provider, exists := self.Config.OidcProviders[mux.Vars(req)["provider"]]
			if !exists {
				self.NotFound(w, nil)
				return
			}

			forward := req.URL.Query().Get("forward")
			if forward == "" {
				forward = "/login/oidc"
			}

			if state, err := json.Marshal(State{
				forward,
				uuid.New().String(),
			}); err != nil {
				self.ClientError(w, errors.WithMessage(err, "While marshaling the `forward` parameter to JSON"))
				return
			} else {
				rp.AuthURLHandler(func() string { return string(state) }, provider.RelyingParty)(w, req)
			}
		}).Methods(http.MethodGet)

		router.HandleFunc(loginOidcPath+"/{provider}/callback", func(w http.ResponseWriter, req *http.Request) {
			providerName := mux.Vars(req)["provider"]
			provider, exists := self.Config.OidcProviders[providerName]
			if !exists {
				self.NotFound(w, nil)
				return
			}

			rp.CodeExchangeHandler(
				rp.UserinfoCallback(func(w http.ResponseWriter, req *http.Request, tokens *oidc.Tokens[*oidc.IDTokenClaims], stateJson string, provider rp.RelyingParty, info *oidc.UserInfo) {
					session, err := self.Config.Sessions.New(req, sessionOidc)
					if err != nil {
						self.ServerError(w, err)
						return
					}

					session.Options.MaxAge = int(time.Until(tokens.Expiry).Seconds())

					if infoJson, err := json.Marshal(info); err != nil {
						self.ServerError(w, err)
						return
					} else {
						session.Values[sessionOidcUserinfo] = infoJson
					}

					session.Values[sessionOidcProvider] = providerName
					session.Values[sessionOidcAccessToken] = tokens.AccessToken
					if tokens.RefreshToken != "" {
						session.Values[sessionOidcRefreshToken] = tokens.RefreshToken
					}

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
				provider.RelyingParty,
			)(w, req)
		})

		router.HandleFunc("/logout", func(w http.ResponseWriter, req *http.Request) {
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

	server := &http.Server{Addr: self.Config.Listen, Handler: router}

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
	var private *bool
	if session == nil {
		false := false
		private = &false
	}
	actions, err = self.ActionService.GetByCurrentByActiveByPrivate(true, &active, private)
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
	if id, err := uuid.Parse(mux.Vars(req)["id"]); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Could not parse Action ID"))
		return
	} else if page, err := getPage(req); err != nil {
		self.ClientError(w, err)
		return
	} else if actionName, err := self.ActionNameService.GetByActionId(id); err != nil {
		self.ServerError(w, err)
		return
	} else if actionName == nil {
		self.NotFound(w, nil)
		return
	} else if session := self.sessionOidc(w, req, actionName.Private); actionName.Private && session == nil {
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
	if id, err := uuid.Parse(mux.Vars(req)["id"]); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Could not parse Action ID"))
		return
	} else if page, err := getPage(req); err != nil {
		self.ClientError(w, err)
		return
	} else if action, err := self.ActionService.GetById(id); err != nil {
		self.ServerError(w, errors.WithMessagef(err, "Could not get Action by ID: %q", id))
		return
	} else if action == nil {
		self.NotFound(w, nil)
		return
	} else if actionName, err := self.ActionNameService.Get(action.Name); err != nil {
		self.ServerError(w, err)
		return
	} else if session := self.sessionOidc(w, req, actionName.Private); actionName.Private && session == nil {
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
	if action, err := self.ActionService.GetById(id); err != nil {
		self.ServerError(w, errors.WithMessagef(err, "Could not get Action by ID: %q", id))
		return
	} else if action == nil {
		self.NotFound(w, nil)
		return
	} else if actionName, err := self.ActionNameService.Get(action.Name); err != nil {
		self.ServerError(w, errors.WithMessagef(err, "Could not get action name %q", action.Name))
		return
	} else if session := self.sessionOidc(w, req, actionName.Private); actionName.Private && session == nil {
		return
	} else if inputs, err := self.ActionService.GetSatisfiedInputs(action); err != nil {
		self.ServerError(w, errors.WithMessagef(err, "Could not get facts that satisfy inputs for Action with ID %q", id))
		return
	} else if err := self.render("action/[id].html", w, session, map[string]any{
		"Action":     action,
		"ActionName": actionName,
		"inputs":     inputs,
	}); err != nil {
		self.ServerError(w, err)
		return
	}
}

func (self *Web) ActionCurrentNamePatch(w http.ResponseWriter, req *http.Request) {
	if self.sessionOidc(w, req, true) == nil {
		return
	}

	self.ApiActionCurrentNamePatch(NopResponseWriter{}, req)

	if referer := req.Header.Get("Referer"); referer != "" {
		http.Redirect(w, req, referer, http.StatusFound)
	} else {
		http.Redirect(w, req, "/action/current?active", http.StatusFound)
	}
}

func (self *Web) ActionNewGet(w http.ResponseWriter, req *http.Request) {
	const templateName = "action/new.html"

	session := self.sessionOidc(w, req, true)
	if session == nil {
		return
	}

	query := req.URL.Query()
	source := query.Get("source")

	// step 1
	if source == "" {
		if err := self.render(templateName, w, session, nil); err != nil {
			self.ServerError(w, err)
			return
		}
		return
	}

	// step 2
	if _, exists := query["name[0]"]; !exists {
		if names, err := self.EvaluationService.ListActions(source); err != nil {
			self.ServerError(w, errors.WithMessagef(err, "While listing Actions in %q", source))
			return
		} else if err := self.render(templateName, w, session, map[string]any{"Source": source, "Names": names}); err != nil {
			self.ServerError(w, err)
			return
		}
		return
	}

	modes := make(map[string]string, 1)
	for i := 0; ; i++ {
		idx := "[" + strconv.Itoa(i) + "]"
		if name := query.Get("name" + idx); name == "" {
			break
		} else if mode := query.Get("mode" + idx); mode == "" {
			continue
		} else {
			switch mode {
			case "public":
			case "private":
				if session == nil {
					w.Header().Add("WWW-Authenticate", `Bearer scope="oidc"`)
					self.ClientError(w, HandlerError{nil, http.StatusUnauthorized})
					return
				}
			default:
				self.ClientError(w, fmt.Errorf("Invalid value for `mode[%d]` query parameter: %s", i, mode))
				return
			}

			modes[name] = mode
		}
	}

	if err := pgx.BeginFunc(context.Background(), self.Db, func(tx pgx.Tx) error {
		actionService := self.ActionService.WithQuerier(tx)
		actionNameService := self.ActionNameService.WithQuerier(tx)

		for name, mode := range modes {
			if action, err := actionService.Create(source, name); err != nil {
				return err
			} else if len(modes) == 1 {
				http.Redirect(w, req, "/action/"+action.ID.String(), http.StatusCreated)
			}

			return actionNameService.Update(name, func(actionName *domain.ActionName, _ config.PgxIface) error {
				actionName.Private = mode == "private"
				return nil
			})
		}

		return nil
	}); err != nil {
		self.ServerError(w, err)
		return
	}

	if len(modes) > 1 {
		http.Redirect(w, req, "/action/current?active", http.StatusCreated)
	}
}

func (self *Web) InvocationIdPost(w http.ResponseWriter, req *http.Request) {
	if self.sessionOidc(w, req, true) == nil {
		return
	}

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

	var session *SessionOidc
	if actionName, err := self.ActionNameService.GetByActionId(invocation.ActionId); err != nil || actionName == nil {
		self.ServerError(w, err)
		return
	} else {
		session = self.sessionOidc(w, req, actionName.Private)
		if actionName.Private && session == nil {
			return
		}
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
	if self.sessionOidc(w, req, true) == nil {
		return
	}

	id, err := uuid.Parse(mux.Vars(req)["id"])
	if err != nil {
		self.ClientError(w, err)
		return
	}

	self.ApiRunIdDelete(NopResponseWriter{}, req)

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

	actionName, err := self.ActionNameService.Get(action.Name)
	if err != nil {
		self.ServerError(w, err)
		return
	}

	session := self.sessionOidc(w, req, actionName.Private)
	if actionName.Private && session == nil {
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
	session := self.sessionOidc(w, req, false)

	var private *bool
	if session == nil {
		false := false
		private = &false
	}

	if page, err := getPage(req); err != nil {
		self.ClientError(w, err)
		return
	} else if invocations, err := self.InvocationService.GetByPrivate(page, private); err != nil {
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
type NopResponseWriter struct{}

func (w NopResponseWriter) Header() http.Header {
	return make(http.Header, 0)
}

func (w NopResponseWriter) WriteHeader(int) {}

func (w NopResponseWriter) Write(b []byte) (int, error) {
	return io.Discard.Write(b)
}

func (self *Web) ApiActionDefinitionSourceGet(w http.ResponseWriter, req *http.Request) {
	if self.sessionOidc(w, req, true) == nil {
		return
	}

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
	if self.sessionOidc(w, req, true) == nil {
		return
	}

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
	var private *bool
	if self.sessionOidc(w, req, false) == nil {
		false := false
		private = &false
	}

	if page, err := getPage(req); err != nil {
		self.ServerError(w, err)
		return
	} else if invocations, err := self.InvocationService.GetByPrivate(page, private); err != nil {
		self.ServerError(w, errors.WithMessage(err, "failed to fetch Invocations"))
		return
	} else {
		self.json(w, invocations, http.StatusOK)
	}
}

func (self *Web) ApiRunGet(w http.ResponseWriter, req *http.Request) {
	var private *bool
	if self.sessionOidc(w, req, false) == nil {
		false := false
		private = &false
	}

	if page, err := getPage(req); err != nil {
		self.ServerError(w, err)
		return
	} else if runs, err := self.RunService.GetByPrivate(page, private); err != nil {
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
		session := self.sessionOidc(w, req, false)

		runs := make([]*domain.Run, 0, len(invocations))
		for _, invocation := range invocations {
			if actionName, err := self.ActionNameService.GetByInvocationId(invocation.Id); err != nil {
				self.ServerError(w, err)
				return
			} else if actionName.Private && session == nil {
				continue
			} else if run, err := self.RunService.GetByInvocationId(invocation.Id); err != nil {
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
		var invocationsToSend []domain.Invocation

		if session := self.sessionOidc(w, req, false); session != nil {
			invocationsToSend = invocations
		} else {
			for _, invocation := range invocations {
				if actionName, err := self.ActionNameService.GetByInvocationId(invocation.Id); err != nil {
					self.ServerError(w, err)
					return
				} else if actionName.Private {
					continue
				}

				invocation := invocation // copy so we don't point to loop variable
				invocationsToSend = append(invocationsToSend, invocation)
			}
		}

		self.json(w, invocationsToSend, http.StatusOK)
	}
}

// XXX take form body instead of json?
type apiActionPostBody struct {
	Source  string  `json:"source"`
	Name    *string `json:"name"`
	Private bool    `json:"private"`
}

func (self *Web) ApiActionPost(w http.ResponseWriter, req *http.Request) {
	if self.sessionOidc(w, req, true) == nil {
		return
	}

	params := apiActionPostBody{}
	if err := json.NewDecoder(req.Body).Decode(&params); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Could not unmarshal params from request body"))
		return
	}

	if err := pgx.BeginFunc(context.Background(), self.Db, func(tx pgx.Tx) error {
		actionService := self.ActionService.WithQuerier(tx)
		actionNameService := self.ActionNameService.WithQuerier(tx)

		if params.Name != nil {
			if action, err := actionService.Create(params.Source, *params.Name); err != nil {
				return err
			} else if err := actionNameService.Update(action.Name, func(actionName *domain.ActionName, _ config.PgxIface) error {
				actionName.Private = params.Private
				return nil
			}); err != nil {
				return err
			} else {
				self.json(w, action, http.StatusOK)
			}
		} else if actionNames, err := self.EvaluationService.ListActions(params.Source); err != nil {
			return errors.WithMessage(err, "Failed to list actions")
		} else {
			actions := make([]*domain.Action, len(actionNames))
			for i, actionName := range actionNames {
				if action, err := actionService.Create(params.Source, actionName); err != nil {
					return err
				} else if err := actionNameService.Update(action.Name, func(actionName *domain.ActionName, _ config.PgxIface) error {
					actionName.Private = params.Private
					return nil
				}); err != nil {
					return err
				} else {
					actions[i] = action
				}
			}
			self.json(w, actions, http.StatusOK)
		}

		return nil
	}); err != nil {
		self.ServerError(w, err)
		return
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
	if run, ok := self.getRun(w, req); !ok {
		return
	} else if run == nil {
		self.NotFound(w, nil)
		return
	} else if actionName, err := self.ActionNameService.GetByInvocationId(run.InvocationId); err != nil {
		self.ServerError(w, err)
		return
	} else if session := self.sessionOidc(w, req, actionName.Private); actionName.Private && session == nil {
		return
	} else {
		self.json(w, run, http.StatusOK)
	}
}

func (self *Web) ApiInvocationIdGet(w http.ResponseWriter, req *http.Request) {
	if invocation, ok := self.getInvocation(w, req); !ok {
		return
	} else if invocation == nil {
		self.NotFound(w, nil)
		return
	} else if actionName, err := self.ActionNameService.GetByInvocationId(invocation.Id); err != nil {
		self.ServerError(w, err)
		return
	} else if session := self.sessionOidc(w, req, actionName.Private); actionName.Private && session == nil {
		return
	} else {
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
	} else if actionName, err := self.ActionNameService.GetByInvocationId(invocation.Id); err != nil {
		self.ServerError(w, err)
		return
	} else if session := self.sessionOidc(w, req, actionName.Private); actionName.Private && session == nil {
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
	} else if actionName, err := self.ActionNameService.GetByInvocationId(id); err != nil {
		self.ServerError(w, err)
		return
	} else if session := self.sessionOidc(w, req, actionName.Private); actionName.Private && session == nil {
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
	} else if actionName, err := self.ActionNameService.GetByInvocationId(id); err != nil {
		self.ServerError(w, err)
		return
	} else if session := self.sessionOidc(w, req, actionName.Private); actionName.Private && session == nil {
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
	if self.sessionOidc(w, req, true) == nil {
		return
	}

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
		return
	} else if actionName, err := self.ActionNameService.GetByInvocationId(run.InvocationId); err != nil {
		self.ServerError(w, err)
		return
	} else if session := self.sessionOidc(w, req, actionName.Private); actionName.Private && session == nil {
		return
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
		return
	} else if actionName, err := self.ActionNameService.GetByInvocationId(run.InvocationId); err != nil {
		self.ServerError(w, err)
		return
	} else if session := self.sessionOidc(w, req, actionName.Private); actionName.Private && session == nil {
		return
	} else if output, err := self.InvocationService.GetOutputById(run.InvocationId); err != nil {
		self.ServerError(w, err)
		return
	} else if output == nil {
		self.NotFound(w, nil)
		return
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
	} else if actionName, err := self.ActionNameService.GetByInvocationId(run.InvocationId); err != nil {
		self.ServerError(w, err)
		return
	} else if session := self.sessionOidc(w, req, actionName.Private); actionName.Private && session == nil {
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

	if actionName, err := self.ActionNameService.GetByInvocationId(run.InvocationId); err != nil {
		self.ServerError(w, err)
		return
	} else if session := self.sessionOidc(w, req, actionName.Private); actionName.Private && session == nil {
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
	var private *bool
	if self.sessionOidc(w, req, false) == nil {
		false := false
		private = &false
	}

	if actions, err := self.ActionService.GetByCurrentByActiveByPrivate(false, nil, private); err != nil {
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

	var private *bool
	if session := self.sessionOidc(w, req, false); session == nil {
		false := false
		private = &false
	}

	var active *bool
	if _, hasActive := req.URL.Query()["active"]; hasActive {
		true := true
		active = &true
	}

	actions, err = self.ActionService.GetByCurrentByActiveByPrivate(true, active, private)

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
	} else if actionName, err := self.ActionNameService.Get(name); err != nil {
		self.ServerError(w, err)
		return
	} else if actionName == nil {
		self.NotFound(w, nil)
		return
	} else if session := self.sessionOidc(w, req, false); actionName.Private && session == nil {
		self.NotFound(w, nil)
		return
	} else if action, err := self.ActionService.GetLatestByName(name); err != nil {
		self.ClientError(w, errors.WithMessagef(err, "Failed to get current action named %q", name))
		return
	} else if action == nil {
		self.NotFound(w, nil)
		return
	} else {
		self.json(w, action, http.StatusOK)
	}
}

func (self *Web) ApiActionCurrentNamePatch(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	name, err := url.PathUnescape(vars["name"])
	if err != nil {
		self.ClientError(w, errors.WithMessagef(err, "Invalid escaping of action name: %q", vars["name"]))
		return
	}

	if err := req.ParseForm(); err != nil {
		self.ServerError(w, err)
		return
	}

	if actionName, err := self.ActionNameService.Get(name); err != nil {
		self.ServerError(w, err)
		return
	} else if actionName == nil {
		self.NotFound(w, nil)
		return
	} else if session := self.sessionOidc(w, req, false); actionName.Private && session == nil {
		self.NotFound(w, nil)
		return
	}

	if err := self.ActionNameService.Update(name, func(actionName *domain.ActionName, _ config.PgxIface) error {
		for key := range req.PostForm {
			valueStr := req.PostFormValue(key)
			if valueStr == "" {
				continue
			}

			value, err := strconv.ParseBool(valueStr)
			if err != nil {
				return errors.WithMessagef(err, "Could not parse %q", key)
			}

			switch key {
			case "active":
				actionName.Active = value
			case "private":
				actionName.Private = value
			default:
				return fmt.Errorf("No such field: %q", key)
			}
		}

		return nil
	}); err != nil {
		self.ServerError(w, err)
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
	} else if action == nil {
		self.NotFound(w, nil)
		return
	} else if actionName, err := self.ActionNameService.Get(action.Name); err != nil {
		self.ServerError(w, err)
		return
	} else if session := self.sessionOidc(w, req, actionName.Private); actionName.Private && session == nil {
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
	} else if actionName, err := self.ActionNameService.GetByInvocationId(run.InvocationId); err != nil {
		self.ServerError(w, err)
		return
	} else if session := self.sessionOidc(w, req, actionName.Private); actionName.Private && session == nil {
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
	} else if runId, err := uuid.Parse(alloc.JobID); err != nil {
		self.ClientError(w, errors.WithMessage(err, "Cannot parse allocation's job's ID as UUID"))
		return
	} else if actionName, err := self.ActionNameService.GetByRunId(runId); err != nil {
		self.ServerError(w, err)
		return
	} else if session := self.sessionOidc(w, req, actionName.Private); actionName.Private && session == nil {
		return
	} else {
		self.log(func(ctx context.Context) service.LokiLineChan {
			return self.RunService.TaskLog(ctx, *alloc, task)
		}, w, req)
	}
}

func (self *Web) ApiFactIdGet(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	id, err := uuid.Parse(vars["id"])
	if err != nil {
		self.ClientError(w, errors.WithMessage(err, "Failed to parse id"))
		return
	}

	fact, err := self.FactService.GetById(id)
	if err != nil {
		self.ServerError(w, errors.WithMessage(err, "Failed to get Fact"))
		return
	}
	if fact == nil {
		self.NotFound(w, nil)
		return
	}

	actionName, err := self.ActionNameService.GetByFactId(id)
	if err != nil {
		self.ServerError(w, err)
		return
	}
	if actionName != nil {
		if session := self.sessionOidc(w, req, actionName.Private); actionName.Private && session == nil {
			self.NotFound(w, nil)
			return
		}
	}

	self.json(w, fact, http.StatusOK)
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
	if err := pgx.BeginFunc(context.Background(), self.Db, func(tx pgx.Tx) error {
		if _, err := tx.Exec(context.Background(), `TRUNCATE action, fact CASCADE`); err != nil {
			return err
		}

		actionService := self.ActionService.WithQuerier(tx)
		actionNameService := self.ActionNameService.WithQuerier(tx)
		factService := self.FactService.WithQuerier(tx)

		// Create action.
		if err := actionService.Save(&action); err != nil {
			return err
		}

		// Mark action inactive to avoid triggering an invocation upon fact creation.
		if err := actionNameService.Update(action.Name, func(actionName *domain.ActionName, _ config.PgxIface) error {
			actionName.Active = false
			return nil
		}); err != nil {
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
	id, err := uuid.Parse(vars["id"])
	if err != nil {
		self.ClientError(w, errors.WithMessage(err, "Failed to parse id"))
		return
	}

	actionName, err := self.ActionNameService.GetByFactId(id)
	if err != nil {
		self.ServerError(w, err)
		return
	}
	if actionName != nil {
		if session := self.sessionOidc(w, req, actionName.Private); actionName.Private && session == nil {
			return
		}
	}

	if err := pgx.BeginFunc(context.Background(), self.Db, func(tx pgx.Tx) error {
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
	} else if actionName, err := self.ActionNameService.GetByRunId(id); err != nil {
		self.ServerError(w, err)
		return
	} else if actionName == nil {
		self.NotFound(w, nil)
		return
	} else if session := self.sessionOidc(w, req, actionName.Private); actionName.Private && session == nil {
		return
	} else if fact, err := self.FactService.GetByRunId(id); err != nil {
		self.ServerError(w, err)
		return
	} else if fact == nil {
		self.NotFound(w, nil)
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
	if err == nil {
		http.NotFound(w, nil)
	} else {
		self.Error(w, HandlerError{err, http.StatusNotFound})
	}
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

	var msg string
	if err != nil {
		msg = err.Error()
	} else {
		msg = "Handler error"
	}

	e.Int("status", status).Msg(msg)

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
	sessionOidc             = "oidc"
	sessionOidcUserinfo     = "userinfo"
	sessionOidcProvider     = "provider"
	sessionOidcAccessToken  = "access-token"
	sessionOidcRefreshToken = "refresh-token"
)

// For protected endpoints do this (before sending the first byte):
//
// session := self.sessionOidc(w, req, true)
// if session == nil { return }
func (self *Web) sessionOidc(w http.ResponseWriter, req *http.Request, redirectToLogin bool) *SessionOidc {
	redirect := func() {
		if !redirectToLogin {
			return
		}

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

		w.Header().Add("WWW-Authenticate", `Bearer scope="oidc"`)
		http.Redirect(w, req, loginUri.RequestURI(), http.StatusUnauthorized)
	}

	// convert the Authorization header to a cookie that we can use with the sessions library
	if auth := req.Header.Get("Authorization"); strings.HasPrefix(auth, "Bearer ") {
		req.AddCookie(&http.Cookie{
			Name:  sessionOidc,
			Value: strings.TrimPrefix(auth, "Bearer "),
		})
	}

	cookie, err := req.Cookie(sessionOidc)
	if err != nil {
		redirect()
		return nil
	}

	session, err := self.Config.Sessions.Get(req, sessionOidc)
	sessionOidc := SessionOidc{
		Session: session,
		Raw:     cookie.Value,
	}
	if err != nil || sessionOidc.UserInfo() == nil {
		redirect()
		return nil
	}

	provider, providerExists := self.Config.OidcProviders[sessionOidc.Provider()]
	if !providerExists {
		redirect()
		return nil
	}

	// To check whether the token is still valid,
	// use the introspection endpoint if supported,
	// otherwise the userinfo endpoint.
	if provider.ResourceServer != nil {
		if introspection, err := rs.Introspect(req.Context(), provider.ResourceServer, sessionOidc.AccessToken()); err != nil {
			self.Logger.Err(err).
				Str("provider", sessionOidc.Provider()).
				Msg("Could not introspect OIDC session token")
			redirect()
			return nil
		} else if !introspection.Active {
			redirect()
			return nil
		}
	} else if _, err := rp.Userinfo(sessionOidc.AccessToken(), oidc.BearerToken, sessionOidc.UserInfo().Subject, provider.RelyingParty); err != nil {
		redirect()
		return nil
	}

	return &sessionOidc
}

type SessionOidc struct {
	Session *sessions.Session
	Raw     string // the raw cookie value

	userinfo *oidc.UserInfo // for caching
}

func (self SessionOidc) UserInfo() *oidc.UserInfo {
	if self.userinfo != nil {
		return self.userinfo
	}

	info := oidc.UserInfo{}
	if infoJsonIf, exists := self.Session.Values[sessionOidcUserinfo]; !exists {
		return nil
	} else if infoJson, ok := infoJsonIf.([]byte); !ok {
		return nil
	} else if err := json.Unmarshal(infoJson, &info); err != nil {
		return nil
	}

	infoRo := oidc.UserInfo(info)
	self.userinfo = &infoRo
	return self.userinfo
}

func (self SessionOidc) Provider() string {
	return self.getString(sessionOidcProvider)
}

func (self SessionOidc) AccessToken() string {
	return self.getString(sessionOidcAccessToken)
}

func (self SessionOidc) RefreshToken() string {
	return self.getString(sessionOidcRefreshToken)
}

func (self SessionOidc) getString(key string) string {
	if v, exists := self.Session.Values[key]; !exists {
		return ""
	} else if v, ok := v.(string); !ok {
		panic("This should never happen™")
	} else {
		return v
	}
}

func (self Web) refreshTokens(ctx context.Context, interval time.Duration) <-chan error {
	done := make(chan error, 1)

	go func() {
		defer close(done)

		ticker := time.NewTicker(interval)
		defer ticker.Stop()

		for {
			select {
			case <-ctx.Done():
				break
			case <-ticker.C:
			}

			sessions, err := self.SessionService.GetExpiredBy(time.Now().Add(interval))
			if err != nil {
				self.Logger.Err(err).Msg("While getting sessions that have not recently been refreshed")
				done <- err
				break
			}

			for _, session := range sessions {
				values := make(map[any]any)
				if err := securecookie.DecodeMulti(sessionOidc, string(session.Data), &values, self.Config.Sessions.Codecs...); err != nil {
					self.Logger.Err(err).Msg("While decoding session")
					continue
				}

				var refreshToken string
				if refreshToken_, exists := values[sessionOidcRefreshToken]; exists {
					if v, ok := refreshToken_.(string); ok {
						refreshToken = v
					}
				}
				if refreshToken == "" {
					continue
				}

				if newTokens, err := rp.RefreshAccessToken(self.Config.OidcProviders[values[sessionOidcProvider].(string)].RelyingParty, refreshToken, "", ""); err != nil {
					self.Logger.Err(err).Str("session", session.Key).Msg("While refreshing session OIDC token")
					continue
				} else {
					values[sessionOidcAccessToken] = newTokens.AccessToken
					if newTokens.RefreshToken != "" {
						values[sessionOidcRefreshToken] = newTokens.RefreshToken
					}
					session.ExpiresOn = newTokens.Expiry
				}

				session.ModifiedOn = time.Now()

				if encoded, err := securecookie.EncodeMulti(sessionOidc, values, self.Config.Sessions.Codecs...); err != nil {
					self.Logger.Err(err).Msg("While decoding session")
					continue
				} else {
					session.Data = encoded
				}

				if err := self.SessionService.Update(session); err != nil {
					self.Logger.Err(err).Msg("While saving session")
					continue
				}
			}
		}
	}()

	return done
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
