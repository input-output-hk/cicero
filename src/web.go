package cicero

import (
	"context"
	"embed"
	"encoding/json"
	"fmt"
	"html/template"
	"log"
	"mime"
	"net/http"
	"net/url"
	"os"
	"path"
	"strconv"
	"strings"
	"time"

	"github.com/georgysavva/scany/pgxscan"
	"github.com/google/uuid"
	"github.com/gorilla/mux"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/input-output-hk/cicero/src/service"
	"github.com/pkg/errors"
)

type WebCmd struct {
	Listen         string `arg:"--listen" default:":8080"`
	LiftbridgeAddr string `arg:"--liftbridge-addr" default:"127.0.0.1:9292"`
	PrometheusAddr string `arg:"--prometheus-addr" default:"http://127.0.0.1:3100"`
	Evaluator      string `arg:"--evaluator" default:"cicero-evaluator-nix"`
}

func (self WebCmd) init(web *Web) {
	if web.Listen == nil {
		web.Listen = &self.Listen
	}
	if web.logger == nil {
		web.logger = log.New(os.Stderr, "web: ", log.LstdFlags)
	}
	if web.messageQueueService == nil {
		if bridge, err := service.LiftbridgeConnect(self.LiftbridgeAddr); err != nil {
			web.logger.Fatalln(err.Error())
			return
		} else {
			s := service.NewMessageQueueService(DB, bridge)
			web.messageQueueService = &s
		}
	}
	if web.workflowService == nil {
		s := service.NewWorkflowService(DB, web.messageQueueService)
		web.workflowService = &s
	}
	if web.actionService == nil {
		s := service.NewActionService(DB, self.PrometheusAddr)
		web.actionService = &s
	}
	if web.evaluator == nil {
		e := NewEvaluator(self.Evaluator)
		web.evaluator = &e
	}
}

func (self WebCmd) Run() error {
	web := Web{}
	self.init(&web)
	return web.start(context.Background())
}

type Web struct {
	Listen              *string
	logger              *log.Logger
	workflowService     *service.WorkflowService
	actionService       *service.ActionService
	messageQueueService *service.MessageQueueService
	evaluator           *Evaluator
}

func (self *Web) start(ctx context.Context) error {

	self.logger.Println("Starting Web")

	router := mux.NewRouter()
	// TODO Implement different Log Levels (Info, Warning, Error etc.)
	// TODO Implement Logging to Loki
	// TODO Show Errors in viewTemplate

	workflowRouter := router.PathPrefix("/workflow").Subrouter()
	workflowRouter.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {
		name := req.URL.Query().Get("name")

		var templateName string
		var instances []*model.WorkflowInstance

		if len(name) == 0 {
			templateName = "workflow"
			if insts, err := (*self.workflowService).GetAll(); err != nil {
				err = errors.WithMessagef(err, "Could not get all workflows")
				writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.logger)
				// TODO Logging -> Grafana Loki
				return
			} else {
				instances = insts
			}
		} else {
			templateName = "workflow/index-name.html"
			if insts, err := (*self.workflowService).GetAllByName(name); err != nil {
				err = errors.WithMessagef(err, "Could not get index-name.html")
				writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.logger)
				// TODO Logging -> Grafana Loki
				return
			} else {
				instances = insts
			}
		}

		err := makeViewTemplate(templateName).Execute(w, instances)
		if err != nil {
			err = errors.WithMessagef(err, "Could not create workflow view")
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.logger)
			// TODO Logging -> Grafana Loki
			return
		}
	}).Methods("GET")

	workflowRouter.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {
		name := req.PostFormValue("name")
		source := req.PostFormValue("source")

		if err := (*self.workflowService).Start(source, name, model.WorkflowCerts{}); err != nil {
			err = errors.WithMessagef(err, "Could not start workflow by name \"%s\" and source \"%s\"", source, name)
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.logger)
			// TODO Logging -> Grafana Loki
			return
		}

		http.Redirect(w, req, "/workflow", 302)
	}).Methods("POST")

	workflowRouter.HandleFunc("{id}/", func(w http.ResponseWriter, req *http.Request) {

		vars := mux.Vars(req)
		id, ok := vars["id"]

		if !ok {
			err := errors.New("Could not read empty id")
			writeLogs(err, self.logger)
			// TODO Logging -> Grafana Loki
			return
		}

		if id, err := strconv.ParseUint(id, 10, 64); err != nil {
			err = errors.WithMessagef(err, "Could not read id \"%s\"", id)
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.logger)
			// TODO Logging -> Grafana Loki
			return

		} else if instance, err := (*self.workflowService).GetById(id); err != nil {
			err = errors.WithMessagef(err, "Could not get workflow by id \"%s", id)
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.logger)
			// TODO Logging -> Grafana Loki
			return

		} else {
			results := []map[string]interface{}{}
			err := pgxscan.Select(context.Background(), DB, &results, `
            SELECT name, payload->>'Allocation' AS alloc
            FROM (
              SELECT id, name
              FROM action_instances
              WHERE workflow_instance_id = $1
            ) action
            LEFT JOIN LATERAL (
              SELECT payload, index
              FROM nomad_events
              WHERE (payload#>>'{Allocation,JobID}')::uuid = action.id
              AND payload#>>'{Allocation,TaskGroup}' = action.name
              AND topic = 'Allocation'
              AND type = 'AllocationUpdated'
              ORDER BY index DESC LIMIT 1
            ) payload ON true;
            `, id)
			if err != nil {
				err = errors.WithMessagef(err, "Could not select action_instance from db", id)
				writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.logger)
				// TODO Logging -> Grafana Loki
				return
			}

			type wrapper struct {
				Alloc *nomad.Allocation
				Logs  *service.LokiOutput
			}

			allocs := map[string]wrapper{}

			for _, result := range results {
				alloc := &nomad.Allocation{}
				err = json.Unmarshal([]byte(result["alloc"].(string)), alloc)
				if err != nil {
				        err = errors.WithMessagef(err, "Could not unmarshal json for alloc \"%s\"", alloc)
				        writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.logger)
					// TODO Logging -> Grafana Loki
					// TODO Update ViewTemplate
				}

				logs, err := (*self.actionService).ActionLogs(alloc.ID, alloc.TaskGroup)
				if err != nil {
				        err = errors.WithMessagef(err, "Could not get logs for alloc \"%s\"", alloc)
				        writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.logger)
					// TODO Logging -> Grafana Loki
					// TODO Update ViewTemplate
				}

				allocs[result["name"].(string)] = wrapper{Alloc: alloc, Logs: logs}
			}

			makeViewTemplate("workflow/[id].html").Execute(w, map[string]interface{}{
				"Instance":   instance,
				"graph":      req.URL.Query().Get("graph"),
				"graphTypes": WorkflowGraphTypeStrings(),
				"allocs":     allocs,
			})
		}
	}).Methods("GET")

	workflowRouter.HandleFunc("{id}/graph", func(w http.ResponseWriter, req *http.Request) {

		vars := mux.Vars(req)
		route, ok := vars["id"]

		if !ok {
			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
		}

		id, err := strconv.ParseUint(route, 10, 64)
		if err != nil {
			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
		}

		instance, err := (*self.workflowService).GetById(id)
		if err != nil {
			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
		}

		def, err := self.evaluator.EvaluateWorkflow(instance.Source, instance.Name, instance.ID, instance.Certs)
		if err != nil {
			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
		}

		var graphType WorkflowGraphType
		graphTypeStr := req.URL.Query().Get("type")
		if len(graphTypeStr) > 0 {
			if gt, err := WorkflowGraphTypeFromString(graphTypeStr); err != nil {
				// TODO Logging -> Local
				// TODO Logging -> Grafana Loki
			} else {
				graphType = gt
			}
		}

		switch graphType {
		case 1:
			RenderWorkflowGraphFlow(def, w)
		case 2:
			RenderWorkflowGraphInputs(def, &instance, w)
		default:
			// should have already exited when parsing the graph type
			self.logger.Panic("reached code that should be unreachable")
		}
	}).Methods("GET")

	apiWorkflowRouter := router.PathPrefix("/api/workflow").Subrouter()
	apiWorkflowRouter.HandleFunc("definition/{source}", func(w http.ResponseWriter, req *http.Request) {

		source := req.URL.Query().Get("source")
		if wfs, err := self.evaluator.ListWorkflows(source); err != nil {
			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
		} else {

			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
			json.NewEncoder(w).Encode(wfs)
		}
	}).Methods("GET")

	apiWorkflowRouter.HandleFunc("definition/{source}/{name}", func(w http.ResponseWriter, req *http.Request) {
		var id uint64
		if idStr := req.URL.Query().Get("id"); len(idStr) > 0 {
			if iid, err := strconv.ParseUint(idStr, 10, 64); err != nil {
				// TODO Logging -> Local
				// TODO Logging -> Grafana Loki
			} else {
				id = iid
			}
		}

		var inputs model.WorkflowCerts
		if inputsStr := req.URL.Query().Get("inputs"); len(inputsStr) > 0 {
			if err := json.Unmarshal([]byte(inputsStr), &inputs); err != nil {
				// TODO Logging -> Local
				// TODO Logging -> Grafana Loki
			}
		}

		name := req.PostFormValue("name")
		source := req.PostFormValue("source")
		if wf, err := self.evaluator.EvaluateWorkflow(source, name, id, inputs); err != nil {
			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
		} else {
			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
			json.NewEncoder(w).Encode(wf)
		}
	}).Methods("GET")

	apiWorkflowRouter.HandleFunc("instance/", func(w http.ResponseWriter, req *http.Request) {

		if instances, err := (*self.workflowService).GetAll(); err != nil {
			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
		} else {
			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
			json.NewEncoder(w).Encode(instances)
		}
	}).Methods("GET")

	apiWorkflowRouter.HandleFunc("definition/instance/", func(w http.ResponseWriter, req *http.Request) {

		var params struct {
			Source string
			Name   string
			Inputs model.WorkflowCerts
		}
		if err := json.NewDecoder(req.Body).Decode(&params); err != nil {
			errors.WithMessage(err, "Could not unmarshal params from request body")
			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
		}
		if err := (*self.workflowService).Start(params.Source, params.Name, model.WorkflowCerts{}); err != nil {
			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
		}
		w.WriteHeader(204)
	}).Methods("POST")

	apiWorkflowInstanceIdRouter := apiWorkflowRouter.PathPrefix("definition/instance/id").Subrouter()
	apiWorkflowInstanceIdRouter.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {
		vars := mux.Vars(req)
		id, ok := vars["id"]

		if !ok {
			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
		}

		if id, err := strconv.ParseUint(id, 10, 64); err != nil {
			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
		} else if instance, err := (*self.workflowService).GetById(id); err != nil {

			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
		} else {
			json.NewEncoder(w).Encode(instance)
		}

	}).Methods("GET")

	apiWorkflowRouter.HandleFunc("definition/instance/id/cert", func(w http.ResponseWriter, req *http.Request) {

		vars := mux.Vars(req)
		id, ok := vars["id"]

		if !ok {
			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
		}

		if id, err := strconv.ParseUint(id, 10, 64); err != nil {
			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
		} else if instance, err := (*self.workflowService).GetById(id); err != nil {

			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
		} else {

			certs := model.WorkflowCerts{}
			if err := json.NewDecoder(req.Body).Decode(&certs); err != nil {
				errors.WithMessage(err, "Could not unmarshal certs from request body")
				// TODO Logging -> Local
				// TODO Logging -> Grafana Loki
			}

			if err := (*self.messageQueueService).Publish(
				fmt.Sprintf("workflow.%s.%d.cert", instance.Name, instance.ID),
				service.CertStreamName,
				certs,
			); err != nil {
				errors.WithMessage(err, "Could not publish certificate")
				// TODO Logging -> Local
				// TODO Logging -> Grafana Loki
			}
			w.WriteHeader(204)
		}
	}).Methods("POST")

	apiActionRouter := router.PathPrefix("/api/action").Subrouter()
	apiActionRouter.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {

		if actions, err := (*self.actionService).GetAll(); err != nil {
			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
		} else {
			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
			json.NewEncoder(w).Encode(actions)
		}
	}).Methods("GET")

	apiActionRouter.HandleFunc("{id}/", func(w http.ResponseWriter, req *http.Request) {

		vars := mux.Vars(req)
		id, ok := vars["id"]
		if !ok {
			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
		}

		if id, err := uuid.Parse(id); err != nil {
			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
		} else if action, err := (*self.actionService).GetById(id); err != nil {
			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
		} else {
			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
			json.NewEncoder(w).Encode(action)
		}
	}).Methods("GET")

	apiActionRouter.HandleFunc("{id}/logs", func(w http.ResponseWriter, req *http.Request) {

		vars := mux.Vars(req)
		id, ok := vars["id"]
		if !ok {
			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
		}

		if id, err := uuid.Parse(id); err != nil {

			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
		} else {
			logs, err := (*self.actionService).JobLogs(id)
			if err != nil {

				// TODO Logging -> Local
				// TODO Logging -> Grafana Loki
			}

			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
			json.NewEncoder(w).Encode(map[string]*service.LokiOutput{"logs": logs})
		}
	}).Methods("GET")

	router.HandleFunc("/*route", func(w http.ResponseWriter, req *http.Request) {
		vars := mux.Vars(req)
		route, ok := vars["route"]

		if !ok {
			logger.Default().logger.Println("Request: ", req, " was not ok")
			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
		}

		if mimeType := mime.TypeByExtension(path.Ext(route)); mimeType != "" {
			w.Header()["Content-Type"] = []string{mimeType}
		}
		err := makeViewTemplate(route).Execute(w, vars)
		if err != nil {
			makeViewTemplate("index.html").Execute(w, nil)
			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
			// TODO Update ViewTemplate
		}

	}).Methods("GET")

	router.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {
		err := makeViewTemplate("index.html").Execute(w, nil)
		if err != nil {
			makeViewTemplate("index.html").Execute(w, nil)
			// TODO Logging -> Local
			// TODO Logging -> Grafana Loki
			// TODO Update ViewTemplate
		}
	}).Methods("GET")

	server := &http.Server{Addr: *self.Listen, Handler: router}

	go func() {
		if err := server.ListenAndServe(); err != nil {
			self.logger.Printf("Failed to start web server: %s", err.Error())
		}
	}()

	<-ctx.Done()

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	if err := server.Shutdown(ctx); err != nil {
		self.logger.Printf("Failed to stop web server: %s", err.Error())
	}

	return nil
}

//go:embed web/* web/**/*
var viewsFs embed.FS

// Learn by example.
//
// For a/b/c templates will be parsed
// in the following order:
//
// 1. _layout.html
// 2. a/_layout.html
// 3. a/b/_layout.html
// 4. a/b/c/_layout.html
// 5. a/b/c/index.html
//
// A library I wanted to use instead:
// https://github.com/dannyvankooten/extemplate
// Does not work with embed.FS though.
func makeViewTemplate(route string) *template.Template {
	root := "web"

	t := template.New("")
	t.Funcs(template.FuncMap{
		"route": func() string { return route },
		"toJson": func(o interface{}) string {
			enc, _ := json.Marshal(o)
			return string(enc)
		},
		"pathEscape": url.PathEscape,
	})

	isDirectory := func(p string) (bool, error) {
		file, err := viewsFs.Open(root + p)
		if err != nil {
			return false, err
		}
		stat, err := file.Stat()
		if err != nil {
			return false, err
		}
		return stat.IsDir(), nil
	}

	tryParse := func(p string) {
		bytes, err := viewsFs.ReadFile(root + p)
		if err == nil && len(bytes) != 0 {
			name := p[1:] // strip leading slash
			t.New(name).Parse(string(bytes))
		}
	}

	// XXX find next index of "/" instead of splitting and joining
	parts := strings.Split("/"+route, "/")
	for i := range parts {
		step := strings.Join(parts[:i+1], "/")
		last := i == len(parts)-1

		if isDir, err := isDirectory(step); err == nil && isDir {
			tryParse(step + "/_layout.html")

			if last {
				index := step + "/index.html"
				tryParse(index)
				route = index[1:] // strip leading slash
			}
		} else if last {
			tryParse(step)
		}
	}

	if routeTmpl := t.Lookup(route); routeTmpl != nil {
		return routeTmpl
	}
	return t
}

func writeHttpErrorAndLogs(w http.ResponseWriter, err error, httpStatusCode int, logger *log.Logger) {
	http.Error(w, err.Error(), httpStatusCode)
	writeLogs(err, logger)
}

func writeLogs(err error, logger *log.Logger) {
	logger.Println(err.Error())
}
