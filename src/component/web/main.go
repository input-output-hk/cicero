package web

import (
	"context"
	"embed"
	"encoding/json"
	"html/template"
	"log"
	"mime"
	"net/http"
	"net/url"
	"path"
	"strconv"
	"strings"
	"time"

	"github.com/google/uuid"
	"github.com/gorilla/mux"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/input-output-hk/cicero/src/service"
	"github.com/pkg/errors"
)

type Web struct {
	Listen              string
	Logger              *log.Logger
	WorkflowService     service.WorkflowService
	ActionService       service.ActionService
	MessageQueueService service.MessageQueueService
	NomadEventService   service.NomadEventService
	EvaluationService   service.EvaluationService
}

func (self *Web) Start(ctx context.Context) error {
	self.Logger.Println("Starting Web")

	router := mux.NewRouter()

	workflowRouter := router.PathPrefix("/workflow").Subrouter()
	workflowRouter.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {
		name := req.URL.Query().Get("name")
		if name == "" {
			if summary, err := self.WorkflowService.GetSummary(); err != nil {
				err = errors.WithMessagef(err, "Could not get summary of workflows")
				writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
				return
			} else {
				writeSuccessLogs("Return summary of workflows", self.Logger)
				makeViewTemplate("workflow").Execute(w, summary)
				return
			}
		} else {
			if instances, err := self.WorkflowService.GetAllByName(name); err != nil {
				err = errors.WithMessagef(err, "Could not get all workflows by name \"%s\"", name)
				writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
				return
			} else {
				writeSuccessLogs("Return workflow/index-name.html", self.Logger)
				makeViewTemplate("workflow/index-name.html").Execute(w, map[string]interface{}{
					"Name":      name,
					"Instances": instances,
				})
				return
			}
		}
	}).Methods("GET")

	workflowRouter.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {
		name := req.PostFormValue("name")
		source := req.PostFormValue("source")

		if err := self.WorkflowService.Start(source, name, model.Facts{}); err != nil {
			err = errors.WithMessagef(err, "Could not start workflow by name \"%s\" and from source \"%s\"", source, name)
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
			return
		}
		http.Redirect(w, req, "/workflow", 302)
	}).Methods("POST")

	workflowRouter.HandleFunc("/new", func(w http.ResponseWriter, req *http.Request) {
		const templateName = "workflow/new.html"

		source := req.URL.Query().Get("source")
		name := req.URL.Query().Get("name")

		// step 1
		if source == "" {
			err := errors.New("Could not read empty source, reroute to default route")
			// TODO Show error in ViewTemplate
			self.Logger.Println(err.Error())
			makeViewTemplate(templateName).Execute(w, map[string]interface{}{})
			return
		}

		// step 4
		if inputsJson := req.URL.Query().Get("inputs"); len(inputsJson) > 0 {
			var inputs model.Facts
			if err := json.Unmarshal([]byte(inputsJson), &inputs); err != nil {

				err = errors.WithMessagef(err, "Could not unmarshall inputs \"%s\"", inputs)
				writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
				return
			}
			if err := self.WorkflowService.Start(source, name, inputs); err != nil {

				err = errors.WithMessagef(err, "Could not start workflow from source \"%s\"", source)
				writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
				return
			}
			http.Redirect(w, req, "/workflow/", 302)
			return
		}

		// step 3
		if len(name) > 0 {
			writeSuccessLogs("Return workflow/new.html", self.Logger)
			makeViewTemplate(templateName).Execute(w, map[string]interface{}{
				"Source": source,
				"Name":   name,
			})
			return
		}

		// step 2
		if names, err := self.EvaluationService.ListWorkflows(source); err != nil {
			err = errors.WithMessagef(err, "Could not get list of workflows by source \"%s\"", source)
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
			return
		} else {
			writeSuccessLogs("Return workflow/new.html", self.Logger)
			makeViewTemplate(templateName).Execute(w, map[string]interface{}{
				"Source": source,
				"Names":  names,
			})
			return
		}
	}).Methods("GET")

	workflowRouter.HandleFunc("{id}/", func(w http.ResponseWriter, req *http.Request) {
		vars := mux.Vars(req)
		id, ok := vars["id"]

		if !ok {
			err := errors.New("Could not read empty id")
			writeHttpErrorAndLogs(w, err, http.StatusOK, self.Logger)
			return
		}

		if id, err := strconv.ParseUint(id, 10, 64); err != nil {
			err = errors.WithMessagef(err, "Could not read id \"%s\"", id)
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
			return

		} else if instance, err := self.WorkflowService.GetById(id); err != nil {
			err = errors.WithMessagef(err, "Could not get workflow by id \"%s", id)
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
			return

		} else {
			allocs, err := self.NomadEventService.GetEventAllocByWorkflowId(id)
			if err != nil {
				err = errors.WithMessagef(err, "Could not get event alloc by workflow id \"%s", id)
				writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
				return
			}
			writeSuccessLogs("Return workflow/[id].html", self.Logger)
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
			err := errors.New("Could not read empty id")
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
			return
		}

		id, err := strconv.ParseUint(route, 10, 64)
		if err != nil {
			err = errors.WithMessagef(err, "Could not parse id \"%s\"", id)
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
			return
		}

		instance, err := self.WorkflowService.GetById(id)
		if err != nil {
			err = errors.WithMessagef(err, "Could not get workflow by id \"%s\"", id)
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
			return
		}

		def, err := self.EvaluationService.EvaluateWorkflow(instance.Source, instance.Name, instance.ID, instance.Facts)
		if err != nil {
			err = errors.WithMessagef(err, "Could not evaluate workflow with id \"%s\"", id)
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
			return
		}

		var graphType WorkflowGraphType
		graphTypeStr := req.URL.Query().Get("type")
		if len(graphTypeStr) > 0 {
			if gt, err := WorkflowGraphTypeFromString(graphTypeStr); err != nil {
				err = errors.WithMessagef(err, "Could not read graphTypeStr \"%s\"", graphTypeStr)
				writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
				return
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
			self.Logger.Panic("reached code that should be unreachable")
		}
	}).Methods("GET")

	apiWorkflowRouter := router.PathPrefix("/api/workflow").Subrouter()
	apiWorkflowRouter.HandleFunc("definition/{source}", func(w http.ResponseWriter, req *http.Request) {
		source := req.URL.Query().Get("source")
		if wfs, err := self.EvaluationService.ListWorkflows(source); err != nil {
			err = errors.WithMessagef(err, "Could not list workflows by source \"%s\"", source)
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
			return
		} else {
			writeSuccessLogs("Return workflows to client", self.Logger)
			prepareJsonResponse(w, 200)
			json.NewEncoder(w).Encode(wfs)
		}
	}).Methods("GET")

	apiWorkflowRouter.HandleFunc("definition/{source}/{name}", func(w http.ResponseWriter, req *http.Request) {
		var id uint64
		if idStr := req.URL.Query().Get("id"); len(idStr) > 0 {
			if iid, err := strconv.ParseUint(idStr, 10, 64); err != nil {
				err = errors.WithMessagef(err, "Could not parse id \"%s\"", id)
				writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
				return
			} else {
				id = iid
			}
		}

		var inputs model.Facts
		if inputsStr := req.URL.Query().Get("inputs"); len(inputsStr) > 0 {
			if err := json.Unmarshal([]byte(inputsStr), &inputs); err != nil {
				err = errors.WithMessagef(err, "Could not unmarshal inputs \"%s\"", inputs)
				writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
				return
			}
		}

		name := req.PostFormValue("name")
		source := req.PostFormValue("source")
		if wf, err := self.EvaluationService.EvaluateWorkflow(source, name, id, inputs); err != nil {
			err = errors.WithMessagef(err, "Could not evaluate workflow with name \"%s\" and source \"&s\"", name, source)
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
			return
		} else {
			writeSuccessLogs("Return evaluated workflow to client", self.Logger)
			prepareJsonResponse(w, 200)
			json.NewEncoder(w).Encode(wf)
		}
	}).Methods("GET")

	apiWorkflowInstanceRouter := apiWorkflowRouter.PathPrefix("/instance}").Subrouter()
	apiWorkflowInstanceRouter.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {

		if instances, err := self.WorkflowService.GetAll(); err != nil {
			err = errors.WithMessagef(err, "Could not get all workflow services")
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
			return
		} else {
			writeSuccessLogs("Return all workflow services", self.Logger)
			prepareJsonResponse(w, 200)
			json.NewEncoder(w).Encode(instances)
		}
	}).Methods("GET")

	apiWorkflowInstanceRouter.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {

		var params struct {
			Source string
			Name   *string
			Inputs model.Facts
		}
		if err := json.NewDecoder(req.Body).Decode(&params); err != nil {
			err = errors.WithMessagef(err, "Could not decode params from request body")
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
			return
		}
		if params.Name != nil {
			if err := self.WorkflowService.Start(params.Source, *params.Name, params.Inputs); err != nil {
				err = errors.WithMessagef(err, "Could not start workflow service")
				writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
				return
			}
		} else {
			if wfNames, err := self.EvaluationService.ListWorkflows(params.Source); err != nil {
				err = errors.WithMessagef(err, "Could not list workflows")
				writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
				return
			} else {
				for _, name := range wfNames {
					if err := self.WorkflowService.Start(params.Source, name, params.Inputs); err != nil {

						err = errors.WithMessagef(err, "Could not start workflow service")
						writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
						return
					}
				}
			}
		}

		writeSuccessLogs("Started workflow service successfully", self.Logger)
	}).Methods("POST")

	apiWorkflowInstanceRouter.HandleFunc("/{id}", func(w http.ResponseWriter, req *http.Request) {
		vars := mux.Vars(req)
		id, ok := vars["id"]

		if !ok {
			err := errors.New("Could not read empty id")
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
			return
		}

		if id, err := strconv.ParseUint(id, 10, 64); err != nil {
			err = errors.WithMessagef(err, "Could not parse id", id)
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
			return
		} else if instance, err := self.WorkflowService.GetById(id); err != nil {
			err = errors.WithMessagef(err, "Could not get workflow service by id", id)
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
			return
		} else {
			writeSuccessLogs("Return workflow service", self.Logger)
			prepareJsonResponse(w, 200)
			json.NewEncoder(w).Encode(instance)
		}

	}).Methods("GET")

	apiWorkflowInstanceRouter.HandleFunc("/{id}/fact", func(w http.ResponseWriter, req *http.Request) {

		vars := mux.Vars(req)
		id, ok := vars["id"]

		if !ok {
			err := errors.New("Could not read empty id")
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
			return
		}

		if id, err := strconv.ParseUint(id, 10, 64); err != nil {
			err = errors.WithMessagef(err, "Could not parse id", id)
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
			return
		} else if instance, err := self.WorkflowService.GetById(id); err != nil {
			err = errors.WithMessagef(err, "Could not get workflow service by id", id)
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
			return
		} else {

			facts := model.Facts{}
			if err := json.NewDecoder(req.Body).Decode(&facts); err != nil {
				err = errors.WithMessage(err, "Could not unmarshal facts from request body")
				writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
				return
			}
			if err := self.MessageQueueService.Publish(
				model.FactStreamName.Fmt(instance.Name, instance.ID),
				model.FactStreamName,
				facts,
			); err != nil {

				err = errors.WithMessage(err, "Could not publish fact")
				writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
				return
			}
			writeSuccessLogs("Published fact successfully", self.Logger)
			w.WriteHeader(204)
		}
	}).Methods("POST")

	apiActionRouter := router.PathPrefix("/api/action").Subrouter()
	apiActionRouter.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {

		if actions, err := self.ActionService.GetAll(); err != nil {
			err = errors.WithMessage(err, "Could not get all action services")
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
			return
		} else {
			writeSuccessLogs("Return all action services", self.Logger)
			prepareJsonResponse(w, 200)
			json.NewEncoder(w).Encode(actions)
		}
	}).Methods("GET")

	apiActionRouter.HandleFunc("{id}/", func(w http.ResponseWriter, req *http.Request) {

		vars := mux.Vars(req)
		id, ok := vars["id"]
		if !ok {
			err := errors.New("Could not read empty id")
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
			return
		}

		if id, err := uuid.Parse(id); err != nil {
			err = errors.WithMessagef(err, "Could not parse id", id)
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
			return
		} else if action, err := self.ActionService.GetById(id); err != nil {
			err = errors.WithMessagef(err, "Could not get action service by id", id)
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
			return
		} else {
			writeSuccessLogs("Return action service", self.Logger)
			prepareJsonResponse(w, 200)
			json.NewEncoder(w).Encode(action)
		}
	}).Methods("GET")

	apiActionRouter.HandleFunc("{id}/logs", func(w http.ResponseWriter, req *http.Request) {

		vars := mux.Vars(req)
		id, ok := vars["id"]
		if !ok {
			err := errors.New("Could not read empty id")
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
			return
		}

		if id, err := uuid.Parse(id); err != nil {
			err = errors.WithMessagef(err, "Could not parse id", id)
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
			return
		} else {
			logs, err := self.ActionService.JobLogs(id)
			if err != nil {
				err = errors.WithMessagef(err, "Could not get job logs for action service by id", id)
				writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
				return
			}

			writeSuccessLogs("Return job logs", self.Logger)
			prepareJsonResponse(w, 200)
			json.NewEncoder(w).Encode(map[string]*service.LokiOutput{"logs": logs})
		}
	}).Methods("GET")

	router.HandleFunc("/*route", func(w http.ResponseWriter, req *http.Request) {
		vars := mux.Vars(req)
		route, ok := vars["route"]
		if !ok {
			err := errors.New("Could not read empty route")
			writeHttpErrorAndLogs(w, err, http.StatusInternalServerError, self.Logger)
			return
		}
		if mimeType := mime.TypeByExtension(path.Ext(route)); mimeType != "" {
			w.Header()["Content-Type"] = []string{mimeType}
		}
		writeSuccessLogs("Return from wildcard route", self.Logger)
		makeViewTemplate(route).Execute(w, mux.Vars)
	}).Methods("GET")

	router.PathPrefix("/static/").Handler(http.StripPrefix("/static/", http.FileServer(http.Dir("src/component/web/static"))))

	router.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {
		writeSuccessLogs("Return index.html", self.Logger)
		makeViewTemplate("index.html").Execute(w, nil)
	}).Methods("GET")

	server := &http.Server{Addr: self.Listen, Handler: router}

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

//go:embed templates/* templates/**/*
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
	root := "templates"

	t := template.New("")
	t.Funcs(template.FuncMap{
		"buildInfo": func() interface{} {
			return model.BuildInfo
		},
		"route": func() string { return route },
		"toJson": func(o interface{}, pretty bool) string {
			var enc []byte
			if pretty {
				enc, _ = json.MarshalIndent(o, "", "\t")
			} else {
				enc, _ = json.Marshal(o)
			}
			return string(enc)
		},
		"pathEscape": url.PathEscape,
		"timeUnixNano": func(ns int64) time.Time {
			return time.Unix(
				ns/int64(time.Second),
				ns%int64(time.Second),
			)
		},
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
	// TODO Implement different Log Levels (Info, Warning, Error etc.)
	http.Error(w, err.Error(), httpStatusCode)
	logger.Println(err.Error())
}

func prepareJsonResponse(w http.ResponseWriter, httpStatus int) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(httpStatus)
}

func writeSuccessLogs(msg string, logger *log.Logger) {
	// TODO Implement different Log Levels (Info, Warning, Error etc.)
	logger.Println(msg)
}
