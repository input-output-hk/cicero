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

	"github.com/google/uuid"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/input-output-hk/cicero/src/service"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pkg/errors"
	"github.com/uptrace/bunrouter"
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
	if web.bridge == nil {
		bridge, err := service.LiftbridgeConnect(self.LiftbridgeAddr)
		if err != nil {
			web.logger.Fatalln(err.Error())
			return
		}
		web.bridge = &bridge
	}
	if web.workflowService == nil {
		s := service.NewWorkflowService(DB, *web.bridge)
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
	Listen          *string
	logger          *log.Logger
	bridge          *liftbridge.Client
	workflowService *service.WorkflowService
	actionService   *service.ActionService
	evaluator       *Evaluator
}

func (self *Web) start(ctx context.Context) error {
	self.logger.Println("Starting Web")

	router := bunrouter.New(
		bunrouter.WithMiddleware(errorHandler),
	)

	router.GET("/", func(w http.ResponseWriter, req bunrouter.Request) error {
		return makeViewTemplate("index.html").Execute(w, nil)
	})

	router.GET("/*route", func(w http.ResponseWriter, req bunrouter.Request) error {
		route := req.Param("route")
		if mimeType := mime.TypeByExtension(path.Ext(route)); mimeType != "" {
			w.Header()["Content-Type"] = []string{mimeType}
		}
		return makeViewTemplate(route).Execute(w, req.Params().Map())
	})

	router.WithGroup("/workflow", func(group *bunrouter.Group) {
		group.GET("/", func(w http.ResponseWriter, req bunrouter.Request) error {
			name := req.URL.Query().Get("name")

			var templateName string
			var instances []*model.WorkflowInstance

			if len(name) == 0 {
				templateName = "workflow"
				if insts, err := (*self.workflowService).GetAll(); err != nil {
					return err
				} else {
					instances = insts
				}
			} else {
				templateName = "workflow/index-name.html"
				if insts, err := (*self.workflowService).GetAllByName(name); err != nil {
					return err
				} else {
					instances = insts
				}
			}

			return makeViewTemplate(templateName).Execute(w, instances)
		})

		group.POST("/", func(w http.ResponseWriter, req bunrouter.Request) error {
			name := req.PostFormValue("name")
			source := req.PostFormValue("source")

			if err := (*self.workflowService).Start(source, name); err != nil {
				return errors.WithMessagef(err, "Could not start workflow \"%s\" from source \"%s\"", name, source)
			}

			http.Redirect(w, req.Request, "/workflow", 302)
			return nil
		})

		group.WithGroup("/:id", func(group *bunrouter.Group) {
			group.GET("/", func(w http.ResponseWriter, req bunrouter.Request) error {
				if id, err := strconv.ParseUint(req.Param("id"), 10, 64); err != nil {
					return err
				} else if instance, err := (*self.workflowService).GetById(id); err != nil {
					return err
				} else {
					return makeViewTemplate("workflow/[id].html").Execute(w, map[string]interface{}{
						"Instance":   instance,
						"graph":      req.URL.Query().Get("graph"),
						"graphTypes": WorkflowGraphTypeStrings(),
					})
				}
			})

			group.GET("/graph", func(w http.ResponseWriter, req bunrouter.Request) error {
				id, err := strconv.ParseUint(req.Param("id"), 10, 64)
				if err != nil {
					return err
				}

				instance, err := (*self.workflowService).GetById(id)
				if err != nil {
					return err
				}

				def, err := self.evaluator.EvaluateWorkflow(instance.Source, instance.Name, instance.ID, instance.Certs)
				if err != nil {
					return err
				}

				var graphType WorkflowGraphType
				graphTypeStr := req.URL.Query().Get("type")
				if len(graphTypeStr) > 0 {
					if gt, err := WorkflowGraphTypeFromString(graphTypeStr); err != nil {
						return err
					} else {
						graphType = gt
					}
				}

				switch graphType {
				case WorkflowGraphTypeFlow:
					return RenderWorkflowGraphFlow(def, w)
				case WorkflowGraphTypeInputs:
					return RenderWorkflowGraphInputs(def, &instance, w)
				default:
					// should have already exited when parsing the graph type
					self.logger.Panic("reached code that should be unreachable")
					return nil
				}
			})
		})
	})

	router.WithGroup("/api", func(group *bunrouter.Group) {
		group.WithGroup("/workflow", func(group *bunrouter.Group) {
			group.WithGroup("/definition", func(group *bunrouter.Group) {
				group.GET("/:source", func(w http.ResponseWriter, req bunrouter.Request) error {
					if wfs, err := self.evaluator.ListWorkflows(req.Param("source")); err != nil {
						return err
					} else {
						return bunrouter.JSON(w, wfs)
					}
				})

				group.GET("/:source/:name", func(w http.ResponseWriter, req bunrouter.Request) error {
					var id uint64
					if idStr := req.URL.Query().Get("id"); len(idStr) > 0 {
						if iid, err := strconv.ParseUint(idStr, 10, 64); err != nil {
							return err
						} else {
							id = iid
						}
					}

					var inputs model.WorkflowCerts
					if inputsStr := req.URL.Query().Get("inputs"); len(inputsStr) > 0 {
						if err := json.Unmarshal([]byte(inputsStr), &inputs); err != nil {
							return err
						}
					}

					if wf, err := self.evaluator.EvaluateWorkflow(req.Param("source"), req.Param("name"), id, inputs); err != nil {
						return err
					} else {
						return bunrouter.JSON(w, wf)
					}
				})
			})
			group.WithGroup("/instance", func(group *bunrouter.Group) {
				group.GET("/", func(w http.ResponseWriter, req bunrouter.Request) error {
					if instances, err := (*self.workflowService).GetAll(); err != nil {
						return err
					} else {
						return bunrouter.JSON(w, instances)
					}
				})

				group.POST("/", func(w http.ResponseWriter, req bunrouter.Request) error {
					var params struct {
						Source string
						Name   string
					}
					if err := json.NewDecoder(req.Body).Decode(&params); err != nil {
						return errors.WithMessage(err, "Could not unmarshal params from request body")
					}
					if err := (*self.workflowService).Start(params.Source, params.Name); err != nil {
						return err
					}
					w.WriteHeader(204)
					return nil
				})

				group.WithGroup("/:id", func(group *bunrouter.Group) {
					const (
						ctxKeyWorkflowInstance = iota
					)

					group = group.WithMiddleware(func(next bunrouter.HandlerFunc) bunrouter.HandlerFunc {
						return func(w http.ResponseWriter, req bunrouter.Request) error {
							if id, err := strconv.ParseUint(req.Param("id"), 10, 64); err != nil {
								return err
							} else if instance, err := (*self.workflowService).GetById(id); err != nil {
								return err
							} else {
								return next(w, req.WithContext(
									context.WithValue(req.Context(), ctxKeyWorkflowInstance, instance),
								))
							}
						}
					})

					group.GET("/", func(w http.ResponseWriter, req bunrouter.Request) error {
						return bunrouter.JSON(w, req.Context().Value(ctxKeyWorkflowInstance))
					})

					group.POST("/cert", func(w http.ResponseWriter, req bunrouter.Request) error {
						instance := req.Context().Value(ctxKeyWorkflowInstance).(model.WorkflowInstance)

						certs := model.WorkflowCerts{}
						if err := json.NewDecoder(req.Body).Decode(&certs); err != nil {
							return errors.WithMessage(err, "Could not unmarshal certs from request body")
						}

						if err := service.Publish(
							self.logger,
							*self.bridge,
							fmt.Sprintf("workflow.%s.%d.cert", instance.Name, instance.ID),
							service.CertStreamName,
							certs,
						); err != nil {
							return errors.WithMessage(err, "Could not publish certificate")
						}
						w.WriteHeader(204)
						return nil
					})
				})
			})
		})
		group.WithGroup("/action", func(group *bunrouter.Group) {
			group.GET("/", func(w http.ResponseWriter, req bunrouter.Request) error {
				if actions, err := (*self.actionService).GetAll(); err != nil {
					return err
				} else {
					return bunrouter.JSON(w, actions)
				}
			})

			group.WithGroup("/:id", func(group *bunrouter.Group) {
				group.GET("/", func(w http.ResponseWriter, req bunrouter.Request) error {
					if id, err := uuid.Parse(req.Param("id")); err != nil {
						return err
					} else if action, err := (*self.actionService).GetById(id); err != nil {
						return err
					} else {
						return bunrouter.JSON(w, action)
					}
				})

				group.GET("/logs", func(w http.ResponseWriter, req bunrouter.Request) error {
					if id, err := uuid.Parse(req.Param("id")); err != nil {
						return err
					} else {
						stdout, stderr, err := (*self.actionService).Logs(id)
						if err != nil {
							return err
						}
						return bunrouter.JSON(w, map[string][]string{
							"stdout": stdout,
							"stderr": stderr,
						})
					}
				})
			})
		})
	})

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

type stackTracer interface {
	StackTrace() errors.StackTrace
}

// copy-pasted error handling from bunrouter's homepage

type HTTPError struct {
	statusCode int

	Code    string            `json:"code"`
	Message string            `json:"message"`
	Trace   errors.StackTrace `json:trace`
}

func (e HTTPError) Error() string {
	return e.Message
}

func NewHTTPError(err error) HTTPError {
	httpErr := HTTPError{
		statusCode: http.StatusInternalServerError,

		Code:    "internal",
		Message: err.Error(),
	}

	if serr, ok := err.(stackTracer); ok {
		httpErr.Trace = serr.StackTrace()
	}

	return httpErr
}

func errorHandler(next bunrouter.HandlerFunc) bunrouter.HandlerFunc {
	return func(w http.ResponseWriter, req bunrouter.Request) error {
		// Call the next handler on the chain to get the error.
		err := next(w, req)

		switch err := err.(type) {
		case nil:
			// no error
		case HTTPError: // already a HTTPError
			w.WriteHeader(err.statusCode)
			_ = bunrouter.JSON(w, err)
		default:
			httpErr := NewHTTPError(err)
			w.WriteHeader(httpErr.statusCode)
			_ = bunrouter.JSON(w, httpErr)
		}

		return err // return the err in case there other middlewares
	}
}
