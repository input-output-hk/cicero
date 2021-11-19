package web

import (
	"context"
	"embed"
	"encoding/json"
	"github.com/input-output-hk/cicero/src/application"
	"github.com/input-output-hk/cicero/src/domain"
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
	"github.com/pkg/errors"
	"github.com/uptrace/bunrouter"
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

			if name == "" {
				if summary, err := self.WorkflowService.GetSummary(); err != nil {
					return err
				} else {
					return makeViewTemplate("workflow").Execute(w, summary)
				}
			} else {
				if instances, err := self.WorkflowService.GetAllByName(name); err != nil {
					return err
				} else {
					return makeViewTemplate("workflow/index-name.html").Execute(w, map[string]interface{}{
						"Name":      name,
						"Instances": instances,
					})
				}
			}
		})

		group.GET("/new", func(w http.ResponseWriter, req bunrouter.Request) error {
			const templateName = "workflow/new.html"

			source := req.URL.Query().Get("source")
			name := req.URL.Query().Get("name")

			// step 1
			if source == "" {
				return makeViewTemplate(templateName).Execute(w, map[string]interface{}{})
			}

			// step 4
			if inputsJson := req.URL.Query().Get("inputs"); len(inputsJson) > 0 {
				var inputs domain.Facts
				if err := json.Unmarshal([]byte(inputsJson), &inputs); err != nil {
					return err
				}
				if err := self.WorkflowService.Start(source, name, inputs); err != nil {
					return err
				}
				http.Redirect(w, req.Request, "/workflow", 302)
				return nil
			}

			// step 3
			if len(name) > 0 {
				return makeViewTemplate(templateName).Execute(w, map[string]interface{}{
					"Source": source,
					"Name":   name,
				})
			}

			// step 2
			if names, err := self.EvaluationService.ListWorkflows(source); err != nil {
				return err
			} else {
				return makeViewTemplate(templateName).Execute(w, map[string]interface{}{
					"Source": source,
					"Names":  names,
				})
			}
		})

		group.POST("/", func(w http.ResponseWriter, req bunrouter.Request) error {
			name := req.PostFormValue("name")
			source := req.PostFormValue("source")

			if err := self.WorkflowService.Start(source, name, domain.Facts{}); err != nil {
				return errors.WithMessagef(err, "Could not start workflow \"%s\" from source \"%s\"", name, source)
			}

			http.Redirect(w, req.Request, "/workflow", 302)
			return nil
		})

		group.WithGroup("/:id", func(group *bunrouter.Group) {
			group.GET("/", func(w http.ResponseWriter, req bunrouter.Request) error {
				if id, err := strconv.ParseUint(req.Param("id"), 10, 64); err != nil {
					return err
				} else if instance, err := self.WorkflowService.GetById(id); err != nil {
					return err
				} else {
					allocs, err := self.NomadEventService.GetEventAllocByWorkflowId(id)
					if err != nil {
						return err
					}
					return makeViewTemplate("workflow/[id].html").Execute(w, map[string]interface{}{
						"Instance":   instance,
						"graph":      req.URL.Query().Get("graph"),
						"graphTypes": WorkflowGraphTypeStrings(),
						"allocs":     allocs,
					})
				}
			})

			group.GET("/graph", func(w http.ResponseWriter, req bunrouter.Request) error {
				id, err := strconv.ParseUint(req.Param("id"), 10, 64)
				if err != nil {
					return err
				}

				instance, err := self.WorkflowService.GetById(id)
				if err != nil {
					return err
				}

				def, err := self.EvaluationService.EvaluateWorkflow(instance.Source, instance.Name, instance.ID, instance.Facts)
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
					self.Logger.Panic("reached code that should be unreachable")
					return nil
				}
			})
		})
	})

	router.WithGroup("/api", func(group *bunrouter.Group) {
		group.WithGroup("/workflow", func(group *bunrouter.Group) {
			group.WithGroup("/definition", func(group *bunrouter.Group) {
				group.GET("/:source", func(w http.ResponseWriter, req bunrouter.Request) error {
					if wfs, err := self.EvaluationService.ListWorkflows(req.Param("source")); err != nil {
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

					var inputs domain.Facts
					if inputsStr := req.URL.Query().Get("inputs"); len(inputsStr) > 0 {
						if err := json.Unmarshal([]byte(inputsStr), &inputs); err != nil {
							return err
						}
					}

					if wf, err := self.EvaluationService.EvaluateWorkflow(req.Param("source"), req.Param("name"), id, inputs); err != nil {
						return err
					} else {
						return bunrouter.JSON(w, wf)
					}
				})
			})
			group.WithGroup("/instance", func(group *bunrouter.Group) {
				group.GET("/", func(w http.ResponseWriter, req bunrouter.Request) error {
					if instances, err := self.WorkflowService.GetAll(); err != nil {
						return err
					} else {
						return bunrouter.JSON(w, instances)
					}
				})

				group.POST("/", func(w http.ResponseWriter, req bunrouter.Request) error {
					var params struct {
						Source string
						Name   *string
						Inputs domain.Facts
					}
					params.Inputs = domain.Facts{}
					if err := json.NewDecoder(req.Body).Decode(&params); err != nil {
						return errors.WithMessage(err, "Could not unmarshal params from request body")
					}

					if params.Name != nil {
						if err := self.WorkflowService.Start(params.Source, *params.Name, params.Inputs); err != nil {
							return err
						}
					} else {
						if wfNames, err := self.EvaluationService.ListWorkflows(params.Source); err != nil {
							return err
						} else {
							for _, name := range wfNames {
								if err := self.WorkflowService.Start(params.Source, name, params.Inputs); err != nil {
									return err
								}
							}
						}
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
							} else if instance, err := self.WorkflowService.GetById(id); err != nil {
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

					group.POST("/fact", func(w http.ResponseWriter, req bunrouter.Request) error {
						instance := req.Context().Value(ctxKeyWorkflowInstance).(domain.WorkflowInstance)

						facts := domain.Facts{}
						if err := json.NewDecoder(req.Body).Decode(&facts); err != nil {
							return errors.WithMessage(err, "Could not unmarshal facts from request body")
						}

						if err := self.MessageQueueService.Publish(
							domain.FactStreamName.Fmt(instance.Name, instance.ID),
							domain.FactStreamName,
							facts,
						); err != nil {
							return errors.WithMessage(err, "Could not publish fact")
						}
						w.WriteHeader(204)
						return nil
					})
				})
			})
		})
		group.WithGroup("/action", func(group *bunrouter.Group) {
			group.GET("/", func(w http.ResponseWriter, req bunrouter.Request) error {
				if actions, err := self.ActionService.GetAll(); err != nil {
					return err
				} else {
					return bunrouter.JSON(w, actions)
				}
			})

			group.WithGroup("/:id", func(group *bunrouter.Group) {
				group.GET("/", func(w http.ResponseWriter, req bunrouter.Request) error {
					if id, err := uuid.Parse(req.Param("id")); err != nil {
						return err
					} else if action, err := self.ActionService.GetById(id); err != nil {
						return err
					} else {
						return bunrouter.JSON(w, action)
					}
				})

				group.GET("/logs", func(w http.ResponseWriter, req bunrouter.Request) error {
					if id, err := uuid.Parse(req.Param("id")); err != nil {
						return err
					} else {
						logs, err := self.ActionService.JobLogs(id)
						if err != nil {
							return err
						}
						return bunrouter.JSON(w, map[string]*application.LokiOutput{"logs": logs})
					}
				})
			})
		})
	})

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
			return domain.BuildInfo
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

type stackTracer interface {
	StackTrace() errors.StackTrace
}

// copy-pasted error handling from bunrouter's homepage

type HTTPError struct {
	statusCode int

	Code    string            `json:"code"`
	Message string            `json:"message"`
	Trace   errors.StackTrace `json:"trace"`
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
