package cicero

import (
	"context"
	"embed"
	"encoding/json"
	"fmt"
	"github.com/google/uuid"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/input-output-hk/cicero/src/service"
	"github.com/liftbridge-io/go-liftbridge"
	"github.com/pkg/errors"
	"github.com/uptrace/bunrouter"
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
)

type WebCmd struct {
	Addr      string `arg:"--listen" default:":8080"`
	logger    *log.Logger
	bridge    liftbridge.Client
	workflowService service.WorkflowService
	actionService service.ActionService
	evaluator Evaluator
}

func (cmd *WebCmd) init() {
	if cmd.logger == nil {
		cmd.logger = log.New(os.Stderr, "web: ", log.LstdFlags)
	}
	if cmd.workflowService == nil {
		wfService := &service.WorkflowServiceCmd{}
		wfService.Init(DB)
		cmd.workflowService = wfService
	}
	if cmd.actionService == nil {
		aService := &service.ActionServiceCmd{}
		aService.Init(DB)
		cmd.actionService = aService
	}
}

func (cmd *WebCmd) Run() error {
	cmd.init()
	return cmd.start(context.Background())
}

func (cmd *WebCmd) start(ctx context.Context) error {
	cmd.init()
	api := Api{
		bridge:    cmd.bridge,
		workflowService: cmd.workflowService,
		evaluator: cmd.evaluator,
	}
	api.init()

	cmd.logger.Println("Starting Web")

	router := bunrouter.New(
		bunrouter.WithMiddleware(errorHandler),
	)

	router.GET("/", func(w http.ResponseWriter, req bunrouter.Request) error {
		return makeViewTemplate("index.html").Execute(w, struct{}{})
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
			wfs, err := api.Workflows()
			if err != nil {
				return err
			}
			return makeViewTemplate("workflow").Execute(w, wfs)
		})

		group.GET("/:name", func(w http.ResponseWriter, req bunrouter.Request) error {
			name := req.Param("name")

			instances, err := cmd.workflowService.GetAllByName(name)
			if err != nil {
				return err
			}

			return makeViewTemplate("workflow/[name].html").Execute(w, map[string]interface{}{
				"Name":       name,
				"Instances":  instances,
				"instance":   req.URL.Query().Get("instance"),
				"graph":      req.URL.Query().Get("graph"),
				"graphTypes": WorkflowGraphTypeStrings(),
			})
		})

		group.GET("/:name/start", func(w http.ResponseWriter, req bunrouter.Request) error {
			name := req.Param("name")
			if err := api.WorkflowStart(name); err != nil {
				return err
			}
			http.Redirect(w, req.Request, "/workflow/"+name, 302)
			return nil
		})

		group.GET("/:name/graph", func(w http.ResponseWriter, req bunrouter.Request) error {
			name := req.Param("name")
			instanceStr := req.URL.Query().Get("instance")
			graphTypeStr := req.URL.Query().Get("type")

			var instanceId *uint64
			if len(instanceStr) > 0 {
				iid, err := strconv.ParseUint(instanceStr, 10, 64)
				if err != nil {
					return err
				}
				instanceId = &iid
			}
			def, err := api.WorkflowForInstance(name, instanceId, cmd.logger)
			if err != nil {
				return err
			}

			var graphType WorkflowGraphType
			if len(graphTypeStr) > 0 {
				var err error
				graphType, err = WorkflowGraphTypeFromString(graphTypeStr)
				if err != nil {
					return err
				}
			}

			return RenderWorkflowGraph(&def, graphType, w)
		})
	})

	router.WithGroup("/api", func(group *bunrouter.Group) {
		group.WithGroup("/workflow", func(group *bunrouter.Group) {
			group.GET("/", func(w http.ResponseWriter, req bunrouter.Request) error {
				wfs, err := api.Workflows()
				if err != nil {
					return err
				}
				return bunrouter.JSON(w, wfs)
			})
			group.GET("/:name", func(w http.ResponseWriter, req bunrouter.Request) error {
				actions, err := api.Workflow(req.Param("name"))
				if err != nil {
					return err
				}
				return bunrouter.JSON(w, actions)
			})
		})
		group.WithGroup("/action", func(group *bunrouter.Group) {
			group.GET("/", func(w http.ResponseWriter, req bunrouter.Request) error {
				actions, err := cmd.actionService.GetAll()
				if err != nil {
					return err
				}
				return bunrouter.JSON(w, actions)
			})
			group.WithGroup("/:id", func(group *bunrouter.Group) {
				const (
					ctxKeyAction = iota
				)
				group = group.WithMiddleware(func(next bunrouter.HandlerFunc) bunrouter.HandlerFunc {
					return func(w http.ResponseWriter, req bunrouter.Request) error {
						id, err := uuid.Parse(req.Param("id"))
						if err != nil {
							return err
						}

						action, err := cmd.actionService.GetById(id)
						if err != nil {
							return err
						}

						return next(w, req.WithContext(
							context.WithValue(req.Context(), ctxKeyAction, *action),
						))
					}
				})
				group.GET("/", func(w http.ResponseWriter, req bunrouter.Request) error {
					return bunrouter.JSON(w, req.Context().Value(ctxKeyAction))
				})
				group.POST("/cert", func(w http.ResponseWriter, req bunrouter.Request) error {
					action := req.Context().Value(ctxKeyAction).(model.ActionInstance)
					wf, err := cmd.workflowService.GetById(action.WorkflowInstanceId)
					if err != nil {
						errors.WithMessagef(err, "Could not get workflow instance for action %s", action.ID)
						return err
					}

					certs := model.WorkflowCerts{}
					if err := json.NewDecoder(req.Body).Decode(&certs); err != nil {
						return errors.WithMessage(err, "Could not unmarshal certs from request body")
					}

					if err := service.Publish(
						cmd.logger,
						cmd.bridge,
						fmt.Sprintf("workflow.%s.%d.cert", wf.Name, wf.ID),
						"workflow.*.*.cert",
						certs,
					); err != nil {
						return errors.WithMessage(err, "Could not publish certificate")
					}
					return nil
				})
			})
		})
	})

	server := &http.Server{Addr: cmd.Addr, Handler: router}

	go func() {
		if err := server.ListenAndServe(); err != nil {
			cmd.logger.Printf("Failed to start web server: %s\n", err.Error())
		}
	}()

	<-ctx.Done()

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	if err := server.Shutdown(ctx); err != nil {
		cmd.logger.Printf("Failed to stop web server: %s\n", err.Error())
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