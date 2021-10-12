package main

import (
	"embed"
	"encoding/base64"
	"encoding/json"
	"html/template"
	"math/rand"
	"mime"
	"net/http"
	"path"
	"strings"
	"time"

	"github.com/go-echarts/go-echarts/v2/charts"
	"github.com/go-echarts/go-echarts/v2/components"
	"github.com/go-echarts/go-echarts/v2/opts"
	"github.com/uptrace/bunrouter"
)

type WebCmd struct {
	Addr string `arg:"--listen" default:":80"`
}

func runWeb(args *WebCmd) error {
	return web(args)
}

func web(args *WebCmd) error {
	router := bunrouter.New(
		bunrouter.WithMiddleware(errorHandler),
	)

	router.GET("/", func(w http.ResponseWriter, req bunrouter.Request) error {
		return makeViewTemplate("index.html").Execute(w, struct{}{})
	})

	router.GET("/*route", func(w http.ResponseWriter, req bunrouter.Request) error {
		route := req.Params().ByName("route")
		if mime := mime.TypeByExtension(path.Ext(route)); mime != "" {
			w.Header()["Content-Type"] = []string{mime}
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
			name := req.Params().ByName("name")

			instances, err := api.WorkflowInstances(name)
			if err != nil {
				return err
			}

			return makeViewTemplate("workflow/[name].html").Execute(w, map[string]interface{}{
				"Name":      name,
				"Instances": instances,
			})
		})

		group.GET("/:name/start", func(w http.ResponseWriter, req bunrouter.Request) error {
			name := req.Params().ByName("name")
			if err := api.WorkflowStart(name); err != nil {
				return err
			}
			http.Redirect(w, req.Request, "/workflow/"+name, 302)
			return nil
		})

		group.GET("/:name/graph", func(w http.ResponseWriter, req bunrouter.Request) error {
			wf, err := api.Workflow(req.Params().ByName("name"))
			if err != nil {
				return err
			}

			// XXX nodes := make([]opts.GraphNode, len(*tasks))
			nodes := make([]opts.GraphNode, 0)
			for name, task := range wf.Tasks {
				graphNode := opts.GraphNode{
					Name:       name,
					Symbol:     "circle",
					SymbolSize: 25,
				}
				if task.Run != nil {
					graphNode.Symbol = "triangle"
					graphNode.Category = 0
					graphNode.Y = 10
					graphNode.SymbolSize = 25 * 1.5
				}

				nodes = append(nodes, graphNode)
			}

			links := make([]opts.GraphLink, 0)
			for name, task := range wf.Tasks {
				for _, input := range task.Inputs {
					for name2, task2 := range wf.Tasks {
						if name == name2 {
							continue
						}
						for _, input2 := range task2.Inputs {
							if input != input2 {
								continue
							}
							links = append(links, opts.GraphLink{
								Source: name,
								Target: name2,
							})
						}
					}
				}
			}

			graph := charts.NewGraph()
			graph.SetGlobalOptions(
				charts.WithInitializationOpts(opts.Initialization{Width: "100%", Height: "100%"}),
				charts.WithToolboxOpts(opts.Toolbox{Show: true}),
			)
			graph.AddSeries("tasks", nodes, links,
				charts.WithGraphChartOpts(
					opts.GraphChart{
						Force:              &opts.GraphForce{Repulsion: 1000},
						Roam:               true,
						FocusNodeAdjacency: true,
						Categories: []*opts.GraphCategory{
							{
								Name: "",
								Label: &opts.Label{
									Show: true,
								},
							},
						},
					},
				),
			)

			page := components.NewPage()
			page.AddCharts(
				graph,
			)

			return page.Render(w)
		})
	})

	router.WithGroup("/api", func(group *bunrouter.Group) {
		group.WithGroup("/workflow", func(group *bunrouter.Group) {
			group.GET("/:name", func(w http.ResponseWriter, req bunrouter.Request) error {
				tasks, err := api.Workflow(req.Params().ByName("name"))
				if err != nil {
					return err
				}
				return bunrouter.JSON(w, tasks)
			})
		})
	})

	return http.ListenAndServe(args.Addr, router)
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
	{
		// XXX use a stable identifier instead
		scope := make([]byte, 20)
		rand.Seed(time.Now().UnixNano())
		for i := 0; i < len(scope); i++ {
			scope[i] = byte(rand.Intn(26))
		}

		t.Funcs(template.FuncMap{
			"route": func() string { return route },
			"scope": func() string {
				return base64.RawURLEncoding.EncodeToString(scope)
			},
			"toJson": func(o interface{}) string {
				enc, _ := json.Marshal(o)
				return string(enc)
			},
		})
	}

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

type HTTPError struct {
	statusCode int

	Code    string `json:"code"`
	Message string `json:"message"`
}

func (e HTTPError) Error() string {
	return e.Message
}

func NewHTTPError(err error) HTTPError {
	return HTTPError{
		statusCode: http.StatusInternalServerError,

		Code:    "internal",
		Message: err.Error(),
	}
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
