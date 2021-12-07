package web

import (
	"embed"
	"encoding/json"
	"html/template"
	"log"
	"net/http"
	"net/url"
	"path/filepath"
	"time"

	"github.com/input-output-hk/cicero/src/domain"
)

//go:embed templates
var templatesFs embed.FS

//go:embed static
var staticFs embed.FS

func (self *Web) ServerError(w http.ResponseWriter, err error) {
	http.Error(w, err.Error(), http.StatusInternalServerError)
}

func (self *Web) ClientError(w http.ResponseWriter, err error) {
	http.Error(w, err.Error(), http.StatusPreconditionFailed)
}

func (self *Web) NotFound(w http.ResponseWriter, err error) {
	http.Error(w, err.Error(), http.StatusNotFound)
}

func (self *Web) Error(w http.ResponseWriter, err error, status int) {
	self.Logger.Println(err.Error())
	http.Error(w, err.Error(), status)
}

func (self *Web) json(w http.ResponseWriter, obj interface{}, status int) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	if err := json.NewEncoder(w).Encode(obj); err != nil {
		self.ServerError(w, err)
		return
	}
}

var layout *template.Template
var templates = map[string]*template.Template{}

func init() {
	tmpl, err := templatesFs.ReadFile("templates/layout.html")
	if err != nil {
		log.Panic(err)
	}
	layout = template.Must(template.New("layout.html").Funcs(templateFuncs).Parse(string(tmpl)))
}

func loadTemplate(route string) (*template.Template, error) {
	if found, ok := templates[route]; ok {
		return found, nil
	}

	clone := template.Must(layout.Clone())
	source, err := templatesFs.ReadFile(filepath.Join("templates", route))
	if err != nil {
		return nil, err
	}

	parsed := template.Must(clone.New(route).Parse(string(source)))
	templates[route] = parsed

	return parsed, nil
}

func render(route string, w http.ResponseWriter, data interface{}) error {
	tmpl, err := loadTemplate(route)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return err
	}

	if err := tmpl.Execute(w, data); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return err
	}

	return nil
}

var templateFuncs = template.FuncMap{
	"buildInfo": func() struct {
		Version string
		Commit  string
	} {
		return domain.BuildInfo
	},
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
}
