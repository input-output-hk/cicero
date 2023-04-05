package web

import (
	"embed"
	"encoding/json"
	"fmt"
	"html/template"
	"log"
	"net/http"
	"net/url"
	"path/filepath"
	"reflect"
	"time"

	"github.com/input-output-hk/cicero/src/domain"
)

//go:embed templates
var templatesFs embed.FS

//go:embed static
var staticFs embed.FS

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

func render(route string, w http.ResponseWriter, data any) error {
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
	"buildInfo": func() domain.BuildInfo {
		return domain.Build
	},
	"toJson": func(o any, pretty bool) string {
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
	"timeNow": time.Now,
	"timeZone": func(t time.Time) string {
		name, offset := t.Zone()
		return fmt.Sprintf(
			"%s (%+03d:%02d:%02d)",
			name,
			offset/60/60,
			offset/60%60,
			offset%60,
		)
	},
	"derefInt": func(ptr *int) int {
		return *ptr
	},
	"derefString": func(ptr *string) string {
		return *ptr
	},
	"addInt": func(a int, b int) int {
		return a + b
	},
	"subInt": func(a int, b int) int {
		return a - b
	},
	"maxInt": func(a int, b int) int {
		if b > a {
			return b
		}
		return a
	},
	"hasKey": func(m any, key interface{}) bool {
		for _, k := range reflect.ValueOf(m).MapKeys() {
			if k.Interface() == key {
				return true
			}
		}
		return false
	},
}
