package apidoc

import (
	"context"
	"net/http"

	swagger "github.com/davidebianchi/gswagger"
	"github.com/davidebianchi/gswagger/apirouter"
	"github.com/getkin/kin-openapi/openapi3"
)

func NewRouterDocumented(router apirouter.Router, title string, version string, ctx context.Context) (*swagger.Router, error) {
	return swagger.NewRouter(router, swagger.Options{
		Context: ctx,
		Openapi: &openapi3.T{
			Info: &openapi3.Info{
				Title:   title,
				Version: version,
			},
		},
	})
}

type PathParams struct {
	Name        string
	Description string
	Value       interface{}
}

func BuildSwaggerPathParams(params []PathParams) swagger.ParameterValue {
	result := make(swagger.ParameterValue)
	for _, param := range params {
		result[param.Name] = struct {
			Content     swagger.Content
			Schema      *swagger.Schema
			Description string
		}{
			Schema:      &swagger.Schema{Value: param.Value},
			Description: param.Description,
		}
	}
	return result
}

type Response struct {
	statusCode int
	body       swagger.ContentValue
}

func BuildResponseSuccessfully(statusCode int, context interface{}, description string) Response {
	return Response{
		statusCode: statusCode,
		body: swagger.ContentValue{
			Content: swagger.Content{
				"text/html": {Value: context},
			},
			Description: description,
		},
	}
}

func BuildBodyRequest(body interface{}) *swagger.ContentValue {
	return &swagger.ContentValue{
		Content: swagger.Content{
			"application/json": {
				Value: &body,
			},
		},
	}
}

func BuildSwaggerDef(parameters swagger.ParameterValue, bodyRequest *swagger.ContentValue, response Response) swagger.Definitions {
	return swagger.Definitions{
		PathParams:  parameters,
		RequestBody: bodyRequest,
		Responses: map[int]swagger.ContentValue{
			response.statusCode: response.body,
			http.StatusPreconditionFailed: {
				Content: swagger.Content{
					"application/json": {Value: &errorResponse{}},
				},
				Description: "ClientError",
			},
			http.StatusInternalServerError: {
				Content: swagger.Content{
					"application/json": {Value: &errorResponse{}},
				},
				Description: "ServerError",
			},
		},
	}
}

type errorResponse struct {
	Message string `json:"message"`
}
