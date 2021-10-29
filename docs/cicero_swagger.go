package docs

import (
	cicero "github.com/input-output-hk/cicero/src/model"
)

// swagger:route GET /api/workflow workflow
// Build a new workflow instance.
// responses:
//   200: WorkflowDefinitions

// swagger:route GET /api/workflow/{name} workflow GetWorkflow
// Build a new workflow instance with certificates.
// responses:
//   200: WorkflowDefinition

// swagger:parameters GetWorkflow
type _ struct {
	// The name of a workflow
	// in:path
	String string `json:"name"`
}

// This text will appear as description of your response body.
// swagger:response WorkflowDefinition
type WorkflowDefinitionWrapper struct {
	// in:body
	Body cicero.WorkflowDefinition
}

// This text will appear as description of your response body.
// swagger:response WorkflowDefinitions
type WorkflowDefinitionsWrapper struct {
	// in:body
	Body cicero.WorkflowDefinitions
}