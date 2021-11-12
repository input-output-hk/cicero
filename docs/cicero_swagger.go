package docs

import (
	cicero "github.com/input-output-hk/cicero/src/model"
)

// swagger:route GET /api/workflow workflow
// List all workflows

// responses:
//   default: GenericError
//   200: WorkflowDefinitions

// swagger:route GET /api/workflow/{name} workflow GetWorkflow
// Evaluate a workflow instance with certificates.
// responses:
//   default: GenericError
//   200: WorkflowDefinition

// swagger:route GET /api/action/ action
// List all Actions
// responses:
//   default: GenericError
//   200: ActionInstances

// swagger:route GET /api/action/{id} action GetActionId
// Search a specific ActionInstance by ID
// responses:
//   default: GenericError
//   200: ActionInstance

// swagger:route GET /api/action/{id}/ action GetActionId
// ???
// responses:
//   default: GenericError

// swagger:route POST /api/action/{id}/cert action PostWorkflowCerts
// Publish new certificate

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

// swagger:parameters GetActionId
type _ struct {
	// The id of an Action
	// in:path
	String string `json:"id"`
}

// This text will appear as description of your response body.
// swagger:response ActionItem
type ActionItemWrapper struct {
	// in:body
	Body cicero.ActionInstance
}

// This text will appear as description of your response body.
// swagger:response ActionItems
type ActionItemsWrapper struct {
	// in:body
	Body cicero.ActionInstances
}

// swagger:parameters PostWorkflowCerts
type _ struct {
	// An certificate
	// in:body
	Body cicero.WorkflowCerts
}

// swagger:response GenericError
type _ struct {
	// A generic error
	// in:body
	Body cicero.GenericError
}
