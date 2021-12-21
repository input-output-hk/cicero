package domain

import "fmt"

type StreamName string

const (
	ActionCreateStreamName StreamName = "action.*.create"
	ActionInvokeStreamName StreamName = "action.*.invoke"
	FactCreateStreamName   StreamName = "fact.create"
)

func (s StreamName) String() string {
	return string(s)
}

func ActionCreateStream(action string) string {
	return fmt.Sprintf("action.%s.create", action)
}

func ActionInvokeStream(action string) string {
	return fmt.Sprintf("action.%s.invoke", action)
}
