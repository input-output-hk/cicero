package model

import "fmt"

type StreamName string

const (
	StartStreamName  StreamName = "workflow.*.start"
	InvokeStreamName StreamName = "workflow.*.*.invoke"
	FactStreamName   StreamName = "workflow.*.*.fact"
)

func (s StreamName) String() string {
	return string(s)
}

func (s StreamName) Fmt(name string, id ...uint64) (result string) {
	switch {
	case s == StartStreamName:
		result = fmt.Sprintf("workflow.%s.start", name)
	case s == InvokeStreamName:
		result = fmt.Sprintf("workflow.%s.%d.invoke", name, id[0])
	case s == FactStreamName:
		result = fmt.Sprintf("workflow.%s.%d.fact", name, id[0])
	}
	return
}
