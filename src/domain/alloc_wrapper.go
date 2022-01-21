package domain

import (
	"time"

	nomad "github.com/hashicorp/nomad/api"
)

type LokiLine struct {
	Time time.Time
	Text string
}

type LokiOutput struct {
	Stderr []LokiLine
	Stdout []LokiLine
}

type AllocWrapper struct {
	Alloc *nomad.Allocation
	Logs  map[string]*LokiOutput
}
