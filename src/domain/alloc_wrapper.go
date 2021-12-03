package domain

import (
	nomad "github.com/hashicorp/nomad/api"
	"time"
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
	Logs  *LokiOutput
}
