package domain

import (
	"time"

	nomad "github.com/hashicorp/nomad/api"
)

type LokiLine struct {
	Time time.Time
	Text string
}

type LokiLog struct {
	Stderr []LokiLine
	Stdout []LokiLine
}

type AllocationWithLogs struct {
	*nomad.Allocation
	Logs map[string]*LokiLog
}
