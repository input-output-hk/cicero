package domain

import (
	"time"

	nomad "github.com/hashicorp/nomad/api"
)

type LokiLine struct {
	Time   time.Time
	Source string
	Text   string
}

type LokiLog []LokiLine

type AllocationWithLogs struct {
	*nomad.Allocation
	Logs map[string]LokiLog
}
