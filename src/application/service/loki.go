package service

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"sort"
	"strconv"
	"strings"
	"time"

	"github.com/grafana/loki/pkg/loghttp"
	"github.com/pborman/ansi"
	"github.com/pkg/errors"
	prometheus "github.com/prometheus/client_golang/api"
)

type LokiService interface {
	QueryRangeLog(string, time.Time, *time.Time, string) (LokiLog, error)
	QueryRange(string, time.Time, *time.Time, func(loghttp.Stream) (bool, error)) error
}

type LokiLog []LokiLine

type LokiLine struct {
	Time   time.Time
	Source string
	Text   string
}

type lokiService struct {
	prometheus prometheus.Client
}

func NewLokiService(prometheusClient prometheus.Client) LokiService {
	return &lokiService{prometheusClient}
}

func (self lokiService) QueryRangeLog(query string, start time.Time, end *time.Time, fdLabel string) (LokiLog, error) {
	const linesToFetch = 10000

	log := LokiLog{}

	if err := self.QueryRange(query, start, end, func(stream loghttp.Stream) (bool, error) {
		callbackLog := new(LokiLog)
		callbackLog.FromStream(stream, fdLabel)

		log = append(log, *callbackLog...)

		return len(log) >= linesToFetch, nil
	}); err != nil {
		return nil, err
	}

	log.Sort()

	return log, nil
}

func (self lokiService) QueryRange(query string, start time.Time, end *time.Time, callback func(loghttp.Stream) (bool, error)) error {
	// TODO: figure out the correct value for our infra, 5000 is the default configuration in loki
	const limit int64 = 5000

	if end == nil {
		now := time.Now().UTC()
		end = &now
	}

	endLater := end.Add(1 * time.Minute)
	end = &endLater

done:
	for {
		req, err := http.NewRequest(
			"GET",
			self.prometheus.URL("/loki/api/v1/query_range", nil).String(),
			http.NoBody,
		)
		if err != nil {
			return err
		}

		q := req.URL.Query()
		q.Set("query", query)
		q.Set("limit", strconv.FormatInt(limit, 10))
		q.Set("start", strconv.FormatInt(start.UnixNano(), 10))
		q.Set("end", strconv.FormatInt(end.UnixNano(), 10))
		q.Set("direction", "FORWARD")
		req.URL.RawQuery = q.Encode()

		done, body, err := self.prometheus.Do(context.Background(), req)
		if err != nil {
			return errors.WithMessage(err, "Failed to talk with loki")
		}

		if done.StatusCode/100 != 2 {
			return fmt.Errorf("Error response %d from Loki: %s", done.StatusCode, string(body))
		}

		response := loghttp.QueryResponse{}

		err = json.Unmarshal(body, &response)
		if err != nil {
			return err
		}

		streams, ok := response.Data.Result.(loghttp.Streams)
		if !ok {
			return fmt.Errorf("Unexpected loki result type: %s", response.Data.Result.Type())
		}

		if len(streams) == 0 {
			break done
		}

		var numEntries int64 
		for _, stream := range streams {
			if stop, err := callback(stream); err != nil {
				return err
			} else if stop {
				break done
			}

			numEntries += int64(len(stream.Entries))
			for _, entry := range stream.Entries {
				if entry.Timestamp.After(start) {
					start = entry.Timestamp
				}
			}
		}
		if numEntries < limit {
			break done
		}
	}

	return nil
}

func (self *LokiLog) FromStream(stream loghttp.Stream, fdLabel string) {
	fd, ok := stream.Labels.Map()[fdLabel]
	if !ok {
		return
	}

	for _, entry := range stream.Entries {
		line := LokiLine{Time: entry.Timestamp, Source: fd, Text: entry.Line}
		lines := strings.Split(entry.Line, "\r")
		for _, l := range lines {
			if sane, err := ansi.Strip([]byte(l)); err == nil {
				line.Text = string(sane)
			} else {
				line.Text = l
			}
			*self = append(*self, line)
		}
	}
}

func (self *LokiLog) Sort() {
	sort.Slice(*self, func(i, j int) bool {
		return (*self)[i].Time.Before((*self)[j].Time)
	})
}

// Removes duplicates as considered by `LokiLine.Equal()`.
// Assumes the log is already sorted.
func (self *LokiLog) Deduplicate() {
	deduped := make(LokiLog, 0, len(*self))
	for i, l := range *self {
		if i > 0 && l.Equal((*self)[i-1]) {
			continue
		}
		deduped = append(deduped, l)
	}
	*self = deduped
}

func (self LokiLine) Equal(o LokiLine) bool {
	return self.Time.Equal(o.Time) &&
		self.Text == o.Text &&
		self.Source == o.Source
}
