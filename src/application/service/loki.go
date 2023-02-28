package service

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"hash/fnv"
	"io"
	"net/http"
	"net/url"
	"reflect"
	"sort"
	"strconv"
	"strings"
	"time"

	"github.com/gorilla/websocket"
	"github.com/grafana/loki/pkg/loghttp"
	"github.com/pborman/ansi"
	"github.com/pkg/errors"
	prometheus "github.com/prometheus/client_golang/api"
	"github.com/rs/zerolog"
)

type LokiService interface {
	// Cancel the context to close the connection.
	Tail(context.Context, string, time.Time) LokiLineChan

	// Start and end times are inclusive.
	// A limit of 0 means no limit.
	QueryRange(string, time.Time, time.Time, LokiDirection, uint) LokiLineChan

	// Start and end times are inclusive.
	QueryRangePages(string, time.Time, time.Time, LokiDirection, uint, func(loghttp.Stream) (bool, error)) error

	// Uses `Tail()` to fetch and `QueryRange()` to know when to stop.
	// More efficient than `QueryRange()` but limited in options.
	// Degrades to `Tail()` if no end time is given.
	TailRange(context.Context, string, time.Time, *time.Time) LokiLineChan

	// Like `Tail()` but also fetches the start of the log up until the tailed lines.
	QueryTail(context.Context, string, time.Time) LokiLineChan

	// Like `TailRange()` but also fetches the start of the log up until the tailed lines.
	QueryTailRange(context.Context, string, time.Time, *time.Time) LokiLineChan
}

type LokiLineChan <-chan LokiLineMsg

func (self LokiLineChan) StripAnsi() LokiLineChan {
	lines := make(chan LokiLineMsg, 1)

	go func() {
		defer close(lines)

		for line := range self {
			if line.LokiLine != nil {
				line.StripAnsi()
			}
			lines <- line
		}
	}()

	return lines
}

// This function filters out duplicate lines with the same timestamp.
// This is needed because the /loki/api/v1/{tail,query_range} endpoints
// stop sending lines after a configured limit is reached.
// `Tail()` and `QueryRangePages()` therefore reconnect in a loop,
// bumping the start time parameter each time to the latest time encountered.
// They cannot bump the time by one unit of the smallest resolution
// because there may be more messages with the same timestamp that
// have not been received yet due to the limit on the endpoint.
// However, this means that the message(s) with the last timestamp
// will be received again, so we have to skip them the second time
// that they are received. That's what this function does.
func (self LokiLineChan) deduplicate() LokiLineChan {
	deduped := make(chan LokiLineMsg)

	go func() {
		defer close(deduped)

		var latestTime *time.Time
		var hashes []uint32
		hash := fnv.New32()

	Line:
		for line := range self {
			if line.Err != nil {
				deduped <- line
				continue
			}

			if latestTime != nil && line.Time.Before(*latestTime) {
				// We know that we have already seen this line because it has an older time.
				continue
			}

			line.Time = line.Time.UTC() // so the timezone does not change the hash
			var buf bytes.Buffer
			if err := json.NewEncoder(&buf).Encode(line); err != nil {
				deduped <- LokiLineMsg{Err: err}
				continue
			}
			hash.Reset()
			if _, err := io.Copy(hash, &buf); err != nil {
				deduped <- LokiLineMsg{Err: err}
				continue
			}
			sum := hash.Sum32()

			if latestTime == nil || line.Time.After(*latestTime) {
				latestTime = &line.Time
				hashes = []uint32{sum}

				// This is either the first line or its time is newer so we cannot have seen it yet.
				deduped <- line
				continue
			}

			if !line.Time.Equal(*latestTime) {
				panic("This should never happen™")
			}

			for _, seen := range hashes {
				if sum == seen {
					// This line's hash has already been seen yet for the latest time.
					continue Line
				}
			}

			// This line's hash has not been seen yet for the latest time.
			hashes = append(hashes, sum)
			deduped <- line
		}
	}()

	return deduped
}

type LokiLog []LokiLine

type LokiLine struct {
	Time   time.Time         `json:"time"`
	Text   string            `json:"text"`
	Labels map[string]string `json:"labels"`
}

const (
	LokiDirectionBackward LokiDirection = iota
	LokiDirectionForward
)

type LokiDirection uint

func (self LokiDirection) String() string {
	switch self {
	case LokiDirectionBackward:
		return "BACKWARD"
	case LokiDirectionForward:
		return "FORWARD"
	default:
		panic("Invalid value for LokiDirection: " + strconv.FormatUint(uint64(self), 10))
	}
}

type lokiService struct {
	logger     zerolog.Logger
	prometheus prometheus.Client
}

func NewLokiService(prometheusClient prometheus.Client, logger *zerolog.Logger) LokiService {
	return &lokiService{
		logger:     logger.With().Str("component", "LokiService").Logger(),
		prometheus: prometheusClient,
	}
}
func (self lokiService) QueryTail(ctx context.Context, query string, start time.Time) LokiLineChan {
	return self.QueryTailRange(ctx, query, start, nil)
}

func (self lokiService) QueryTailRange(ctx context.Context, query string, start time.Time, end *time.Time) LokiLineChan {
	lines := make(chan LokiLineMsg, 1)

	go func() {
		defer close(lines)

		ctx, cancel := context.WithCancel(ctx)
		defer cancel()

		// First, start tailing.
		tail := self.TailRange(ctx, query, start, end)

		// Read the first line from tail
		// and query up to that line's time (inclusive).
		var log LokiLineChan
		firstTailLine, ok := <-tail
		if !ok {
			return
		} else if err := firstTailLine.Err; err != nil {
			lines <- firstTailLine
			return
		} else {
			log = self.QueryRange(query, start, firstTailLine.Time, LokiDirectionForward, 0)
		}

		// Now, if there were multiple lines with the same time, they may be duplicated.
		// So we concatenate and deduplicate `log` and `tail` until we encounter a line
		// with a newer time than the first tail line; at that point we know that lines
		// will not overlap anymore.
		seam := make(chan LokiLineMsg, 1)
		go func() {
			defer close(seam)

			for line := range log {
				seam <- line
			}

			for line := range tail {
				seam <- line

				if line.Err == nil && line.Time.After(firstTailLine.Time) {
					break
				}
			}
		}()

		for line := range LokiLineChan(seam).deduplicate() {
			lines <- line
		}

		// Finally, read the remaining tail lines.
		for line := range tail {
			lines <- line
		}
	}()

	return lines
}

func (self lokiService) TailRange(ctx context.Context, query string, start time.Time, end *time.Time) LokiLineChan {
	if end == nil {
		return self.Tail(ctx, query, start)
	}

	if end.After(time.Now()) {
		panic("`end` must not be in the future! Otherwise new lines may be pushed so the expected last line is no longer the last, blocking forever.")
	}

	lines := make(chan LokiLineMsg, 1) // at least one so we can return immediately on error

	var lastLine LokiLineMsg
	if line, ok := <-self.QueryRange(query, start, *end, LokiDirectionBackward, 1); !ok {
		close(lines)
		return lines
	} else if line.Err != nil {
		lines <- LokiLineMsg{Err: line.Err}
		close(lines)
		return lines
	} else {
		lastLine = line
	}

	ctx, cancel := context.WithCancel(ctx)

	go func() {
		defer close(lines)
		defer cancel()

		tail := self.Tail(ctx, query, start)

		/*
			You might think why not just do this?

			```
			for line := range tail {
				lines <- line

				if line.LokiLine != nil && line.Equal(*lastLine.LokiLine) {
					break
				}
			}
			```

			There may be multiple lines that equal `lastLine`
			before the actual last line.
			In that case we want to include them.
			So we read as long as lines are available in the channel
			and only check for equality when there are no more values.
			If that was not the last line though we might just have
			been faster than the sender can send values to the channel
			so we try a blocking read to avoid a busy loop.
		*/

		block := false
	Outer:
		for {
			var line *LokiLineMsg

		Recv:
			for {
				if block {
					if l, ok := <-tail; !ok {
						break Outer
					} else {
						line = &l
					}
					block = false
				} else {
					select {
					case l, ok := <-tail:
						if !ok {
							break Outer
						}
						line = &l
					default:
						// `line` is still the value from the last iteration of `Recv`
						// because there was no value on `tail`.
						if line != nil && line.Equal(*lastLine.LokiLine) {
							break Outer
						}
						break Recv
					}
				}

				lines <- *line

				if line.Err != nil {
					break Outer
				}
			}

			block = true
		}
	}()

	return lines
}

type LokiLineMsg struct {
	*LokiLine
	Err error `json:"error,omitempty"`
}

// https://github.com/golang/go/issues/5161
func (self LokiLineMsg) MarshalJSON() ([]byte, error) {
	type Self LokiLineMsg

	var errMsg string
	if self.Err != nil {
		errMsg = self.Err.Error()
	}

	return json.Marshal(struct {
		Self
		Err string `json:"error,omitempty"`
	}{
		Self: Self(self),
		Err:  errMsg,
	})
}

func (self lokiService) Tail(ctx context.Context, query string, start time.Time) LokiLineChan {
	const limit = 500 // default from Loki's API docs

	lines := make(chan LokiLineMsg, 1) // at least one so we can shut down immediately on error

	go func() {
		closedLines := false
		defer func() {
			closedLines = true
			close(lines)
		}()

	Conn:
		for {
			u := self.prometheus.URL("/loki/api/v1/tail", nil)
			u.Scheme = "ws"
			{
				q := url.Values{}
				q.Set("query", query)
				q.Set("limit", strconv.FormatUint(limit, 10))
				q.Set("start", strconv.FormatInt(start.UnixNano(), 10))
				u.RawQuery = q.Encode()
			}

			conn, _, err := websocket.DefaultDialer.DialContext(ctx, u.String(), nil)
			if err != nil {
				lines <- LokiLineMsg{Err: err}
				break
			}

			closureAnnounced := false
			announceClosure := func(message string) {
				if closureAnnounced {
					return
				}
				closureAnnounced = true

				// Not really necessary but let's be a good citizen and announce the closure.
				if err := conn.WriteControl(
					websocket.CloseMessage,
					websocket.FormatCloseMessage(websocket.CloseNormalClosure, message),
					time.Now().Add(time.Millisecond), // Do not wait for long though.
				); err != nil {
					// Does not matter much, just log and go on.
					self.logger.Err(err).Msg("Failed to send close message")
				}
			}

			go func() {
				<-ctx.Done()

				announceClosure("canceled")

				// This will unblock reads and return error
				// thereby exiting the `Conn` loop and close `lines`.
				if err := conn.Close(); err != nil && !closedLines {
					lines <- LokiLineMsg{Err: err}
				}
			}()

			var numLinesRead uint = 0

		Read:
			for {
				msg := struct {
					Streams []struct {
						Stream map[string]string `json:"stream"`
						Values [][]string        `json:"values"`
					} `json:"streams"`
				}{}
				err := conn.ReadJSON(&msg)
				if err != nil {
					if websocket.IsCloseError(err, websocket.CloseNormalClosure) {
						// Loki closed the connection normally but unexpectedly, let's reconnect.
						break
					} else {
						if _, ok := err.(*websocket.CloseError); ok {
							// Do not try and fail to send a close message
							// as the websocket is already closed.
							closureAnnounced = true
						}

						// If there is an error on `ctx` that means it was canceled
						// so the error from `conn.ReadJSON()` can be ignored.
						if ctx.Err() == nil {
							lines <- LokiLineMsg{Err: err}
						}

						break Conn
					}
				}

				for _, stream := range msg.Streams {
					for _, value := range stream.Values {
						numLinesRead += 1

						ns, err := strconv.ParseInt(value[0], 10, 64)
						if err != nil {
							lines <- LokiLineMsg{Err: err}
							break Read
						}

						lineTime := time.Unix(
							ns/int64(time.Second),
							ns%int64(time.Second),
						).UTC()

						lines <- LokiLineMsg{
							LokiLine: &LokiLine{
								Time:   lineTime,
								Text:   value[1],
								Labels: stream.Stream,
							},
						}

						// Bump the timestamp so in the next iteration
						// we do not receive messages that we already received in this iteration
						// except those with the same timestamp. This overlap is necessary.
						// We cannot bump the timestamp even by one nanosecond because
						// when there are multiple lines with the same timestamp
						// and the limit is reached before the last line with that timestamp has been sent,
						// then the remaining not yet sent lines with the same timestamp would never be sent
						// because we would skip them in the next iteration due to the bumped start time.
						// The overlapping lines with the same timestamp are filtered out by `LokiLineChan.deduplicate()`.
						// Note this only works if all lines with the same timestamp fit in the limit,
						// otherwise we will loop indefinitely.
						start = lineTime
					}
				}

				if numLinesRead >= limit {
					// When the limit is reached Loki will not send any further lines
					// but it does also not close the websocket connection
					// or send a message indicating that the limit was reached.
					// Let's reconnect to reset the limit so we won't hang waiting forever.
					announceClosure("limit reached")
					break
				}
			}

			announceClosure("done")
			if err := conn.Close(); err != nil {
				lines <- LokiLineMsg{Err: err}
			}
		}
	}()

	return LokiLineChan(lines).deduplicate()
}

func (self lokiService) QueryRange(query string, start time.Time, end time.Time, direction LokiDirection, limit uint) LokiLineChan {
	lines := make(chan LokiLineMsg, 1) // at least one so we can shut down immediately on error

	go func() {
		defer close(lines)

		// TODO: figure out the correct value for our infra, 5000 is the default configuration in loki
		var pageSize uint = 5000
		if limit < pageSize && limit != 0 {
			pageSize = limit
		}

		log := make(LokiLog, 0, pageSize)

		if err := self.QueryRangePages(query, start, end, direction, pageSize, func(stream loghttp.Stream) (bool, error) {
			page := make(LokiLog, 0, len(stream.Entries))
			page.FromStream(stream)

			pageLenCap := uint(len(page))
			if numRemaining := limit - uint(len(log)); limit != 0 && numRemaining < uint(len(page)) {
				pageLenCap = numRemaining
			}

			log = append(log, page[0:pageLenCap]...)

			if limit != 0 && uint(len(log)) == limit {
				return true, nil
			}

			return false, nil
		}); err != nil {
			lines <- LokiLineMsg{Err: err}
		}

		log.Sort(direction)

		for _, line := range log {
			line := line // copy so we don't point to loop variable
			lines <- LokiLineMsg{LokiLine: &line}
		}
	}()

	return LokiLineChan(lines).deduplicate()
}

func (self lokiService) QueryRangePages(query string, start time.Time, end time.Time, direction LokiDirection, limit uint, callback func(loghttp.Stream) (bool, error)) error {
	const timeout time.Duration = 2 * time.Second

	end = end.Add(time.Nanosecond) // make end inclusive

Page:
	for {
		self.logger.Trace().Str("query", query).Stringer("start", start).Stringer("end", end).Msg("Fetching from query_range endpoint")

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
		q.Set("limit", strconv.FormatUint(uint64(limit), 10))
		q.Set("start", strconv.FormatInt(start.UnixNano(), 10))
		q.Set("end", strconv.FormatInt(end.UnixNano(), 10))
		q.Set("direction", direction.String())
		req.URL.RawQuery = q.Encode()

		ctxTimeout, ctxTimeoutCancel := context.WithTimeout(context.Background(), timeout)
		done, body, err := self.prometheus.Do(ctxTimeout, req)
		ctxTimeoutCancel()
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
			break Page
		}

		var numEntries uint
		for _, stream := range streams {
			if stop, err := callback(stream); err != nil {
				return err
			} else if stop {
				break Page
			}

			numEntries += uint(len(stream.Entries))
			switch direction {
			case LokiDirectionBackward:
				if entry := stream.Entries[0]; entry.Timestamp.Before(start) {
					start = entry.Timestamp
				}
			case LokiDirectionForward:
				if entry := stream.Entries[len(stream.Entries)-1]; entry.Timestamp.After(start) {
					start = entry.Timestamp
				}
			default:
				panic("Unhandled value for LokiDirection")
			}
		}
		if numEntries < limit {
			break Page
		}
	}

	return nil
}

func (self *LokiLog) FromStream(stream loghttp.Stream) {
	for _, entry := range stream.Entries {
		lines := strings.Split(entry.Line, "\r")
		for _, line := range lines {
			*self = append(*self, LokiLine{
				Time:   entry.Timestamp,
				Text:   line,
				Labels: stream.Labels.Map(),
			})
		}
	}
}

func (self *LokiLog) Sort(direction LokiDirection) {
	switch direction {
	case LokiDirectionBackward:
		sort.Sort(sort.Reverse(self))
	case LokiDirectionForward:
		sort.Sort(self)
	default:
		panic("Unhandled value for LokiDirection")
	}
}

// Removes consecutive duplicates as considered by `LokiLine.Equal()`.
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

// Implements `sort.Interface`.
func (self LokiLog) Len() int {
	return len(self)
}

// Implements `sort.Interface`.
func (self LokiLog) Less(i, j int) bool {
	a := self[i].Time
	b := self[j].Time
	return a.Before(b)
}

// Implements `sort.Interface`.
func (self LokiLog) Swap(i, j int) {
	self[i], self[j] = self[j], self[i]
}

func (self *LokiLine) StripAnsi() {
	if sane, _ := ansi.Strip([]byte(self.Text)); sane != nil {
		self.Text = string(sane)
	}
}

func (self LokiLine) Equal(o LokiLine) bool {
	return self.Time.Equal(o.Time) &&
		self.Text == o.Text &&
		reflect.DeepEqual(self.Labels, o.Labels)
}
