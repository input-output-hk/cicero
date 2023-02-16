package service

import (
	"bufio"
	"bytes"
	"context"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"io"
	"net/url"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"sync"
	"time"

	"github.com/adrg/xdg"
	"github.com/google/uuid"
	promtail "github.com/grafana/loki/clients/pkg/promtail/api"
	"github.com/grafana/loki/pkg/logproto"
	getter "github.com/hashicorp/go-getter/v2"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/hashicorp/nomad/jobspec2"
	"github.com/pkg/errors"
	"github.com/prometheus/common/model"
	"github.com/rs/zerolog"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/util"
)

type EvaluationService interface {
	ListActions(src string) ([]string, error)
	EvaluateAction(src, name string, id uuid.UUID) (domain.ActionDefinition, error)
	EvaluateRun(src, name string, id, invocationId uuid.UUID, inputs map[string]domain.Fact) (*nomad.Job, error)
}

const (
	lokiSourceErr    = "error"
	lokiSourceStdout = "stdout"
	lokiSourceStderr = "stderr"
	lokiEval         = "eval"
	lokiTransform    = "eval-transform"
)

func newScanner(input io.Reader) (scanner *bufio.Scanner) {
	scanner = bufio.NewScanner(input)
	scanner.Buffer(make([]byte, 1024*64), 1024*1024*3)
	return
}

func parseSource(src string) (fetchUrl *url.URL, evaluator string, err error) {
	fetchUrl, err = url.Parse(src)
	err = errors.WithMessage(err, "While parsing action source")
	if err != nil {
		return
	}

	evaluator = fetchUrl.Fragment
	fetchUrl.Fragment = ""
	fetchUrl.RawFragment = ""

	return
}

type evaluationService struct {
	Evaluators   []string // Default evaluators. Will be tried in order if none is given for a source.
	Transformers []string
	promtailChan chan<- promtail.Entry
	logger       zerolog.Logger
}

func NewEvaluationService(evaluators, transformers []string, promtailChan chan<- promtail.Entry, logger *zerolog.Logger) EvaluationService {
	return &evaluationService{
		Evaluators:   evaluators,
		Transformers: transformers,
		promtailChan: promtailChan,
		logger:       logger.With().Str("component", "EvaluationService").Logger(),
	}
}

// Evaluation failed due to a faulty action definition or transformer output.
type EvaluationError exec.ExitError

func (e *EvaluationError) Error() string {
	return (*exec.ExitError)(e).Error()
}

func (e *EvaluationError) Unwrap() error {
	return (*exec.ExitError)(e)
}

func (e evaluationService) cacheDir(src string) (string, error) {
	cacheDir := config.GetenvStr("CICERO_CACHE_DIR")
	if cacheDir == "" {
		e.logger.Debug().Msg("Falling back to XDG cache directory")
		cacheDir = xdg.CacheHome + "/cicero"
	}
	cacheDir += "/sources"

	return filepath.Abs(cacheDir + "/" + base64.RawURLEncoding.EncodeToString([]byte(src)))
}

func (e evaluationService) fetchSource(src string) (string, string, error) {
	dst, err := e.cacheDir(src)
	if err != nil {
		return dst, "", err
	}

	fetchUrl, evaluator, err := parseSource(src)
	if err != nil {
		return dst, evaluator, err
	}

	for {
		result, err := getter.GetAny(context.Background(), dst, fetchUrl.String())
		if err != nil {
			if strings.Contains(err.Error(), "git exited with 128: ") && strings.Contains(err.Error(), "fatal: Not possible to fast-forward, aborting.\n\n") {
				if err := os.RemoveAll(dst); err != nil {
					return dst, evaluator, err
				}
				continue
			}
			return dst, evaluator, err
		}
		if result.Dst != dst {
			panic("go-getter did not download to the given directory. This should never happen™")
		}
		break
	}

	return dst, evaluator, err
}

func (e evaluationService) evaluate(src, evaluator string, args, extraEnv []string, invocationId *uuid.UUID) ([]byte, []byte, error) {
	tryEval := func(evaluator string) ([]byte, []byte, error) {
		cmd := exec.Command("cicero-evaluator-"+evaluator, args...)
		cmd.Env = append(os.Environ(), extraEnv...) //nolint:gocritic // false positive
		cmd.Dir = src

		e.logger.Debug().
			Stringer("command", cmd).
			Strs("environment", extraEnv).
			Str("directory", src).
			Msg("Running evaluator")

		stdout, err := cmd.StdoutPipe()
		if err != nil {
			return nil, nil, err
		}

		stderr, stderrBuf, err := util.BufStderr(cmd)
		if err != nil {
			return nil, nil, err
		}

		var lokiWg *sync.WaitGroup
		var lokiStderrErr *error
		if invocationId != nil {
			stderrReader := io.Reader(stderr)
			lokiWg, _, lokiStderrErr = e.pipeToLoki(lokiEval, nil, &stderrReader, *invocationId)
		}

		if err := cmd.Start(); err != nil {
			return nil, nil, err
		}

		var scanErr error
		var result []byte
		scanner := newScanner(stdout)
	Scan:
		for scanner.Scan() {
			if invocationId != nil {
				e.promtailChan <- promtailEntry(scanner.Text(), lokiEval, lokiSourceStdout, *invocationId)
			}

			var msg map[string]interface{}
			if err := json.Unmarshal(scanner.Bytes(), &msg); err != nil {
				scanErr = err
				break
			}

			if event, ok := msg["event"]; !ok {
				scanErr = fmt.Errorf("Message without event key received from evaluator: %v", msg)
				break
			} else {
				switch event {
				default:
					scanErr = fmt.Errorf("Message with unknown event received from evaluator: %q", event)
					break Scan
				case "error":
					scanErr = fmt.Errorf("Error message received from evaluator: %q", msg["error"])
					break Scan
				case "result":
					// XXX Take *interface{} to unmarshal result into using json.Decoder instead of returning its []byte?
					if r, err := json.Marshal(msg["result"]); err != nil {
						scanErr = errors.WithMessage(err, "While marshaling evaluator result")
						break Scan
					} else {
						result = r
					}
				case "prepare":
					e.logger.Debug().Fields(msg).Msg("Running prepare step")
				}
			}
		}
		if err := scanner.Err(); err != nil {
			return nil, stderrBuf.Bytes(), errors.WithMessage(err, "While scanning stdout")
		}

		// fill stderrBuf
		if lokiWg != nil {
			lokiWg.Wait()
			if *lokiStderrErr != nil {
				return nil, stderrBuf.Bytes(), *lokiStderrErr
			}
		} else {
			for {
				if _, err := stderr.Read(make([]byte, 512)); err != nil {
					if err == io.EOF {
						break
					}
					return nil, nil, errors.WithMessage(err, "While reading stderr")
				}
			}
		}

		if err := cmd.Wait(); err != nil {
			var errExit *exec.ExitError
			if errors.As(err, &errExit) {
				evalErr := EvaluationError(*errExit)
				err = errors.WithMessage(&evalErr, "Failed to evaluate")
			}
			return nil, stderrBuf.Bytes(), err
		}

		return result, stderrBuf.Bytes(), scanErr
	}

	if evaluator != "" {
		if output, stderr, err := tryEval(evaluator); err != nil {
			if invocationId != nil {
				e.promtailChan <- promtailEntry(err.Error(), lokiEval, lokiSourceErr, *invocationId)
			}
			return nil, stderr, errors.WithMessagef(err, "Evaluator %q specified in source failed. Stderr: %s", evaluator, string(stderr))
		} else {
			return output, stderr, nil
		}
	} else {
		e.logger.Debug().Msg("No evaluator given in source, trying all")
		var evalErrs error
		for _, evaluator := range e.Evaluators {
			if output, stderr, err := tryEval(evaluator); err != nil {
				if invocationId != nil {
					e.promtailChan <- promtailEntry(err.Error(), lokiEval, lokiSourceErr, *invocationId)
				}

				var evalErr *EvaluationError
				if errors.As(err, &evalErr) {
					format := ""
					if evalErr != nil {
						format = "\n" + format
					}
					format += "Evaluator %q failed: %w. Stderr: %s"
					evalErrs = fmt.Errorf(format, evaluator, evalErr, string(stderr))
				} else {
					return output, stderr, err
				}
			} else {
				return output, stderr, nil
			}
		}
		const errMsg = "No evaluator succeeded."
		e.logger.Err(evalErrs).Msg(errMsg)
		return nil, nil, errors.WithMessage(evalErrs, errMsg)
	}
}

func promtailEntry(line, label, source string, invocationId uuid.UUID) promtail.Entry {
	return promtail.Entry{
		Labels: map[model.LabelName]model.LabelValue{
			"cicero":     model.LabelValue(label),
			"source":     model.LabelValue(source),
			"invocation": model.LabelValue(invocationId.String()),
		},
		Entry: logproto.Entry{
			Timestamp: time.Now(),
			Line:      line,
		},
	}
}

// Sends stdout and stderr (if not nil) async to Loki.
// Returns a WaitGroup that finishes when both streams have been read completely.
func (e evaluationService) pipeToLoki(label string, stdout, stderr *io.Reader, invocationId uuid.UUID) (wg *sync.WaitGroup, stdoutErr, stderrErr *error) {
	wg = &sync.WaitGroup{}
	stdoutErr = new(error)
	stderrErr = new(error)

	pipeAll := func(input io.Reader, source string) error {
		scanner := newScanner(input)
		for scanner.Scan() {
			e.promtailChan <- promtailEntry(scanner.Text(), label, source, invocationId)
		}
		// Intentionally not sending the error to promtail here; caller should do that.
		return errors.WithMessage(scanner.Err(), "While scanning "+source)
	}

	if stdout != nil {
		wg.Add(1)
		go func() {
			defer wg.Done()
			*stdoutErr = pipeAll(*stdout, lokiSourceStdout)
		}()
	}

	if stderr != nil {
		wg.Add(1)
		go func() {
			defer wg.Done()
			*stderrErr = pipeAll(*stderr, lokiSourceStderr)
		}()
	}

	return
}

func (e evaluationService) EvaluateAction(src, name string, id uuid.UUID) (domain.ActionDefinition, error) {
	var def domain.ActionDefinition

	dst, evaluator, err := e.fetchSource(src)
	if err != nil {
		return def, err
	}

	if output, stderr, err := e.evaluate(
		dst, evaluator,
		[]string{"eval", "meta", "io"},
		[]string{
			"CICERO_ACTION_NAME=" + name,
			"CICERO_ACTION_ID=" + id.String(),
		},
		nil,
	); err != nil {
		return def, err
	} else if err := json.Unmarshal(output, &def); err != nil {
		e.logger.Err(err).RawJSON("output", output).Str("stderr", string(stderr)).Send()
		return def, errors.WithMessage(err, "While unmarshaling evaluator output")
	}

	return def, nil
}

func (e evaluationService) EvaluateRun(src, name string, id, invocationId uuid.UUID, inputs map[string]domain.Fact) (*nomad.Job, error) {
	dst, evaluator, err := e.fetchSource(src)
	if err != nil {
		e.promtailChan <- promtailEntry(err.Error(), lokiEval, lokiSourceErr, invocationId)
		return nil, err
	}

	inputsJson, err := json.Marshal(inputs)
	if err != nil {
		e.promtailChan <- promtailEntry(err.Error(), lokiEval, lokiSourceErr, invocationId)
		return nil, errors.WithMessagef(err, "Could not marshal inputs to JSON: %v", inputs)
	}

	extraEnv := []string{
		"CICERO_ACTION_NAME=" + name,
		"CICERO_ACTION_ID=" + id.String(),
		"CICERO_ACTION_INPUTS=" + string(inputsJson),
	}

	output, stderr, err := e.evaluate(dst, evaluator, []string{"eval", "job"}, extraEnv, &invocationId)
	if err != nil {
		return nil, err
	}

	// We support both HCL-JSON (HCL jobspec in JSON equivalent notation)
	// and Nomad's API JSON Job specification.
	// So at this point we don't know which one we get.
	type FreeformDef struct {
		Job *any `json:"job"`
		// We are not interested in other fields of the run definition.
	}
	freeformDef := FreeformDef{}

	err = json.Unmarshal(output, &freeformDef)
	if err != nil {
		err = errors.WithMessage(err, "While unmarshaling evaluator output into freeform definition")
		e.promtailChan <- promtailEntry(err.Error(), lokiEval, lokiSourceErr, invocationId)
		return nil, errors.WithMessagef(err, "\nOutput: %s\nStderr: %s", string(output), string(stderr))
	}

	if freeformDef.Job == nil {
		return nil, nil
	}

	// Canonicalize the definition. That is, make sure the job is in API-JSON.
	if job, err := e.unmarshalJob(*freeformDef.Job, invocationId); err != nil {
		e.promtailChan <- promtailEntry(err.Error(), lokiEval, lokiSourceErr, invocationId)
		return nil, err
	} else {
		var jobIface any = *job
		freeformDef.Job = &jobIface
	}

	// Marshal back to JSON to pass through transformers.
	canonicalizedDefJson, err := json.Marshal(freeformDef)
	if err != nil {
		err = errors.WithMessage(err, "While marshaling canonicalized freeformDef")
		e.promtailChan <- promtailEntry(err.Error(), lokiTransform, lokiSourceErr, invocationId)
		return nil, err
	}

	if output, err = e.transform(canonicalizedDefJson, dst, extraEnv, invocationId); err != nil {
		return nil, errors.WithMessage(err, "While transforming")
	}

	type Def struct {
		Job nomad.Job `json:"job"`
		// We are not interested in other fields of the run definition.
	}
	def := Def{}

	if err := json.Unmarshal(output, &def); err != nil {
		err = errors.WithMessagef(err, "While unmarshaling transformer output. Output: %s", output)
		e.promtailChan <- promtailEntry(err.Error(), lokiTransform, lokiSourceErr, invocationId)
		return nil, err
	}

	setJobDefaults(&def.Job)

	return &def.Job, nil
}

// Takes either an API-JSON or HCL-JSON job.
func (e evaluationService) unmarshalJob(freeformJob any, invocationId uuid.UUID) (*nomad.Job, error) {
	// Marshal back to JSON so we can try both formats.
	freeformJobJson, err := json.Marshal(freeformJob)
	if err != nil {
		return nil, errors.WithMessage(err, "While marshaling freeformJob")
	}

	def := &nomad.Job{}

	// Try parsing as API-JSON.
	err = json.Unmarshal(freeformJobJson, def)

	// If that didn't work try parsing as HCL-JSON.
	if err != nil {
		e.promtailChan <- promtailEntry(err.Error(), lokiEval, lokiSourceErr, invocationId)

		// escape HCL variable interpolation
		hclJsonJob := bytes.ReplaceAll(freeformJobJson, []byte("${"), []byte("$${"))

		def, err = jobspec2.ParseWithConfig(&jobspec2.ParseConfig{
			Body:    []byte(`{"job":` + string(hclJsonJob) + "}"),
			AllowFS: false,
			Strict:  true,
		})
		if err != nil {
			return def, err
		}
	}

	return def, nil
}

func setJobDefaults(job *nomad.Job) {
	if job.Type == nil {
		jobType := nomad.JobTypeBatch
		job.Type = &jobType
	}

	for _, group := range job.TaskGroups {
		if *job.Type == nomad.JobTypeBatch && (group.RestartPolicy == nil || group.RestartPolicy.Attempts == nil) {
			if group.RestartPolicy == nil {
				group.RestartPolicy = &nomad.RestartPolicy{}
			}
			attempts := 0
			group.RestartPolicy.Attempts = &attempts
		}

		for _, task := range group.Tasks {
			if task.Driver == "nix" {
				if _, found := task.Config["console"]; !found {
					task.Config["console"] = "pipe"
				}

				// deduplicate and sort packages
				if pkgsI, found := task.Config["packages"]; found {
					keys := map[string]bool{}
					for _, pkg := range pkgsI.([]interface{}) {
						keys[pkg.(string)] = true
					}

					uniq := []string{}
					for key := range keys {
						uniq = append(uniq, key)
					}

					sort.Strings(uniq)

					task.Config["packages"] = uniq
				}
			}
		}
	}
}

func (e evaluationService) transform(output []byte, src string, extraEnv []string, invocationId uuid.UUID) ([]byte, error) {
	for _, transformer := range e.Transformers {
		cmd := exec.Command(transformer)
		cmd.Env = append(os.Environ(), extraEnv...) //nolint:gocritic // false positive
		cmd.Dir = src

		e.logger.Debug().
			Stringer("command", cmd).
			Strs("environment", extraEnv).
			Str("transformer", transformer).
			Msg("Running transformer")

		stdout, stdoutBuf, stderr, stderrBuf, err := util.BufPipes(cmd)
		if err != nil {
			e.promtailChan <- promtailEntry(err.Error(), lokiTransform, lokiSourceErr, invocationId)
			return nil, err
		}
		var lokiWg *sync.WaitGroup
		var lokiStdoutErr *error
		var lokiStderrErr *error
		{
			stdoutReader := io.Reader(stdout)
			stderrReader := io.Reader(stderr)
			lokiWg, lokiStdoutErr, lokiStderrErr = e.pipeToLoki(lokiTransform, &stdoutReader, &stderrReader, invocationId)
		}

		stdin, err := cmd.StdinPipe()
		if err != nil {
			e.promtailChan <- promtailEntry(err.Error(), lokiTransform, lokiSourceErr, invocationId)
			return nil, err
		}

		if err := cmd.Start(); err != nil {
			e.promtailChan <- promtailEntry(err.Error(), lokiTransform, lokiSourceErr, invocationId)
			return nil, err
		}

		if _, err := io.Copy(stdin, bytes.NewReader(output)); err != nil {
			e.promtailChan <- promtailEntry(err.Error(), lokiTransform, lokiSourceErr, invocationId)
			return nil, err
		}
		if err := stdin.Close(); err != nil {
			e.promtailChan <- promtailEntry(err.Error(), lokiTransform, lokiSourceErr, invocationId)
			return nil, err
		}

		lokiWg.Wait() // fill stdoutBuf and stderrBuf
		err = cmd.Wait()

		if err == nil {
			err = *lokiStdoutErr
		}
		if err == nil {
			err = *lokiStderrErr
		}

		if err != nil {
			e.promtailChan <- promtailEntry(err.Error(), lokiTransform, lokiSourceErr, invocationId)

			var errExit *exec.ExitError
			if errors.As(err, &errExit) {
				evalErr := EvaluationError(*errExit)
				err = errors.WithMessagef(&evalErr, "Failed to transform\nStderr: %s", stderrBuf.String())
			}

			return nil, err
		}

		output = stdoutBuf.Bytes()
	}

	return output, nil
}

func (e evaluationService) ListActions(src string) ([]string, error) {
	dst, evaluator, err := e.fetchSource(src)
	if err != nil {
		return nil, err
	}

	output, stderr, err := e.evaluate(dst, evaluator, []string{"list"}, nil, nil)
	if err != nil {
		return nil, err
	}

	var names []string
	if err := json.Unmarshal(output, &names); err != nil {
		e.logger.Err(err).Str("output", string(output)).Str("stderr", string(stderr)).Msg("Could not unmarshal action names")
		return nil, errors.WithMessagef(err, "While unmarshaling action names:\n%s\nStderr: %s", string(output), string(stderr))
	}

	return names, nil
}
