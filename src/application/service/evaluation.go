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
	EvaluateRun(src, name string, id, invocationId uuid.UUID, inputs map[string]*domain.Fact) (*nomad.Job, error)
}

func parseSource(src string) (fetchUrl *url.URL, evaluator string, err error) {
	fetchUrl, err = url.Parse(src)
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
	const lokiLabel = "eval"

	tryEval := func(evaluator string) ([]byte, []byte, error) {
		cmd := exec.Command("cicero-evaluator-"+evaluator, args...)
		cmd.Env = append(os.Environ(), extraEnv...) //nolint:gocritic // false positive
		cmd.Dir = src

		e.logger.Debug().
			Stringer("command", cmd).
			Strs("environment", extraEnv).
			Msg("Running evaluator")

		stdout, err := cmd.StdoutPipe()
		if err != nil {
			return nil, nil, err
		}

		stderr, stderrBuf, err := util.BufStderr(cmd)
		if err != nil {
			return nil, nil, err
		}

		e.pipeToLoki(lokiLabel, nil, &stderr, invocationId)

		if err := cmd.Start(); err != nil {
			return nil, nil, err
		}

		var scanErr error
		var result []byte
		scanner := bufio.NewScanner(stdout)
	Scan:
		for scanner.Scan() {
			e.promtailChan <- promtailEntry(lokiLabel, scanner.Text(), "stdout", invocationId)

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
						scanErr = err
						break Scan
					} else {
						result = r
					}
				case "prepare":
					e.logger.Debug().Fields(msg).Msg("Running prepare step")
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
		} else {
			return result, stderrBuf.Bytes(), scanErr
		}
	}

	if evaluator != "" {
		if output, stderr, err := tryEval(evaluator); err != nil {
			e.promtailChan <- promtailEntry(err.Error(), lokiLabel, "error", invocationId)
			return nil, stderr, errors.WithMessagef(err, "Evaluator %q specified in source failed. Stderr: %s", evaluator, string(stderr))
		} else {
			return output, stderr, nil
		}
	} else {
		e.logger.Debug().Msg("No evaluator given in source, trying all")
		var evalErrs error
		for _, evaluator := range e.Evaluators {
			if output, stderr, err := tryEval(evaluator); err != nil {
				var evalErr *EvaluationError
				if errors.As(err, &evalErr) {
					format := ""
					if evalErr != nil {
						format = "\n" + format
					}
					format += "Evaluator %q failed: %w. Stderr: %s"
					evalErrs = fmt.Errorf(format, evaluator, *evalErr, string(stderr))
				} else {
					e.promtailChan <- promtailEntry(err.Error(), lokiLabel, "error", invocationId)
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

func promtailEntry(line, label, fd string, invocationId *uuid.UUID) promtail.Entry {
	labels := map[model.LabelName]model.LabelValue{
		"cicero": model.LabelValue(label),
		"fd":     model.LabelValue(fd),
	}
	if invocationId != nil {
		labels["invocation"] = model.LabelValue(invocationId.String())
	}

	return promtail.Entry{
		Labels: labels,
		Entry: logproto.Entry{
			Timestamp: time.Now(),
			Line:      line,
		},
	}
}

func (e evaluationService) pipeToLoki(label string, stdout, stderr *io.Reader, invocationId *uuid.UUID) {
	if stdout != nil {
		go func() {
			scanner := bufio.NewScanner(*stdout)
			for scanner.Scan() {
				e.promtailChan <- promtailEntry(scanner.Text(), label, "stdout", invocationId)
			}
		}()
	}

	if stderr != nil {
		go func() {
			scanner := bufio.NewScanner(*stderr)
			for scanner.Scan() {
				e.promtailChan <- promtailEntry(scanner.Text(), label, "stderr", invocationId)
			}
		}()
	}
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
		e.logger.Err(err).Str("output", string(output)).Str("stderr", string(stderr)).Send()
		return def, errors.WithMessage(err, "While unmarshaling evaluator output")
	}

	return def, nil
}

func (e evaluationService) EvaluateRun(src, name string, id, invocationId uuid.UUID, inputs map[string]*domain.Fact) (*nomad.Job, error) {
	var def *nomad.Job

	dst, evaluator, err := e.fetchSource(src)
	if err != nil {
		return def, err
	}

	inputsJson, err := json.Marshal(inputs)
	if err != nil {
		return def, errors.WithMessagef(err, "Could not marshal inputs to JSON: %v", inputs)
	}

	extraEnv := []string{
		"CICERO_ACTION_NAME=" + name,
		"CICERO_ACTION_ID=" + id.String(),
		"CICERO_ACTION_INPUTS=" + string(inputsJson),
	}

	output, stderr, err := e.evaluate(dst, evaluator, []string{"eval", "job"}, extraEnv, &invocationId)
	if err != nil {
		return def, err
	}

	output, err = e.transform(output, dst, extraEnv, invocationId)
	if err != nil {
		return def, err
	}

	freeformDef := struct {
		Job *interface{} `json:"job"`
	}{}

	err = json.Unmarshal(output, &freeformDef)
	if err != nil {
		return def, errors.WithMessagef(err, "While unmarshaling evaluator output %s into freeform definition. Stderr: %s", string(output), string(stderr))
	}

	if freeformDef.Job != nil {
		if job, err := json.Marshal(*freeformDef.Job); err != nil {
			return def, err
		} else {
			// escape HCL variable interpolation
			job = bytes.ReplaceAll(job, []byte("${"), []byte("$${"))

			if job, err := jobspec2.ParseWithConfig(&jobspec2.ParseConfig{
				Body:    []byte(`{"job":` + string(job) + "}"),
				AllowFS: false,
				Strict:  true,
			}); err != nil {
				return def, err
			} else {
				for _, tg := range job.TaskGroups {
					for _, t := range tg.Tasks {
						if t.Driver == "nix" {
							if _, found := t.Config["console"]; !found {
								t.Config["console"] = "pipe"
							}

							if pkgsI, found := t.Config["packages"]; found {
								keys := map[string]bool{}
								for _, pkg := range pkgsI.([]interface{}) {
									keys[pkg.(string)] = true
								}

								uniq := []string{}
								for key := range keys {
									uniq = append(uniq, key)
								}

								sort.Strings(uniq)

								t.Config["packages"] = uniq
							}
						}
					}
				}
				def = job
			}
		}
	}

	return def, nil
}

func (e evaluationService) transform(output []byte, src string, extraEnv []string, invocationId uuid.UUID) ([]byte, error) {
	const lokiLabel = "eval-transform"

	for _, transformer := range e.Transformers {
		cmd := exec.Command(transformer)
		cmd.Env = append(os.Environ(), extraEnv...) //nolint:gocritic // false positive
		cmd.Dir = src

		stdin, err := cmd.StdinPipe()
		if err != nil {
			e.promtailChan <- promtailEntry(err.Error(), lokiLabel, "error", &invocationId)
			return nil, err
		}
		if _, err := io.Copy(stdin, bytes.NewReader(output)); err != nil {
			e.promtailChan <- promtailEntry(err.Error(), lokiLabel, "error", &invocationId)
			return nil, err
		}
		if err := stdin.Close(); err != nil {
			e.promtailChan <- promtailEntry(err.Error(), lokiLabel, "error", &invocationId)
			return nil, err
		}

		e.logger.Debug().
			Stringer("command", cmd).
			Strs("environment", extraEnv).
			Str("transformer", transformer).
			Msg("Running transformer")

		stdout, stdoutBuf, stderr, stderrBuf, err := util.BufPipes(cmd)
		if err != nil {
			e.promtailChan <- promtailEntry(err.Error(), lokiLabel, "error", &invocationId)
			return nil, err
		}
		e.pipeToLoki(lokiLabel, &stdout, &stderr, &invocationId)

		if err := cmd.Start(); err != nil {
			e.promtailChan <- promtailEntry(err.Error(), lokiLabel, "error", &invocationId)
			return nil, err
		}

		if err := cmd.Wait(); err != nil {
			var errExit *exec.ExitError
			if errors.As(err, &errExit) {
				evalErr := EvaluationError(*errExit)
				err = errors.WithMessagef(&evalErr, "Failed to transform\nStderr: %s", stderrBuf.String())
			} else {
				e.promtailChan <- promtailEntry(err.Error(), lokiLabel, "error", &invocationId)
			}
			return nil, err
		} else {
			output = stdoutBuf.Bytes()
		}
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
