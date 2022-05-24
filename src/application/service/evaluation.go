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

	"github.com/adrg/xdg"
	"github.com/google/uuid"
	getter "github.com/hashicorp/go-getter/v2"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/hashicorp/nomad/jobspec2"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
)

type EvaluationService interface {
	ListActions(src string) ([]string, error)
	EvaluateAction(src, name string, id uuid.UUID) (domain.ActionDefinition, error)
	EvaluateRun(src, name string, id uuid.UUID, inputs map[string]*domain.Fact) (*nomad.Job, error)
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
	logger       zerolog.Logger
}

func NewEvaluationService(evaluators, transformers []string, logger *zerolog.Logger) EvaluationService {
	return &evaluationService{
		Evaluators:   evaluators,
		Transformers: transformers,
		logger:       logger.With().Str("component", "EvaluationService").Logger(),
	}
}

// Evaluation failed due to a faulty action definition or transformer output.
type EvaluationError struct {
	*exec.ExitError
	Stdout []byte
}

func (e *EvaluationError) Error() string {
	return fmt.Sprintf("%s\nStderr: %s\nStdout: %s", e.ExitError, e.Stderr, e.Stdout)
}

func (e *EvaluationError) Unwrap() error {
	return e.ExitError
}

func (e evaluationService) evaluate(src string, args, extraEnv []string) ([]byte, error) {
	fetchUrl, evaluator, err := parseSource(src)
	if err != nil {
		return nil, err
	}

	cacheDir := config.GetenvStr("CICERO_CACHE_DIR")
	if cacheDir == "" {
		e.logger.Debug().Err(err).Msg("Falling back to XDG cache directory")
		cacheDir = xdg.CacheHome + "/cicero"
	}
	cacheDir += "/sources"

	dst, err := filepath.Abs(cacheDir + "/" + base64.RawURLEncoding.EncodeToString([]byte(src)))
	if err != nil {
		return nil, err
	}

	for {
		result, err := getter.GetAny(context.Background(), dst, fetchUrl.String())
		if err != nil {
			if strings.Contains(err.Error(), "git exited with 128: ") && strings.Contains(err.Error(), "fatal: Not possible to fast-forward, aborting.\n\n") {
				if err := os.RemoveAll(dst); err != nil {
					return nil, err
				}
				continue
			}
			return nil, err
		}
		if result.Dst != dst {
			panic("go-getter did not download to the given directory. This should never happen™")
		}
		break
	}

	tryEval := func(evaluator string) ([]byte, error) {
		cmd := exec.Command("cicero-evaluator-"+evaluator, args...)
		cmd.Env = append(os.Environ(), extraEnv...) //nolint:gocritic // false positive
		cmd.Dir = dst

		e.logger.Debug().
			Stringer("command", cmd).
			Strs("environment", extraEnv).
			Msg("Running evaluator")

		stdout := bytes.Buffer{}
		var stdoutTee io.Reader
		if pipe, err := cmd.StdoutPipe(); err != nil {
			return nil, err
		} else {
			stdoutTee = io.TeeReader(pipe, &stdout)
		}

		// TODO the Invocation must probably somehow be passed to the EvaluationService
		// so that it can update its stdout and stderr fields (e.g. at 500ms intervals)
		// WHILE the command is running.
		// TODO Display the status of prepare steps in the web UI?
		if err := cmd.Start(); err != nil {
			return nil, err
		}

		var scanErr error
		var result []byte
		scanner := bufio.NewScanner(stdoutTee)
	Scan:
		for scanner.Scan() {
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
					// XXX Take *interface{} to unmarshal result into instead of returning its []byte using json.Decoder?
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
				err = errors.WithMessage(&EvaluationError{errExit, stdout.Bytes()}, "Failed to evaluate")
			}
			return nil, err
		} else {
			return result, scanErr
		}
	}

	if evaluator != "" {
		if output, err := tryEval(evaluator); err != nil {
			return nil, errors.WithMessagef(err, "Evaluator %q specified in source failed", evaluator)
		} else {
			return output, nil
		}
	} else {
		e.logger.Debug().Msg("No evaluator given in source, trying all")
		var evalErr error
		for _, evaluator := range e.Evaluators {
			if output, err := tryEval(evaluator); err != nil {
				format := ""
				if evalErr != nil {
					format = "\n" + format
				}
				format += "Evaluator %q failed: %w"
				evalErr = fmt.Errorf(format, evaluator, err)
			} else {
				return output, nil
			}
		}
		e.logger.Err(evalErr).Msg("No evaluator succeeded.")
		return nil, errors.WithMessage(evalErr, "No evaluator succeeded.")
	}
}

func (e evaluationService) EvaluateAction(src, name string, id uuid.UUID) (domain.ActionDefinition, error) {
	var def domain.ActionDefinition

	if output, err := e.evaluate(src,
		[]string{"eval", "meta", "io"},
		[]string{
			"CICERO_ACTION_NAME=" + name,
			"CICERO_ACTION_ID=" + id.String(),
		},
	); err != nil {
		return def, err
	} else if err := json.Unmarshal(output, &def); err != nil {
		e.logger.Err(err).Str("output", string(output)).Send()
		return def, errors.WithMessage(err, "While unmarshaling evaluator output")
	}

	return def, nil
}

func (e evaluationService) EvaluateRun(src, name string, id uuid.UUID, inputs map[string]*domain.Fact) (*nomad.Job, error) {
	var def *nomad.Job

	inputsJson, err := json.Marshal(inputs)
	if err != nil {
		return def, errors.WithMessagef(err, "Could not marshal inputs to JSON: %v", inputs)
	}

	extraEnv := []string{
		"CICERO_ACTION_NAME=" + name,
		"CICERO_ACTION_ID=" + id.String(),
		"CICERO_ACTION_INPUTS=" + string(inputsJson),
	}

	output, err := e.evaluate(src, []string{"eval", "job"}, extraEnv)
	if err != nil {
		return def, err
	}

	output, err = e.transform(output, src, extraEnv)
	if err != nil {
		return def, err
	}

	freeformDef := struct {
		Job *interface{} `json:"job"`
	}{}

	err = json.Unmarshal(output, &freeformDef)
	if err != nil {
		return def, errors.WithMessagef(err, "While unmarshaling evaluator output %s into freeform definition", string(output))
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
						switch t.Driver {
						case "nix":
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

func (e evaluationService) transform(output []byte, src string, extraEnv []string) ([]byte, error) {
	for _, transformer := range e.Transformers {
		cmd := exec.Command(transformer)
		cmd.Env = append(os.Environ(), extraEnv...) //nolint:gocritic // false positive
		cmd.Dir = src

		stdin, err := cmd.StdinPipe()
		if err != nil {
			return nil, err
		}
		if _, err := io.Copy(stdin, bytes.NewReader(output)); err != nil {
			return nil, err
		}
		if err := stdin.Close(); err != nil {
			return nil, err
		}

		e.logger.Debug().
			Stringer("command", cmd).
			Strs("environment", extraEnv).
			Str("transformer", transformer).
			Msg("Running transformer")

		if transformedOutput, err := cmd.Output(); err != nil {
			var errExit *exec.ExitError
			if errors.As(err, &errExit) {
				err = errors.WithMessage(&EvaluationError{errExit, transformedOutput}, "Failed to transform")
			}

			return nil, err
		} else {
			output = transformedOutput
		}
	}

	return output, nil
}

func (e evaluationService) ListActions(src string) ([]string, error) {
	output, err := e.evaluate(src, []string{"list"}, nil)
	if err != nil {
		return nil, err
	}

	var names []string
	if err := json.Unmarshal(output, &names); err != nil {
		e.logger.Err(err).Str("output", string(output)).Msg("Could not unmarshal action names")
		return nil, errors.WithMessagef(err, "While unmarshaling action names:\n%s", string(output))
	}

	return names, nil
}
