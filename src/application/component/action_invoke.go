package component

import (
	"context"
	"encoding/json"
	"strings"

	nomad "github.com/hashicorp/nomad/api"
	"github.com/jackc/pgx/v4"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pkg/errors"
	"github.com/vivek-ng/concurrency-limiter/priority"
	"github.com/rs/zerolog"

	"github.com/input-output-hk/cicero/src/application"
	"github.com/input-output-hk/cicero/src/application/service"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
)

type ActionInvokeConsumer struct {
	Logger              zerolog.Logger
	Limiter             *priority.PriorityLimiter
	EvaluationService   service.EvaluationService
	ActionService       service.ActionService
	RunService          service.RunService
	MessageQueueService service.MessageQueueService
	Db                  config.PgxIface
	NomadClient         application.NomadClient
}

func (self *ActionInvokeConsumer) Start(ctx context.Context) error {
	self.Logger.Info().Msg("Starting")

	if err := self.MessageQueueService.Subscribe(ctx, domain.ActionInvokeStreamName, self.invokerSubscriber(ctx), 0); err != nil {
		return errors.WithMessagef(err, "Could not subscribe to stream %s", domain.ActionInvokeStreamName)
	}

	<-ctx.Done()
	self.Logger.Debug().Msg("context was cancelled")
	return nil
}

func (self *ActionInvokeConsumer) invokerSubscriber(ctx context.Context) func(*liftbridge.Message, error) {
	return func(msg *liftbridge.Message, err error) {
		if err != nil {
			self.Logger.Fatal().Err(err).Msg("error in liftbridge message")
			//TODO: If err is not nil, the subscription will be terminated
			return
		}

		self.Logger.Debug().
			Str("stream", msg.Stream()).
			Str("subject", msg.Subject()).
			Int64("offset", msg.Offset()).
			Str("value", string(msg.Value())).
			Time("time", msg.Timestamp()).
			Msg("Received message")

		if err := self.Db.BeginFunc(ctx, func(tx pgx.Tx) error {
			return self.processMessage(ctx, tx, msg)
		}); err != nil {
			self.Logger.Fatal().Err(err).Bytes("message", msg.Value()).Msg("Could not process message")
			return
		}
	}
}

func (self *ActionInvokeConsumer) getActionName(msg *liftbridge.Message) string {
	return strings.Split(msg.Subject(), ".")[1]
}

func (self *ActionInvokeConsumer) processMessage(ctx context.Context, tx pgx.Tx, msg *liftbridge.Message) error {
	if err := self.MessageQueueService.Save(tx, msg); err != nil {
		return errors.WithMessage(err, "Could not save message")
	}

	if action, err := self.ActionService.GetLatestByName(self.getActionName(msg)); err != nil {
		if errors.Is(err, pgx.ErrNoRows) {
			self.Logger.Info().Msg("No Action with that name, ignoring invoke message")
		} else {
			return err
		}
	} else if runnable, inputs, err := self.ActionService.IsRunnable(&action); err != nil {
		return err
	} else if runnable {
		self.Limiter.Wait(ctx, priority.High) // TODO: What is the ctx to use in this case?
		defer self.Limiter.Finish()

		if runDef, err := self.EvaluationService.EvaluateRun(action.Source, action.Name, action.ID, inputs); err != nil {
			var evalErr service.EvaluationError
			if errors.As(err, &evalErr) {
				self.Logger.Err(evalErr).Str("source", action.Source).Str("name", action.Name).Msg("Could not evaluate action")
			} else {
				return err
			}
		} else if runDef.IsDecision() {
			if runDef.Outputs.Success != nil {
				if factJson, err := json.Marshal(
					domain.Fact{Value: runDef.Outputs.Success},
				); err != nil {
					return errors.WithMessagef(err, "Could not marshal fact")
				} else if err := self.MessageQueueService.Publish(
					domain.FactCreateStreamName.String(),
					domain.FactCreateStreamName,
					factJson,
				); err != nil {
					return errors.WithMessage(err, "Could not publish fact")
				}
			}
		} else {
			run := domain.Run{
				ActionId:   action.ID,
				RunOutputs: runDef.Outputs,
			}

			if err := self.RunService.Save(tx, &run); err != nil {
				return errors.WithMessage(err, "Could not insert Run")
			}

			runId := run.NomadJobID.String()
			runDef.Job.ID = &runId

			if response, _, err := self.NomadClient.JobsRegister(runDef.Job, &nomad.WriteOptions{}); err != nil {
				return errors.WithMessage(err, "Failed to run Action")
			} else if len(response.Warnings) > 0 {
				self.Logger.Warn().
					Str("nomad-job", runId).
					Str("nomad-evaluation", response.EvalID).
					Str("warnings", response.Warnings).
					Msg("Warnings occured registering Nomad job")
			}
		}
	}

	return nil
}
