package config

import (
	"time"

	"github.com/go-kit/log"
	"github.com/grafana/dskit/backoff"
	"github.com/grafana/dskit/flagext"
	promtailClient "github.com/grafana/loki/clients/pkg/promtail/client"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/rs/zerolog"
)

func NewPromtailClient(prometheusAddr string, logger *zerolog.Logger) (client promtailClient.Client, err error) {
	var url flagext.URLValue
	err = url.Set(prometheusAddr + "/loki/api/v1/push")
	if err != nil {
		return
	}

	contextualLogger := logger.With().Str("client", "promtail").Logger()

	streamLagLabels := []string{}
	client, err = promtailClient.New(
		promtailClient.NewMetrics(
			prometheus.NewRegistry(),
			streamLagLabels,
		),
		promtailClient.Config{
			URL:           url,
			BatchWait:     100 * time.Millisecond,
			BatchSize:     100,
			BackoffConfig: backoff.Config{MinBackoff: 1 * time.Millisecond, MaxBackoff: 2 * time.Millisecond, MaxRetries: 3},
			Timeout:       1 * time.Second,
		},
		streamLagLabels,
		log.LoggerFunc(func(keyvals ...interface{}) error {
			contextualLogger.Trace().Fields(keyvals).Send()
			return nil
		}),
	)
	return
}
