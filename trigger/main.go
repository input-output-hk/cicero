package main

import (
	"bytes"
	"crypto/hmac"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"strings"
	"time"

	"github.com/alexflint/go-arg"
	"github.com/miekg/dns"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"
)

type Config struct {
	Host         string `arg:"--host" default:"0.0.0.0"`
	Port         int    `arg:"--port, env:NOMAD_PORT_http" default:"8099"`
	SecretPath   string `arg:"--secret-file, required"`
	CiceroApiUrl string `arg:"--cicero-api-url"`
	ResolvConf   string `arg:"--resolv-conf" default:"/etc/resolv.conf"`
	LogLevel     string `arg:"--log-level" default:"info"`
}

func main() {
	config := Config{}
	arg.MustParse(&config)

	log := zerolog.New(os.Stderr).With().Timestamp().Logger()

	if level, err := zerolog.ParseLevel(config.LogLevel); err != nil {
		log.Fatal().Err(err).Msg("parsing log level")
	} else {
		zerolog.SetGlobalLevel(level)
	}

	clientConfig, err := dns.ClientConfigFromFile(config.ResolvConf)
	if err != nil {
		log.Fatal().Err(err).Msg("reading resolv config")
	}

	secret, err := os.ReadFile(config.SecretPath)
	if err != nil {
		log.Fatal().Err(err).Msg("reading secret file")
	}
	secret = bytes.TrimSuffix(secret, []byte{'\n'})

	server := http.Server{
		Addr:         fmt.Sprintf("%s:%d", config.Host, config.Port),
		ReadTimeout:  5 * time.Second,
		WriteTimeout: 10 * time.Second,
		Handler:      handler{clientConfig, secret, log, config.CiceroApiUrl},
	}

	log.Info().Str("addr", server.Addr).Msg("Starting server")
	if err := server.ListenAndServe(); err != nil {
		log.Fatal().Err(err).Msg("starting server")
	}
}

type handler struct {
	clientConfig *dns.ClientConfig
	secret       []byte
	log          zerolog.Logger
	ciceroApiUrl string
}

func fail(w http.ResponseWriter, err error, status int) bool {
	if err != nil {
		w.WriteHeader(status)
		_, _ = fmt.Fprint(w, err.Error())
		return true
	}
	return false
}

const acceptedSignature = "sha256"
const MiB = 1048576

func (h handler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	eventType := r.Header.Get("X-GitHub-Event")

	h.log.Info().Str("method", r.Method).Str("event", eventType).Msg("request")

	// We limit body sizes to 1MiB, hopefully that is enough
	body, err := io.ReadAll(io.LimitReader(r.Body, MiB))
	if fail(w, errors.WithMessage(err, "reading body"), 400) {
		return
	}

	err = h.validateSignature(
		r.Header.Get("X-Hub-Signature-256"),
		body,
	)
	if fail(w, errors.WithMessage(err, "HMAC invalid"), 400) {
		return
	}

	apiAddr := h.ciceroApiUrl
	if apiAddr == "" {
		apiAddr, err = h.lookupSRV("_cicero._tcp.service.consul")
		if fail(w, errors.WithMessage(err, "Looking up DNS"), 500) {
			return
		}
		apiAddr = fmt.Sprintf("http://%s/api", apiAddr)
	}

	event := map[string]interface{}{}
	err = json.Unmarshal(body, &event)
	if fail(w, errors.WithMessage(err, "unmarshal body"), 400) {
		return
	}

	buf := &bytes.Buffer{}
	enc := json.NewEncoder(buf)

	err = enc.Encode(map[string]interface{}{
		"github_event": eventType,
		"github_body":  event,
	})
	if fail(w, errors.WithMessage(err, "marshal payload"), 500) {
		return
	}

	_, err = http.Post(apiAddr+"/fact", "text/json", buf)
	if fail(w, errors.WithMessage(err, "creating fact"), 500) {
		return
	}

	fmt.Fprint(w, "ok")
}

// TODO: should add retries, Ndot handling, local search, etc...
// maybe replace with https://github.com/benschw/srv-lb
func (h handler) lookupSRV(query string) (string, error) {
	server := fmt.Sprintf("%s:%s", h.clientConfig.Servers[0], h.clientConfig.Port)

	m := &dns.Msg{}
	m.SetQuestion(dns.Fqdn(query), dns.TypeSRV)
	c := &dns.Client{}
	in, _, err := c.Exchange(m, server)
	if err != nil {
		return "", errors.WithMessage(err, "looking up SRV record")
	}

	for _, answer := range in.Answer {
		if srv, ok := answer.(*dns.SRV); ok {
			m := &dns.Msg{}
			m.SetQuestion(dns.Fqdn(srv.Target), dns.TypeA)
			in, _, err := c.Exchange(m, server)
			if err != nil {
				return "", errors.WithMessage(err, "looking up A record")
			}
			for _, answer := range in.Answer {
				if a, ok := answer.(*dns.A); ok {
					return fmt.Sprintf("%s:%d", a.A, srv.Port), nil
				}
			}
		}
	}

	return "", errors.New("No DNS record found")
}

func (h handler) validateSignature(signatureRaw string, body []byte) error {
	signatureParts := strings.Split(signatureRaw, "=")
	if len(signatureParts) != 2 {
		return fmt.Errorf("Invalid signature %q", signatureRaw)
	}

	signatureType := signatureParts[0]

	if signatureType != acceptedSignature {
		return fmt.Errorf("HMAC Signature type unexpected %q != %q", signatureType, acceptedSignature)
	}

	msgMac, err := hex.DecodeString(signatureParts[1])
	if err != nil {
		return errors.WithMessage(err, "failed to decode header signature")
	}

	ok, err := h.checkMac(body, msgMac, h.secret)
	if err != nil {
		return err
	} else if !ok {
		return fmt.Errorf("HMAC message digest or secret invalid")
	}

	return nil
}

func (h handler) checkMac(msg, msgMac, key []byte) (bool, error) {
	mac := hmac.New(sha256.New, key)
	n, err := mac.Write(msg)
	if err != nil {
		return false, errors.WithMessage(err, "failed to calculate")
	} else if n != len(msg) {
		return false, fmt.Errorf("hashed %d of %d bytes", n, len(msg))
	}

	hash := mac.Sum(nil)

	if hmac.Equal(msgMac, hash) {
		return true, nil
	}

	h.log.Info().
		Bytes("msg", msg).
		Str("msgMac", fmt.Sprintf("%x", msgMac)).
		Str("hash", fmt.Sprintf("%x", hash)).
		Send()

	return false, nil
}
