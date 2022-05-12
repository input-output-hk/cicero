package main

import (
	"fmt"
	"log"
	"net"
	"net/http"
	"os"
	"strconv"
	"testing"
	"time"

	"github.com/miekg/dns"
	"github.com/rs/zerolog"
	"github.com/steinfletcher/apitest"
)

func TestMain(m *testing.M) {
	go startDnsServer()

	for {
		_, err := net.Dial("udp", "127.0.0.1:10053")
		if err == nil {
			break
		}
		time.Sleep(1 * time.Millisecond)
	}
	os.Exit(m.Run())
}

func TestServer(t *testing.T) {
	fixture := "cicero_ping.json"
	secret := []byte("foobar")
	hash := "26f0d79bd4b0aafbe0c6469bb3ee761d5b0e88d2c2387655178981841d4ea9a1"

	h := handler{
		&dns.ClientConfig{
			Servers: []string{"127.0.0.1"},
			Port:    "10053",
		},
		secret,
		zerolog.Nop(),
		// zerolog.New(os.Stderr).With().Timestamp().Logger(),
	}

	mock := apitest.NewMock().
		Post("/fact").
		RespondWith().
		Body(`happy path`).
		End()

	apitest.New().Handler(h).
		Mocks(mock).
		Method("POST").
		URL("/").
		Header("X-Hub-Signature-256", "sha256="+hash).
		Header("X-GitHub-Event", "push").
		BodyFromFile(fixture).
		Expect(t).
		Header("Content-Type", "text/plain; charset=utf-8").
		Body(`ok`).
		Status(http.StatusOK).
		End()
}

func startDnsServer() {
	parseQuery := func(m *dns.Msg) {
		for _, q := range m.Question {
			switch q.Qtype {
			default:
				rr, err := dns.NewRR(fmt.Sprintf("%s A %s", q.Name, "127.0.0.1"))
				if err != nil {
					log.Fatalf("failed to create RR: %s\n", err)
				}
				m.Answer = append(m.Answer, rr)
			case dns.TypeSRV:
				priority := 1
				weight := 1
				port := 1234
				for _, host := range []string{"0a187988.addr.eu-central-1.consul", "0a187989.addr.eu-central-1.consul"} {
					rr, err := dns.NewRR(fmt.Sprintf("%s SRV %d %d %d %s", q.Name, priority, weight, port, host))
					if err != nil {
						log.Fatalf("failed to create RR: %s\n", err)
					}
					m.Answer = append(m.Answer, rr)
				}
			}
		}
	}

	handleDnsRequest := func(w dns.ResponseWriter, r *dns.Msg) {
		m := &dns.Msg{}
		m.SetReply(r)
		m.Compress = false

		if r.Opcode == dns.OpcodeQuery {
			parseQuery(m)
		}

		_ = w.WriteMsg(m)
	}

	dns.HandleFunc("consul.", handleDnsRequest)

	port := 10053
	server := &dns.Server{Addr: ":" + strconv.Itoa(port), Net: "udp"}
	log.Printf("Starting at %d\n", port)
	err := server.ListenAndServe()
	if err != nil {
		log.Fatalf("Failed to start server: %s\n ", err.Error())
	}
}
