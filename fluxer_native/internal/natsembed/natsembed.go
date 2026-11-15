// SPDX-License-Identifier: AGPL-3.0-or-later

package natsembed

import (
	"errors"
	"fmt"
	"time"

	"github.com/nats-io/nats-server/v2/server"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

const readyTimeout = 10 * time.Second

type Options struct {
	StoreDir       string
	Token          string
	Port           int
	MaxMemoryStore string
	MaxFileStore   string
}

type Server struct {
	*server.Server
	jetstream server.JetStreamConfig
}

func Start(o Options) (*Server, error) {
	if o.Token == "" {
		return nil, errors.New("nats: FLUXER_NATS_AUTH_TOKEN is empty")
	}
	opts := &server.Options{}
	limits := fmt.Sprintf("jetstream {\n  max_memory_store: %s\n  max_file_store: %s\n}\n", o.MaxMemoryStore, o.MaxFileStore)
	if err := opts.ProcessConfigString(limits); err != nil {
		return nil, fmt.Errorf("nats: jetstream limits: %w", err)
	}
	opts.Host = spec.Loopback
	opts.Port = o.Port
	opts.Authorization = o.Token
	opts.JetStream = true
	opts.StoreDir = o.StoreDir
	opts.NoSigs = true
	s, err := server.NewServer(opts)
	if err != nil {
		return nil, fmt.Errorf("nats: %w", err)
	}
	s.ConfigureLogger()
	s.Start()
	if !s.ReadyForConnections(readyTimeout) {
		s.Shutdown()
		s.WaitForShutdown()
		return nil, fmt.Errorf("nats: not ready for connections after %s", readyTimeout)
	}
	if !s.JetStreamEnabled() {
		s.Shutdown()
		s.WaitForShutdown()
		return nil, errors.New("nats: JetStream did not start")
	}
	cfg := *s.JetStreamConfig()
	cfg.StoreDir = o.StoreDir
	return &Server{Server: s, jetstream: cfg}, nil
}

func (s *Server) SetJetStream(on bool) error {
	if !on {
		return s.DisableJetStream()
	}
	if s.JetStreamEnabled() {
		return nil
	}
	cfg := s.jetstream
	if err := s.EnableJetStream(&cfg); err != nil {
		return fmt.Errorf("nats: starting JetStream again: %w", err)
	}
	return nil
}
