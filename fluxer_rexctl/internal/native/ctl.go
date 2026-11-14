// SPDX-License-Identifier: AGPL-3.0-or-later

package native

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/url"
	"os/exec"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const (
	pathPs  = "/v1/ps"
	pathSvc = "/v1/svc/"

	ActionStart   = "start"
	ActionStop    = "stop"
	ActionRestart = "restart"

	rustProcessEnv = "FLUXER_RUST_PROCESS"

	stateWaiting    = "waiting"
	stateStarting   = "starting"
	stateRunning    = "running"
	stateRestarting = "restarting"
	stateCrashloop  = "crashloop"
	stateStopping   = "stopping"
	stateStopped    = "stopped"
	stateExited     = "exited"

	Supervisor = "fluxerd"

	unitCrashRestarts = 3
	readyInterval     = 5 * time.Second
)

type PsEntry struct {
	Name      string       `json:"name"`
	State     string       `json:"state"`
	Health    string       `json:"health"`
	Restarts  int          `json:"restarts"`
	PID       int          `json:"pid"`
	StartedAt time.Time    `json:"started_at"`
	ExitCode  int          `json:"exit_code"`
	Core      bool         `json:"core"`
	Oneshot   bool         `json:"oneshot"`
	Watch     []WatchEntry `json:"watch,omitempty"`
}

type WatchEntry struct {
	Name   string `json:"name"`
	Health string `json:"health"`
	Error  string `json:"error,omitempty"`
}

var ErrNoSupervisor = errors.New("native: fluxerd isn't answering")

func (s *Stack) client() *http.Client {
	socket := s.ControlSocket()
	return &http.Client{
		Timeout: 30 * time.Second,
		Transport: &http.Transport{DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) {
			var d net.Dialer
			return d.DialContext(ctx, "unix", socket)
		}},
	}
}

func (s *Stack) call(ctx context.Context, method, path string, out any) error {
	req, err := http.NewRequestWithContext(ctx, method, "http://fluxerd"+path, nil)
	if err != nil {
		return err
	}
	resp, err := s.client().Do(req)
	if err != nil {
		if ctx.Err() != nil {
			return ctx.Err()
		}
		return fmt.Errorf("%w: %v", ErrNoSupervisor, err)
	}
	defer resp.Body.Close()
	data, err := io.ReadAll(io.LimitReader(resp.Body, 4<<20))
	if err != nil {
		return err
	}
	if resp.StatusCode >= 300 {
		return fmt.Errorf("native: fluxerd answered %s %s with %d: %s", method, path, resp.StatusCode, strings.TrimSpace(string(data)))
	}
	if out == nil {
		return nil
	}
	return json.Unmarshal(data, out)
}

func (s *Stack) Entries(ctx context.Context) ([]PsEntry, error) {
	var out []PsEntry
	if err := s.call(ctx, http.MethodGet, pathPs, &out); err != nil {
		return nil, err
	}
	return out, nil
}

func (s *Stack) PS(ctx context.Context) ([]engine.Container, error) {
	entries, err := s.Entries(ctx)
	if err != nil {
		return nil, err
	}
	out := make([]engine.Container, 0, len(entries))
	for _, e := range entries {
		out = append(out, Container(e))
	}
	return out, nil
}

func Container(e PsEntry) engine.Container {
	c := engine.Container{ID: e.Name, Name: e.Name, Service: e.Name, Health: e.Health, ExitCode: e.ExitCode, RestartCount: e.Restarts, Created: e.StartedAt}
	switch e.State {
	case stateRunning:
		c.State = "running"
	case stateStarting:
		c.State = "running"
		if c.Health == "" {
			c.Health = "starting"
		}
	case stateRestarting, stateCrashloop:
		c.State = "restarting"
	case stateWaiting:
		c.State = "created"
	case stateStopping, stateStopped, stateExited:
		c.State = "exited"
	default:
		c.State = e.State
	}
	return c
}

func (s *Stack) Service(ctx context.Context, name, action string) error {
	return s.call(ctx, http.MethodPost, pathSvc+url.PathEscape(name)+"/"+action, nil)
}

func (s *Stack) eachService(ctx context.Context, action string, services []string) error {
	single := s.RustSingle()
	var done []string
	for _, name := range services {
		name = stack.RunsIn(name, single)
		if slices.Contains(done, name) {
			continue
		}
		done = append(done, name)
		if err := s.Service(ctx, name, action); err != nil {
			return err
		}
	}
	return nil
}

func (s *Stack) RustSingle() bool {
	if s.Inst.State != nil && stack.RustSingle(s.Inst.State.Settings) {
		return true
	}
	f, err := envfile.Load(s.EnvFile())
	return err == nil && strings.TrimSpace(f.Value(rustProcessEnv)) == "single"
}

func (s *Stack) Start(ctx context.Context, services ...string) error {
	if len(services) == 0 {
		return systemctl(ctx, "start", "--no-block", s.Unit())
	}
	return s.eachService(ctx, ActionStart, services)
}

func (s *Stack) StartStopped(ctx context.Context) error {
	entries, err := s.Entries(ctx)
	if err != nil {
		return err
	}
	var stopped []string
	for _, e := range entries {
		if e.State == stateStopped {
			stopped = append(stopped, e.Name)
		}
	}
	if len(stopped) == 0 {
		return nil
	}
	return s.Start(ctx, stopped...)
}

func (s *Stack) Stop(ctx context.Context, services ...string) error {
	if len(services) == 0 {
		return systemctl(ctx, "stop", s.Unit())
	}
	return s.eachService(ctx, ActionStop, services)
}

func (s *Stack) OwnsUnit(ctx context.Context) bool {
	return unitProperty(ctx, s.Unit(), "Description") == "Fluxer in "+s.Inst.Dir
}

func (s *Stack) Restart(ctx context.Context, services ...string) error {
	if len(services) == 0 {
		s.replaced = unitProperty(ctx, s.Unit(), "InvocationID")
		return systemctl(ctx, "restart", "--no-block", s.Unit())
	}
	return s.eachService(ctx, ActionRestart, services)
}

func (s *Stack) Disable(ctx context.Context) error {
	return systemctl(ctx, "disable", s.Unit())
}

var systemctlCmd = func(ctx context.Context, args ...string) ([]byte, error) {
	return exec.CommandContext(ctx, "systemctl", args...).CombinedOutput()
}

func systemctl(ctx context.Context, args ...string) error {
	out, err := systemctlCmd(ctx, args...)
	if err != nil {
		return fmt.Errorf("systemctl %s: %w: %s", strings.Join(args, " "), err, strings.TrimSpace(string(out)))
	}
	return nil
}

func unitProperty(ctx context.Context, unit, name string) string {
	out, _ := systemctlCmd(ctx, "show", "-p", name, "--value", unit)
	return strings.TrimSpace(string(out))
}

func unitActive(ctx context.Context, unit string) bool {
	return unitProperty(ctx, unit, "ActiveState") == "active"
}

func (s *Stack) ActiveState(ctx context.Context) string {
	return unitProperty(ctx, s.Unit(), "ActiveState")
}

func (s *Stack) UnitRestarts(ctx context.Context) int {
	n, _ := strconv.Atoi(unitProperty(ctx, s.Unit(), "NRestarts"))
	return n
}

type Status struct {
	Unit       string
	Restarts   int
	Readiness  engine.Readiness
	Containers []engine.Container
	Crashing   []string
	Fatal      bool
	PSErr      error
}

func (s *Stack) Status(ctx context.Context, skip ...string) Status {
	st := Status{Unit: s.ActiveState(ctx), Restarts: s.UnitRestarts(ctx)}
	if st.Unit == "active" && s.replaced != "" && unitProperty(ctx, s.Unit(), "InvocationID") == s.replaced {
		st.Unit = "restarting"
	}
	entries, err := s.Entries(ctx)
	if err != nil {
		st.PSErr = err
		st.Readiness = engine.Readiness{Ready: []string{}, NotReady: []string{Supervisor}, Failed: []string{}}
		if st.Unit == "failed" {
			st.Readiness.NotReady, st.Readiness.Failed = []string{}, []string{Supervisor}
			st.Fatal = true
		}
		return st
	}
	var services []string
	for _, e := range entries {
		if slices.Contains(skip, e.Name) {
			continue
		}
		services = append(services, e.Name)
		st.Containers = append(st.Containers, Container(e))
		if e.State == stateCrashloop {
			st.Crashing = append(st.Crashing, e.Name)
		}
	}
	st.Readiness, st.Fatal = engine.Assess(services, st.Containers)
	if st.Unit != "active" && len(st.Readiness.NotReady)+len(st.Readiness.Failed) == 0 {
		st.Readiness.NotReady = append(st.Readiness.NotReady, Supervisor)
	}
	slices.Sort(st.Crashing)
	return st
}

func (st Status) Ready() bool {
	return st.PSErr == nil && st.Unit == "active" && len(st.Readiness.NotReady) == 0 && len(st.Readiness.Failed) == 0
}

func (s *Stack) WaitReady(ctx context.Context, timeout time.Duration, onTick func(engine.Readiness), skip ...string) error {
	if timeout <= 0 {
		timeout = engine.DefaultReadyTimeout
	}
	deadline := time.Now().Add(timeout)
	base := -1
	ticker := time.NewTicker(readyInterval)
	defer ticker.Stop()
	for {
		st := s.Status(ctx, skip...)
		if base < 0 {
			base = st.Restarts
		}
		if onTick != nil {
			onTick(st.Readiness)
		}
		if st.Ready() {
			return nil
		}
		crashing := st.Crashing
		if st.Restarts-base >= unitCrashRestarts {
			crashing = append(crashing, Supervisor)
		}
		if st.Fatal || len(crashing) > 0 || time.Now().After(deadline) {
			return &engine.NotReadyError{Readiness: st.Readiness, Fatal: st.Fatal, Crashing: crashing}
		}
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-ticker.C:
		}
	}
}

func (s *Stack) Crashing(ctx context.Context) error {
	st := s.Status(ctx)
	if len(st.Crashing) > 0 || st.Unit == "failed" {
		crashing := st.Crashing
		if st.Unit == "failed" {
			crashing = append(crashing, Supervisor)
		}
		return &engine.NotReadyError{Readiness: st.Readiness, Fatal: st.Fatal, Crashing: crashing}
	}
	return nil
}

var journalctlCmd = func(ctx context.Context, args []string, stdout io.Writer) error {
	cmd := exec.CommandContext(ctx, "journalctl", args...)
	var stderr bytes.Buffer
	cmd.Stdout, cmd.Stderr = stdout, &stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("journalctl: %w: %s", err, strings.TrimSpace(stderr.String()))
	}
	return nil
}

var logAliases = map[string][]string{
	"edge":         {Supervisor},
	"nats":         {Supervisor},
	"worker":       {"fluxer-api"},
	"static-proxy": {"fluxer-app-proxy"},
	"seaweedfs":    {"fluxer-seaweedfs", "fluxer-seaweedfs-filer"},
}

func LogsArgs(unit string, opts engine.LogsOptions, services ...string) []string {
	args := []string{"-u", unit, "--no-pager"}
	var ids []string
	for _, svc := range services {
		aliases, ok := logAliases[svc]
		if !ok {
			aliases = []string{"fluxer-" + svc}
		}
		for _, id := range aliases {
			if !slices.Contains(ids, id) {
				ids = append(ids, id)
				args = append(args, "-t", id)
			}
		}
	}
	if opts.Follow {
		args = append(args, "-f")
	}
	if opts.Since != "" {
		since := opts.Since
		if _, err := time.ParseDuration(since); err == nil {
			since = "-" + since
		}
		args = append(args, "--since", since)
	}
	if opts.Tail > 0 {
		args = append(args, "-n", strconv.Itoa(opts.Tail))
	}
	return args
}

func (s *Stack) Logs(ctx context.Context, opts engine.LogsOptions, services ...string) error {
	return journalctlCmd(ctx, LogsArgs(s.Unit(), opts, services...), opts.Stdout)
}
