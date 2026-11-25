// SPDX-License-Identifier: AGPL-3.0-or-later

package native

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"net"
	"net/http"
	"net/url"
	"os"
	"os/exec"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const (
	pathPs   = "/v1/ps"
	pathSvc  = "/v1/svc/"
	pathLogs = "/v1/logs/"

	actionStart   = "start"
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

	healthStarting  = "starting"
	healthHealthy   = "healthy"
	healthUnhealthy = "unhealthy"

	Supervisor = "fluxerd"

	stoppedMarkerName = "stopped"

	unitCrashRestarts = 3
	readyInterval     = 5 * time.Second
	eventGap          = time.Second
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
	Core   bool   `json:"core,omitempty"`
}

var errNoSupervisor = errors.New("native: fluxerd isn't responding")

const callTimeout = 30 * time.Second

func (s *Stack) client(timeout time.Duration) *http.Client {
	if s.Dial != nil {
		return &http.Client{
			Timeout: timeout,
			Transport: &http.Transport{DisableKeepAlives: true, DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) {
				c, err := s.Dial(ctx, DialControl)
				if err != nil {
					return nil, &net.OpError{Op: "dial", Net: "exec", Err: err}
				}
				return c, nil
			}},
		}
	}
	socket := s.ControlSocket()
	return &http.Client{
		Timeout: timeout,
		Transport: &http.Transport{DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) {
			var d net.Dialer
			return d.DialContext(ctx, "unix", socket)
		}},
	}
}

func (s *Stack) UnitState(ctx context.Context) string {
	state, _ := s.hostState(ctx)
	return state
}

func (s *Stack) hostState(ctx context.Context) (string, int) {
	if s.Host != nil {
		return s.Host(ctx)
	}
	return s.ActiveState(ctx), s.UnitRestarts(ctx)
}

func requestFailed(ctx context.Context, err error) error {
	if ctx.Err() != nil {
		return ctx.Err()
	}
	var op *net.OpError
	if errors.As(err, &op) && op.Op == "dial" {
		return fmt.Errorf("%w: %v", errNoSupervisor, err)
	}
	return err
}

func (s *Stack) call(ctx context.Context, method, path string, out any) error {
	return s.callWithin(ctx, callTimeout, method, path, out)
}

func (s *Stack) callWithin(ctx context.Context, timeout time.Duration, method, path string, out any) error {
	req, err := http.NewRequestWithContext(ctx, method, "http://fluxerd"+path, nil)
	if err != nil {
		return err
	}
	resp, err := s.client(timeout).Do(req)
	if err != nil {
		return requestFailed(ctx, err)
	}
	defer resp.Body.Close()
	data, err := io.ReadAll(io.LimitReader(resp.Body, 4<<20))
	if err != nil {
		return err
	}
	if resp.StatusCode >= 300 {
		return &statusError{method: method, path: path, code: resp.StatusCode, body: strings.TrimSpace(string(data))}
	}
	if out == nil {
		return nil
	}
	return json.Unmarshal(data, out)
}

type statusError struct {
	method, path string
	code         int
	body         string
}

func (e *statusError) Error() string {
	return fmt.Sprintf("native: fluxerd returned %[3]d for %[1]s %[2]s: %[4]s", e.method, e.path, e.code, e.body)
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
			c.Health = healthStarting
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
	env, err := envfile.LoadRaw(s.EnvFile())
	return err == nil && strings.TrimSpace(env[rustProcessEnv]) == "single"
}

func (s *Stack) StoppedMarker() string { return s.Inst.Path(instance.StateDirName, stoppedMarkerName) }

func (s *Stack) clearStopped() error {
	if err := os.Remove(s.StoppedMarker()); err != nil && !errors.Is(err, fs.ErrNotExist) {
		return err
	}
	return nil
}

func (s *Stack) Start(ctx context.Context, services ...string) error {
	if len(services) == 0 {
		if err := s.clearStopped(); err != nil {
			return err
		}
		return systemctl(ctx, "start", "--no-block", s.Unit())
	}
	return s.eachService(ctx, actionStart, services)
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
		if err := s.Halt(ctx); err != nil {
			return err
		}
		return os.WriteFile(s.StoppedMarker(), nil, 0o600)
	}
	return s.eachService(ctx, ActionStop, services)
}

func (s *Stack) Halt(ctx context.Context) error {
	return systemctl(ctx, "stop", s.Unit())
}

func (s *Stack) OwnsUnit(ctx context.Context) bool {
	d := unitProperty(ctx, s.Unit(), "Description")
	return d == "Fluxer in "+s.Inst.Dir || d == "Fluxer voice servers in "+s.Inst.Dir
}

func (s *Stack) Restart(ctx context.Context, services ...string) error {
	if len(services) == 0 {
		if err := s.clearStopped(); err != nil {
			return err
		}
		s.replaced = unitProperty(ctx, s.Unit(), "InvocationID")
		return systemctl(ctx, "restart", "--no-block", s.Unit())
	}
	return s.eachService(ctx, ActionRestart, services)
}

func (s *Stack) Disable(ctx context.Context) error {
	return systemctl(ctx, "disable", s.Unit())
}

func (s *Stack) Enable(ctx context.Context) error {
	return systemctl(ctx, "enable", s.Unit())
}

func (s *Stack) Enabled(ctx context.Context) bool {
	return unitProperty(ctx, s.Unit(), "UnitFileState") == "enabled"
}

func systemctlCmd(ctx context.Context, args ...string) ([]byte, error) {
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

func (s *Stack) ActiveState(ctx context.Context) string {
	if s.Host != nil {
		state, _ := s.Host(ctx)
		return state
	}
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

func CrashLooping(unit string, restarts int) bool {
	return unit == "activating" && restarts >= unitCrashRestarts
}

func (s *Stack) Status(ctx context.Context, skip ...string) Status {
	st := Status{}
	st.Unit, st.Restarts = s.hostState(ctx)
	if st.Unit == "active" && s.Host == nil && s.replaced != "" && unitProperty(ctx, s.Unit(), "InvocationID") == s.replaced {
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
	var gating []engine.Container
	for _, e := range entries {
		if slices.Contains(skip, e.Name) {
			continue
		}
		c := Container(e)
		for _, w := range e.Watch {
			switch {
			case w.Health == healthHealthy, !w.Core:
			case c.Health == healthHealthy && w.Health == healthUnhealthy:
				c.Health = healthUnhealthy
			case c.Health == healthHealthy:
				c.Health = healthStarting
			}
		}
		st.Containers = append(st.Containers, c)
		st.Containers = append(st.Containers, watched(e, Container(e))...)
		if !e.Core && !e.Oneshot {
			continue
		}
		services = append(services, e.Name)
		gating = append(gating, c)
		if e.State == stateCrashloop {
			st.Crashing = append(st.Crashing, e.Name)
		}
	}
	st.Readiness, st.Fatal = engine.Assess(services, gating)
	if st.Unit != "active" && st.Unit != "failed" {
		st.Readiness.NotReady = slices.Sorted(slices.Values(append(st.Readiness.NotReady, st.Readiness.Failed...)))
		st.Readiness.Failed = []string{}
		st.Fatal = false
	}
	if st.Unit != "active" && len(st.Readiness.NotReady)+len(st.Readiness.Failed) == 0 {
		st.Readiness.NotReady = append(st.Readiness.NotReady, Supervisor)
	}
	slices.Sort(st.Crashing)
	return st
}

func watched(e PsEntry, parent engine.Container) []engine.Container {
	var out []engine.Container
	index := map[string]int{}
	for _, w := range e.Watch {
		if w.Name == e.Name {
			continue
		}
		c := parent
		c.ID, c.Name, c.Service, c.RestartCount = w.Name, w.Name, w.Name, 0
		if parent.State == "running" {
			switch {
			case w.Health == healthHealthy:
				c.Health = healthHealthy
			case w.Health == healthUnhealthy && w.Core:
				c.State, c.Health = "restarting", healthUnhealthy
			case w.Health == healthUnhealthy:
				c.Health = healthUnhealthy
			default:
				c.Health = healthStarting
			}
		}
		i, ok := index[w.Name]
		if !ok {
			index[w.Name] = len(out)
			out = append(out, c)
			continue
		}
		if watchRank(c) > watchRank(out[i]) {
			out[i] = c
		}
	}
	return out
}

func watchRank(c engine.Container) int {
	switch {
	case c.State != "running":
		return 3
	case c.Health == healthUnhealthy:
		return 2
	case c.Health == healthStarting:
		return 1
	}
	return 0
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
	watchCtx, stopWatch := context.WithCancel(ctx)
	defer stopWatch()
	kick := make(chan struct{}, 1)
	go s.watchEvents(watchCtx, kick)
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
		checked := time.Now()
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-ticker.C:
		case <-kick:
			select {
			case <-ctx.Done():
				return ctx.Err()
			case <-time.After(time.Until(checked.Add(eventGap))):
			}
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

func Journal(ctx context.Context, args []string, stdout io.Writer) error {
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
	if s.Dial != nil {
		return s.socketLogs(ctx, opts, services)
	}
	return Journal(ctx, LogsArgs(s.Unit(), opts, services...), opts.Stdout)
}

func (s *Stack) socketLogs(ctx context.Context, opts engine.LogsOptions, services []string) error {
	for _, svc := range services {
		path := pathLogs + url.PathEscape(svc)
		if opts.Follow {
			path += "?follow=1"
		}
		req, err := http.NewRequestWithContext(ctx, http.MethodGet, "http://fluxerd"+path, nil)
		if err != nil {
			return err
		}
		timeout := callTimeout
		if opts.Follow {
			timeout = 0
		}
		resp, err := s.client(timeout).Do(req)
		if err != nil {
			return requestFailed(ctx, err)
		}
		if resp.StatusCode >= 300 {
			data, _ := io.ReadAll(io.LimitReader(resp.Body, 4096))
			resp.Body.Close()
			return &statusError{method: http.MethodGet, path: path, code: resp.StatusCode, body: strings.TrimSpace(string(data))}
		}
		if opts.Follow || opts.Tail <= 0 {
			_, err = io.Copy(opts.Stdout, resp.Body)
		} else {
			err = lastLines(opts.Stdout, resp.Body, opts.Tail)
		}
		resp.Body.Close()
		if err != nil && ctx.Err() == nil {
			return err
		}
	}
	return nil
}

func lastLines(w io.Writer, r io.Reader, n int) error {
	data, err := io.ReadAll(r)
	if err != nil {
		return err
	}
	lines := strings.SplitAfter(string(data), "\n")
	if lines[len(lines)-1] == "" {
		lines = lines[:len(lines)-1]
	}
	_, err = io.WriteString(w, strings.Join(lines[max(0, len(lines)-n):], ""))
	return err
}
