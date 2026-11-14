// SPDX-License-Identifier: AGPL-3.0-or-later

package supervise

import (
	"cmp"
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"sync"
	"syscall"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/logs"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

var (
	ErrCrashloop = errors.New("supervise: a child fluxerd cannot run without is crash looping")
	ErrUnknown   = errors.New("supervise: no such child")
	ErrStopping  = errors.New("supervise: fluxerd is shutting down")
	ErrAction    = errors.New("supervise: unknown action")
)

type Timing struct {
	BackoffMin     time.Duration
	BackoffMax     time.Duration
	StableAfter    time.Duration
	CrashWindow    time.Duration
	CrashLimit     int
	ProbeEvery     time.Duration
	HealthEvery    time.Duration
	ProbeTimeout   time.Duration
	UnhealthyAfter int
	WatchGrace     time.Duration
	StopTimeout    time.Duration
	ShutdownBudget time.Duration
}

var DefaultTiming = Timing{
	BackoffMin:     time.Second,
	BackoffMax:     30 * time.Second,
	StableAfter:    time.Minute,
	CrashWindow:    5 * time.Minute,
	CrashLimit:     5,
	ProbeEvery:     time.Second,
	HealthEvery:    10 * time.Second,
	ProbeTimeout:   5 * time.Second,
	UnhealthyAfter: 3,
	WatchGrace:     time.Minute,
	StopTimeout:    10 * time.Second,
	ShutdownBudget: 80 * time.Second,
}

type Options struct {
	Out     *logs.Output
	OnReady func()
	Timing  Timing
}

type request struct {
	action  string
	timeout time.Duration
	done    chan struct{}
}

type proc struct {
	c     spec.Child
	ring  *logs.Ring
	reqs  chan request
	watch []*watched

	state     spec.State
	health    spec.HealthState
	restarts  int
	pid       int
	gen       int
	startedAt time.Time
	exitCode  int
	crashes   []time.Time
}

type Supervisor struct {
	out     *logs.Output
	onReady func()
	t       Timing
	procs   []*proc
	byName  map[string]*proc
	levels  [][]*proc

	mu       sync.Mutex
	changed  chan struct{}
	stopping bool
	readied  bool
	fatal    chan error
	quit     chan struct{}
	wg       sync.WaitGroup
}

func New(children []spec.Child, opt Options) (*Supervisor, error) {
	s := &Supervisor{
		out:     opt.Out,
		onReady: opt.OnReady,
		t:       opt.Timing,
		byName:  map[string]*proc{},
		changed: make(chan struct{}),
		fatal:   make(chan error, 1),
		quit:    make(chan struct{}),
	}
	if s.out == nil {
		s.out = logs.NewWriter(os.Stdout)
	}
	if s.t == (Timing{}) {
		s.t = DefaultTiming
	}
	for _, c := range children {
		if c.Name == "" || len(c.Argv) == 0 {
			return nil, fmt.Errorf("supervise: child %q has no name or argv", c.Name)
		}
		if _, dup := s.byName[c.Name]; dup {
			return nil, fmt.Errorf("supervise: child %q is listed twice", c.Name)
		}
		p := &proc{c: c, ring: logs.NewRing(logs.RingLines), reqs: make(chan request), state: spec.StateWaiting, watch: groupWatch(c.Watch)}
		s.procs = append(s.procs, p)
		s.byName[c.Name] = p
	}
	depth := map[string]int{}
	for _, p := range s.procs {
		if _, err := s.depth(p, depth, map[string]bool{}); err != nil {
			return nil, err
		}
	}
	maxDepth := 0
	for _, d := range depth {
		maxDepth = max(maxDepth, d)
	}
	s.levels = make([][]*proc, maxDepth+1)
	for _, p := range s.procs {
		d := maxDepth - depth[p.c.Name]
		s.levels[d] = append(s.levels[d], p)
	}
	return s, nil
}

func (s *Supervisor) depth(p *proc, memo map[string]int, visiting map[string]bool) (int, error) {
	if d, ok := memo[p.c.Name]; ok {
		return d, nil
	}
	if visiting[p.c.Name] {
		return 0, fmt.Errorf("supervise: dependency cycle through %q", p.c.Name)
	}
	visiting[p.c.Name] = true
	d := 0
	for _, name := range p.c.Deps {
		dep, ok := s.byName[name]
		if !ok {
			return 0, fmt.Errorf("supervise: %q depends on unknown child %q", p.c.Name, name)
		}
		dd, err := s.depth(dep, memo, visiting)
		if err != nil {
			return 0, err
		}
		d = max(d, dd+1)
	}
	memo[p.c.Name] = d
	return d, nil
}

func (s *Supervisor) Run(ctx context.Context) error {
	for _, p := range s.procs {
		s.wg.Add(1)
		go s.loop(p)
	}
	err := s.wait(ctx)
	s.shutdown()
	return err
}

func (s *Supervisor) wait(ctx context.Context) error {
	for {
		changed := s.watch()
		if s.Health().Ready && s.markReadied() {
			s.out.Logf(logs.PrioInfo, "every child is ready")
			if s.onReady != nil {
				s.onReady()
			}
		}
		select {
		case <-ctx.Done():
			return nil
		case err := <-s.fatal:
			return err
		case <-changed:
		}
	}
}

func (s *Supervisor) shutdown() {
	s.mu.Lock()
	s.stopping = true
	s.mu.Unlock()
	deadline := time.Now().Add(s.t.ShutdownBudget)
	need := make([]time.Duration, len(s.levels)+1)
	for i := len(s.levels) - 1; i >= 0; i-- {
		var longest time.Duration
		for _, p := range s.levels[i] {
			longest = max(longest, s.stopTimeout(p))
		}
		need[i] = need[i+1] + longest
	}
	for i, level := range s.levels {
		left := time.Until(deadline)
		var wg sync.WaitGroup
		for _, p := range level {
			wg.Add(1)
			go func() {
				defer wg.Done()
				r := request{action: spec.ActionStop, done: make(chan struct{})}
				if s.t.ShutdownBudget > 0 && left < need[i] {
					r.timeout = max(time.Duration(float64(s.stopTimeout(p))*float64(left)/float64(need[i])), time.Millisecond)
				}
				p.reqs <- r
				<-r.done
			}()
		}
		wg.Wait()
	}
	close(s.quit)
	s.wg.Wait()
}

func (s *Supervisor) Act(ctx context.Context, name, action string) error {
	p, ok := s.byName[name]
	if !ok {
		return fmt.Errorf("%w: %s", ErrUnknown, name)
	}
	if action != spec.ActionStart && action != spec.ActionStop && action != spec.ActionRestart {
		return fmt.Errorf("%w: %s", ErrAction, action)
	}
	s.mu.Lock()
	stopping := s.stopping
	s.mu.Unlock()
	if stopping {
		return ErrStopping
	}
	r := request{action: action, done: make(chan struct{})}
	select {
	case p.reqs <- r:
	case <-s.quit:
		return ErrStopping
	case <-ctx.Done():
		return ctx.Err()
	}
	select {
	case <-r.done:
		return nil
	case <-ctx.Done():
		return ctx.Err()
	}
}

func (s *Supervisor) Logs(name string) (*logs.Ring, error) {
	p, ok := s.byName[name]
	if !ok {
		return nil, fmt.Errorf("%w: %s", ErrUnknown, name)
	}
	return p.ring, nil
}

func (s *Supervisor) Ps() []spec.PsEntry {
	s.mu.Lock()
	defer s.mu.Unlock()
	now := time.Now()
	out := make([]spec.PsEntry, 0, len(s.procs))
	for _, p := range s.procs {
		state := p.state
		if s.looping(p, now) && !p.ready() {
			state = spec.StateCrashloop
		}
		out = append(out, spec.PsEntry{
			Name:      p.c.Name,
			State:     state,
			Health:    p.health,
			Restarts:  p.restarts,
			PID:       p.pid,
			StartedAt: p.startedAt,
			ExitCode:  p.exitCode,
			Core:      p.c.Core,
			Oneshot:   p.c.Oneshot,
			Watch:     p.watchEntries(),
		})
	}
	return out
}

func (p *proc) watchEntries() []spec.WatchEntry {
	var out []spec.WatchEntry
	for _, w := range p.watch {
		out = append(out, spec.WatchEntry{Name: w.name, Health: w.health, Error: w.err})
	}
	return out
}

func (s *Supervisor) Health() spec.Health {
	s.mu.Lock()
	defer s.mu.Unlock()
	now := time.Now()
	h := spec.Health{Stopping: s.stopping, Pending: []string{}, Failed: []string{}}
	for _, p := range s.procs {
		switch {
		case p.ready():
		case s.looping(p, now) || p.health == spec.HealthUnhealthy:
			h.Failed = append(h.Failed, p.c.Name)
		default:
			h.Pending = append(h.Pending, p.c.Name)
		}
		for _, w := range p.watch {
			if p.state == spec.StateRunning && w.health == spec.HealthUnhealthy {
				h.Degraded = append(h.Degraded, p.c.Name+"/"+w.name)
			}
		}
	}
	h.Ready = !s.stopping && len(h.Pending) == 0 && len(h.Failed) == 0
	return h
}

func (p *proc) ready() bool {
	if p.c.Oneshot {
		return p.state == spec.StateExited && p.exitCode == 0
	}
	return p.state == spec.StateRunning && (p.health == spec.HealthNone || p.health == spec.HealthHealthy)
}

func (p *proc) resetWatch(h spec.HealthState) {
	for _, w := range p.watch {
		w.health, w.err = h, ""
	}
}

func (s *Supervisor) looping(p *proc, now time.Time) bool {
	n := 0
	for _, t := range p.crashes {
		if now.Sub(t) <= s.t.CrashWindow {
			n++
		}
	}
	return n >= s.t.CrashLimit
}

func (s *Supervisor) markReadied() bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	first := !s.readied
	s.readied = true
	return first
}

func (s *Supervisor) watch() <-chan struct{} {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.changed
}

func (s *Supervisor) update(fn func()) {
	s.mu.Lock()
	fn()
	close(s.changed)
	s.changed = make(chan struct{})
	s.mu.Unlock()
}

func (s *Supervisor) depsReady(p *proc) bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	for _, name := range p.c.Deps {
		if !s.byName[name].ready() {
			return false
		}
	}
	return true
}

type event int

const (
	evRequest event = iota
	evTimer
	evChanged
	evQuit
)

func (s *Supervisor) await(p *proc, timer <-chan time.Time, changed <-chan struct{}) (request, event) {
	select {
	case r := <-p.reqs:
		return r, evRequest
	case <-timer:
		return request{}, evTimer
	case <-changed:
		return request{}, evChanged
	case <-s.quit:
		return request{}, evQuit
	}
}

func (s *Supervisor) loop(p *proc) {
	defer s.wg.Done()
	want := true
	var delay, backoff time.Duration
	for {
		if !want {
			s.update(func() {
				if p.state != spec.StateExited {
					p.state = spec.StateStopped
				}
				p.health = spec.HealthNone
				p.pid = 0
				p.resetWatch(spec.HealthNone)
			})
			r, ev := s.await(p, nil, nil)
			if ev == evQuit {
				return
			}
			close(r.done)
			if r.action != spec.ActionStop {
				want, delay = true, 0
			}
			continue
		}
		if delay > 0 {
			s.update(func() { p.state = spec.StateRestarting })
			timer := time.NewTimer(delay)
			r, ev := s.await(p, timer.C, nil)
			timer.Stop()
			delay = 0
			switch ev {
			case evQuit:
				return
			case evRequest:
				close(r.done)
				if r.action == spec.ActionStop {
					want = false
					continue
				}
			}
		}
		if !s.depsReady(p) {
			changed := s.watch()
			if s.depsReady(p) {
				continue
			}
			s.update(func() { p.state = spec.StateWaiting })
			r, ev := s.await(p, nil, changed)
			switch ev {
			case evQuit:
				return
			case evRequest:
				close(r.done)
				if r.action == spec.ActionStop {
					want = false
				}
			}
			continue
		}
		code, r, ran, quit := s.runOnce(p)
		if quit {
			return
		}
		if r != nil {
			close(r.done)
			if r.action == spec.ActionStop {
				want = false
			}
			continue
		}
		s.mu.Lock()
		stopping := s.stopping
		s.mu.Unlock()
		if (p.c.Oneshot && code == 0) || stopping {
			s.update(func() {
				p.state = spec.StateExited
				p.exitCode = code
			})
			want = false
			continue
		}
		if backoff == 0 || ran >= s.t.StableAfter {
			backoff = s.t.BackoffMin
		} else {
			backoff = min(backoff*2, s.t.BackoffMax)
		}
		delay = backoff
		s.crashed(p, code, delay)
	}
}

func (s *Supervisor) crashed(p *proc, code int, delay time.Duration) {
	now := time.Now()
	looping, fatal := false, false
	s.update(func() {
		p.exitCode = code
		p.restarts++
		kept := p.crashes[:0]
		for _, t := range p.crashes {
			if now.Sub(t) <= s.t.CrashWindow {
				kept = append(kept, t)
			}
		}
		p.crashes = append(kept, now)
		looping = s.looping(p, now)
		fatal = looping && (p.c.Core || !s.readied)
	})
	if !fatal {
		s.out.Logf(logs.PrioWarning, "%s exited with code %d, restarting in %s", p.c.Name, code, delay)
	}
	if !looping {
		return
	}
	if !fatal {
		s.out.Logf(logs.PrioErr, "%s is crash looping (%d exits within %s)", p.c.Name, s.t.CrashLimit, s.t.CrashWindow)
		return
	}
	reason := "before every child was ready"
	if p.c.Core {
		reason = "and it is a core child"
	}
	select {
	case s.fatal <- fmt.Errorf("%w: %s exited %d times within %s %s", ErrCrashloop, p.c.Name, s.t.CrashLimit, s.t.CrashWindow, reason):
	default:
	}
}

func (s *Supervisor) runOnce(p *proc) (code int, r *request, ran time.Duration, quit bool) {
	cmd, err := s.spawn(p)
	if err != nil {
		s.out.Logf(logs.PrioErr, "%s could not start: %v", p.c.Name, err)
		return 127, nil, 0, false
	}
	started := time.Now()
	pid := cmd.Process.Pid
	var gen int
	s.update(func() {
		p.gen++
		gen = p.gen
		p.state = spec.StateRunning
		p.health = spec.HealthNone
		if len(p.c.Probes) > 0 {
			p.health = spec.HealthStarting
		}
		p.resetWatch(spec.HealthStarting)
		p.pid = pid
		p.startedAt = started
		p.exitCode = 0
	})
	exited := make(chan struct{})
	go func() {
		cmd.Wait()
		close(exited)
	}()
	probeCtx, cancel := context.WithCancel(context.Background())
	defer cancel()
	go s.watchHealth(probeCtx, p, gen)
	for {
		select {
		case <-exited:
			cancel()
			killGroup(pid)
			return exitCode(cmd.ProcessState), nil, time.Since(started), false
		case req := <-p.reqs:
			if req.action == spec.ActionStart {
				close(req.done)
				continue
			}
			cancel()
			s.terminate(p, pid, exited, cmp.Or(req.timeout, s.stopTimeout(p)))
			return exitCode(cmd.ProcessState), &req, time.Since(started), false
		case <-s.quit:
			cancel()
			s.terminate(p, pid, exited, s.stopTimeout(p))
			return exitCode(cmd.ProcessState), nil, time.Since(started), true
		}
	}
}

func (s *Supervisor) spawn(p *proc) (*exec.Cmd, error) {
	if p.c.Cwd != "" {
		if err := os.MkdirAll(p.c.Cwd, 0o700); err != nil {
			return nil, err
		}
	}
	r, w, err := os.Pipe()
	if err != nil {
		return nil, err
	}
	cmd := exec.Command(p.c.Argv[0], p.c.Argv[1:]...)
	cmd.Env = p.c.Env.List()
	cmd.Dir = p.c.Cwd
	cmd.Stdout = w
	cmd.Stderr = w
	cmd.SysProcAttr = sysProcAttr()
	err = cmd.Start()
	w.Close()
	if err != nil {
		r.Close()
		return nil, err
	}
	s.out.Logf(logs.PrioInfo, "started %s (pid %d)", p.c.Name, cmd.Process.Pid)
	go func() {
		defer r.Close()
		demux := logs.Demux{Fallback: p.c.Name}
		logs.Pump(r, func(line string, continued bool) {
			p.ring.Add(line)
			name := p.c.Name
			if p.c.SvcLogs {
				name = demux.Svc(line, continued)
			}
			s.out.Child(name, logs.PrioInfo, line)
		})
	}()
	return cmd, nil
}

func (s *Supervisor) stopTimeout(p *proc) time.Duration {
	return cmp.Or(p.c.StopTimeout, s.t.StopTimeout)
}

func (s *Supervisor) terminate(p *proc, pid int, exited <-chan struct{}, timeout time.Duration) {
	s.update(func() { p.state = spec.StateStopping })
	sig := p.c.StopSignal
	if sig == 0 {
		sig = syscall.SIGTERM
	}
	syscall.Kill(pid, sig)
	timer := time.NewTimer(timeout)
	defer timer.Stop()
	select {
	case <-exited:
	case <-timer.C:
		s.out.Logf(logs.PrioWarning, "%s did not stop within %s, killing its process group", p.c.Name, timeout)
		syscall.Kill(-pid, syscall.SIGKILL)
		<-exited
	}
	killGroup(pid)
	s.out.Logf(logs.PrioInfo, "stopped %s", p.c.Name)
}

func killGroup(pid int) {
	syscall.Kill(-pid, syscall.SIGKILL)
}

func exitCode(ps *os.ProcessState) int {
	if ws, ok := ps.Sys().(syscall.WaitStatus); ok && ws.Signaled() {
		return 128 + int(ws.Signal())
	}
	return ps.ExitCode()
}
