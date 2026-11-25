// SPDX-License-Identifier: AGPL-3.0-or-later

package supervise

import (
	"cmp"
	"context"
	"errors"
	"fmt"
	"maps"
	"os"
	"os/exec"
	"slices"
	"strings"
	"sync"
	"syscall"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/logs"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

const minStopSlice = time.Second

var (
	ErrCrashloop = errors.New("supervise: a core child is crash looping")
	ErrUnknown   = errors.New("supervise: no such child")
	ErrStopping  = errors.New("supervise: fluxerd is shutting down")
	ErrAction    = errors.New("supervise: unknown action")
	ErrRestart   = errors.New("supervise: the change needs a full restart")
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
	StartPeriod    time.Duration
	Retries        int
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
	StartPeriod:    time.Minute,
	Retries:        10,
	StopTimeout:    10 * time.Second,
	ShutdownBudget: 120 * time.Second,
}

type Options struct {
	Out     *logs.Output
	OnReady func(ready bool)
	Timing  Timing
}

type request struct {
	action    string
	child     *spec.Child
	timeout   time.Duration
	done      chan struct{}
	restarted *bool
}

const (
	actionReplace = "replace"
	actionDrop    = "drop"
)

type proc struct {
	c     spec.Child
	ring  *logs.Ring
	reqs  chan request
	kick  chan int
	watch []*watched

	state     spec.State
	health    spec.HealthState
	restarts  int
	pid       int
	gen       int
	startedAt time.Time
	exitCode  int
	crashes   []time.Time
	depGens   map[string]int
}

type Supervisor struct {
	out     *logs.Output
	onReady func(ready bool)
	t       Timing
	procs   []*proc
	byName  map[string]*proc
	levels  [][]*proc

	mu       sync.Mutex
	changed  chan struct{}
	running  bool
	stopping bool
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
		if _, dup := s.byName[c.Name]; dup {
			return nil, fmt.Errorf("supervise: child %q is listed twice", c.Name)
		}
		p, err := newProc(c)
		if err != nil {
			return nil, err
		}
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

func newProc(c spec.Child) (*proc, error) {
	if c.Name == "" || len(c.Argv) == 0 {
		return nil, fmt.Errorf("supervise: child %q has no name or argv", c.Name)
	}
	return &proc{c: c, ring: logs.NewRing(logs.RingLines), reqs: make(chan request), kick: make(chan int, 1), state: spec.StateWaiting, watch: groupWatch(c.Watch)}, nil
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
	s.mu.Lock()
	s.running = true
	for _, p := range s.procs {
		s.wg.Add(1)
		go s.loop(p)
	}
	s.mu.Unlock()
	err := s.wait(ctx)
	s.shutdown()
	return err
}

func (s *Supervisor) wait(ctx context.Context) error {
	ready := false
	for {
		changed := s.watch()
		h := s.Health()
		if h.Ready != ready && !h.Stopping {
			ready = h.Ready
			if ready {
				s.out.Logf(logs.PrioInfo, "every core child is ready")
			} else {
				s.out.Logf(logs.PrioWarning, "a core child is not ready any more: %s", strings.Join(append(h.Failed, h.Pending...), ", "))
			}
			if s.onReady != nil {
				s.onReady(ready)
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
	levels := s.levels
	s.mu.Unlock()
	deadline := time.Now().Add(s.t.ShutdownBudget)
	for i, level := range levels {
		slice := max(time.Until(deadline)-s.reserve(levels[i+1:]), minStopSlice)
		var wg sync.WaitGroup
		for _, p := range level {
			wg.Add(1)
			go func() {
				defer wg.Done()
				r := request{action: spec.ActionStop, done: make(chan struct{})}
				if s.t.ShutdownBudget > 0 {
					r.timeout = min(s.stopTimeout(p), slice)
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
	s.mu.Lock()
	p, ok := s.byName[name]
	stopping := s.stopping
	s.mu.Unlock()
	if !ok {
		return fmt.Errorf("%w: %s", ErrUnknown, name)
	}
	if action != spec.ActionStart && action != spec.ActionStop && action != spec.ActionRestart {
		return fmt.Errorf("%w: %s", ErrAction, action)
	}
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

type reloadPlan struct {
	added, removed, changed []*proc
	next                    []spec.Child
}

func (s *Supervisor) plan(children []spec.Child, force []string) (reloadPlan, error) {
	var rp reloadPlan
	var err error
	rp.added, rp.removed, err = s.listChanges(children)
	if err != nil {
		return rp, err
	}
	for _, c := range children {
		p, ok := s.byName[c.Name]
		if !ok {
			continue
		}
		if !slices.Equal(c.Deps, p.c.Deps) || c.Core != p.c.Core || c.Oneshot != p.c.Oneshot {
			return rp, fmt.Errorf("%w: %s changed how it starts", ErrRestart, c.Name)
		}
		if sameChild(c, p.c) && !slices.Contains(force, c.Name) {
			continue
		}
		rp.changed = append(rp.changed, p)
		rp.next = append(rp.next, c)
	}
	return rp, nil
}

func (s *Supervisor) Plan(children []spec.Child) ([]string, error) {
	s.mu.Lock()
	rp, err := s.plan(children, nil)
	s.mu.Unlock()
	if err != nil {
		return nil, err
	}
	var names []string
	for _, group := range [][]*proc{rp.added, rp.removed, rp.changed} {
		for _, p := range group {
			names = append(names, p.c.Name)
		}
	}
	slices.Sort(names)
	return names, nil
}

func (s *Supervisor) Reload(ctx context.Context, children []spec.Child, force []string) ([]string, error) {
	s.mu.Lock()
	rp, err := s.plan(children, force)
	if err != nil {
		s.mu.Unlock()
		return nil, err
	}
	added, removed, changed, next := rp.added, rp.removed, rp.changed, rp.next
	stopping, running := s.stopping, s.running
	s.mu.Unlock()
	if stopping {
		return nil, ErrStopping
	}
	for _, p := range removed {
		s.out.Logf(logs.PrioInfo, "%s was removed from the configuration, stopping it", p.c.Name)
		if !running {
			continue
		}
		if err := s.send(ctx, p, request{action: spec.ActionStop, done: make(chan struct{})}); err != nil {
			return nil, err
		}
	}
	if len(added) > 0 || len(removed) > 0 {
		s.regroup(children, added, removed)
	}
	for _, p := range removed {
		if running {
			if err := s.send(ctx, p, request{action: actionDrop, done: make(chan struct{})}); err != nil {
				return nil, err
			}
		}
	}
	for _, p := range added {
		s.out.Logf(logs.PrioInfo, "%s was added to the configuration, starting it", p.c.Name)
	}
	var names []string
	for i, p := range changed {
		restarted := new(bool)
		if err := s.send(ctx, p, request{action: actionReplace, child: &next[i], done: make(chan struct{}), restarted: restarted}); err != nil {
			return names, err
		}
		if *restarted {
			names = append(names, next[i].Name)
		}
	}
	return names, nil
}

func (s *Supervisor) send(ctx context.Context, p *proc, r request) error {
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

func (s *Supervisor) listChanges(children []spec.Child) ([]*proc, []*proc, error) {
	names := map[string]bool{}
	for _, c := range children {
		if names[c.Name] {
			return nil, nil, fmt.Errorf("supervise: child %q is listed twice", c.Name)
		}
		names[c.Name] = true
	}
	dependedOn := func(name string) bool {
		for _, c := range children {
			if slices.Contains(c.Deps, name) {
				return true
			}
		}
		for _, p := range s.procs {
			if slices.Contains(p.c.Deps, name) {
				return true
			}
		}
		return false
	}
	standalone := func(c spec.Child) bool {
		return !c.Core && !c.Oneshot && len(c.Deps) == 0 && !dependedOn(c.Name)
	}
	var added, removed []*proc
	for _, c := range children {
		if _, ok := s.byName[c.Name]; ok {
			continue
		}
		if !standalone(c) {
			return nil, nil, fmt.Errorf("%w: %s was added", ErrRestart, c.Name)
		}
		p, err := newProc(c)
		if err != nil {
			return nil, nil, err
		}
		added = append(added, p)
	}
	for _, p := range s.procs {
		if names[p.c.Name] {
			continue
		}
		if !standalone(p.c) {
			return nil, nil, fmt.Errorf("%w: %s was removed", ErrRestart, p.c.Name)
		}
		removed = append(removed, p)
	}
	return added, removed, nil
}

func (s *Supervisor) regroup(children []spec.Child, added, removed []*proc) {
	s.update(func() {
		for _, p := range removed {
			delete(s.byName, p.c.Name)
			for i := range s.levels {
				s.levels[i] = slices.DeleteFunc(slices.Clone(s.levels[i]), func(q *proc) bool { return q == p })
			}
		}
		if len(added) > 0 && len(s.levels) == 0 {
			s.levels = make([][]*proc, 1)
		}
		last := len(s.levels) - 1
		for _, p := range added {
			s.byName[p.c.Name] = p
			s.levels[last] = append(slices.Clone(s.levels[last]), p)
		}
		procs := make([]*proc, 0, len(children))
		for _, c := range children {
			procs = append(procs, s.byName[c.Name])
		}
		s.procs = procs
		if s.running && !s.stopping {
			for _, p := range added {
				s.wg.Add(1)
				go s.loop(p)
			}
		}
	})
}

func sameChild(a, b spec.Child) bool {
	if !slices.Equal(a.Argv, b.Argv) || !maps.Equal(a.Env, b.Env) || a.Cwd != b.Cwd || a.Health != b.Health ||
		a.RestartUnhealthy != b.RestartUnhealthy || a.Restart != b.Restart || a.Rearm != b.Rearm || a.SvcLogs != b.SvcLogs ||
		a.StopSignal != b.StopSignal || a.StopTimeout != b.StopTimeout {
		return false
	}
	return slices.EqualFunc(a.Probes, b.Probes, sameProbe) && slices.EqualFunc(a.Watch, b.Watch, sameProbe)
}

func sameProbe(a, b spec.Probe) bool {
	return a.Name == b.Name && a.Kind == b.Kind && a.Addr == b.Addr && a.Path == b.Path && a.Status == b.Status &&
		slices.Equal(a.Argv, b.Argv) && a.File == b.File && a.MaxAge == b.MaxAge && a.Core == b.Core
}

func (s *Supervisor) Logs(name string) (*logs.Ring, error) {
	s.mu.Lock()
	p, ok := s.byName[name]
	s.mu.Unlock()
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
		out = append(out, spec.WatchEntry{Name: w.name, Health: w.health, Error: w.err, Core: w.core})
	}
	return out
}

func (s *Supervisor) reserve(levels [][]*proc) time.Duration {
	s.mu.Lock()
	defer s.mu.Unlock()
	var total time.Duration
	for _, level := range levels {
		var longest time.Duration
		for _, p := range level {
			if p.alive() {
				longest = max(longest, s.stopTimeout(p))
			}
		}
		total += longest
	}
	return total
}

func (s *Supervisor) Health() spec.Health {
	s.mu.Lock()
	defer s.mu.Unlock()
	now := time.Now()
	h := spec.Health{Stopping: s.stopping, Pending: []string{}, Failed: []string{}}
	for _, p := range s.procs {
		failed := s.looping(p, now) || p.health == spec.HealthUnhealthy || p.state == spec.StateExited && !p.c.Oneshot
		switch {
		case p.ready():
		case p.c.Core && failed:
			h.Failed = append(h.Failed, p.c.Name)
		case p.c.Core:
			h.Pending = append(h.Pending, p.c.Name)
		case failed || p.state == spec.StateStopped:
			h.Degraded = append(h.Degraded, p.c.Name)
		default:
			h.Starting = append(h.Starting, p.c.Name)
		}
		if !p.ready() {
			continue
		}
		for _, w := range p.watch {
			name := p.c.Name + "/" + w.name
			switch {
			case w.health == spec.HealthHealthy:
			case p.c.Core && w.core && w.health == spec.HealthUnhealthy:
				h.Failed = append(h.Failed, name)
			case p.c.Core && w.core:
				h.Pending = append(h.Pending, name)
			case w.health == spec.HealthUnhealthy:
				h.Degraded = append(h.Degraded, name)
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

func (p *proc) alive() bool {
	return p.state == spec.StateStarting || p.state == spec.StateRunning
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

func (s *Supervisor) Changes() <-chan struct{} {
	return s.watch()
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

func (s *Supervisor) setState(p *proc, st spec.State) {
	s.mu.Lock()
	same := p.state == st
	s.mu.Unlock()
	if !same {
		s.update(func() { p.state = st })
	}
}

func (s *Supervisor) rearm(p *proc) bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	if !p.c.Rearm || s.stopping || p.state != spec.StateExited || p.exitCode != 0 {
		return false
	}
	for _, name := range p.c.Deps {
		if s.byName[name].gen != p.depGens[name] {
			return true
		}
	}
	return false
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

func (s *Supervisor) idle(p *proc) bool {
	for {
		var changed <-chan struct{}
		if p.c.Rearm {
			changed = s.watch()
			if s.rearm(p) {
				s.out.Logf(logs.PrioInfo, "%s runs again because a service it depends on restarted", p.c.Name)
				return false
			}
		}
		r, ev := s.await(p, nil, changed)
		switch ev {
		case evQuit:
			return true
		case evChanged:
			continue
		}
		if r.action == actionDrop {
			s.accept(p, r, false)
			return true
		}
		if r.action == actionReplace {
			s.mu.Lock()
			rerun := p.c.Oneshot && p.state == spec.StateExited && p.exitCode == 0 && !s.stopping
			s.mu.Unlock()
			s.accept(p, r, rerun)
			if rerun {
				s.out.Logf(logs.PrioInfo, "%s runs again because its settings changed", p.c.Name)
				return false
			}
			continue
		}
		s.accept(p, r, false)
		if r.action != spec.ActionStop {
			return false
		}
	}
}

func (s *Supervisor) accept(p *proc, r request, restarted bool) {
	if r.child != nil {
		s.update(func() {
			p.c = *r.child
			p.watch = groupWatch(r.child.Watch)
		})
	}
	if r.restarted != nil {
		*r.restarted = restarted
	}
	close(r.done)
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
			if quit := s.idle(p); quit {
				return
			}
			want, delay = true, 0
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
				s.accept(p, r, r.action == actionReplace)
				if r.action == spec.ActionStop {
					want = false
					continue
				}
			}
		}
		if !s.depsReady(p) {
			s.setState(p, spec.StateWaiting)
			changed := s.watch()
			if s.depsReady(p) {
				continue
			}
			r, ev := s.await(p, nil, changed)
			switch ev {
			case evQuit:
				return
			case evRequest:
				s.accept(p, r, false)
				if r.action == spec.ActionStop {
					want = false
				}
			}
			continue
		}
		code, r, ran, kicked, quit := s.runOnce(p)
		if quit {
			return
		}
		if r != nil {
			s.accept(p, *r, r.action == actionReplace)
			if r.action == spec.ActionStop {
				want = false
			}
			continue
		}
		s.mu.Lock()
		stopping := s.stopping
		s.mu.Unlock()
		if kicked && !stopping {
			delay = s.t.BackoffMin
			s.update(func() {
				p.exitCode = code
				p.restarts++
			})
			continue
		}
		kept := !p.c.Oneshot && (p.c.Restart == spec.RestartNever || p.c.Restart == spec.RestartOnFailure && code == 0)
		if (p.c.Oneshot && code == 0) || stopping || kept {
			s.update(func() {
				p.state = spec.StateExited
				p.exitCode = code
			})
			if kept && !stopping {
				s.out.Logf(logs.PrioWarning, "%s exited with code %d and is not restarted, because its restart policy is %q", p.c.Name, code, p.c.Restart)
			}
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
		fatal = looping && p.c.Core
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
	select {
	case s.fatal <- fmt.Errorf("%w: %s exited %d times within %s", ErrCrashloop, p.c.Name, s.t.CrashLimit, s.t.CrashWindow):
	default:
	}
}

func (s *Supervisor) runOnce(p *proc) (code int, r *request, ran time.Duration, kicked, quit bool) {
	cmd, err := s.spawn(p)
	if err != nil {
		s.out.Logf(logs.PrioErr, "%s could not start: %v", p.c.Name, err)
		return 127, nil, 0, false, false
	}
	started := time.Now()
	pid := cmd.Process.Pid
	var gen int
	s.update(func() {
		p.gen++
		gen = p.gen
		p.depGens = map[string]int{}
		for _, name := range p.c.Deps {
			p.depGens[name] = s.byName[name].gen
		}
		p.state = spec.StateRunning
		p.health = spec.HealthNone
		if len(p.c.Probes) > 0 {
			p.state = spec.StateStarting
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
	go s.watchHealth(probeCtx, p, gen, p.c, p.watch)
	for {
		select {
		case <-exited:
			cancel()
			killGroup(pid)
			return exitCode(cmd.ProcessState), nil, time.Since(started), false, false
		case req := <-p.reqs:
			if req.action == spec.ActionStart {
				close(req.done)
				continue
			}
			cancel()
			s.terminate(p, pid, exited, cmp.Or(req.timeout, s.stopTimeout(p)))
			return exitCode(cmd.ProcessState), &req, time.Since(started), false, false
		case g := <-p.kick:
			if g != gen {
				continue
			}
			cancel()
			s.out.Logf(logs.PrioWarning, "%s stayed unhealthy, restarting it", p.c.Name)
			s.terminate(p, pid, exited, s.stopTimeout(p))
			return exitCode(cmd.ProcessState), nil, time.Since(started), true, false
		case <-s.quit:
			cancel()
			s.terminate(p, pid, exited, s.stopTimeout(p))
			return exitCode(cmd.ProcessState), nil, time.Since(started), false, true
		}
	}
}

func (s *Supervisor) spawn(p *proc) (*exec.Cmd, error) {
	if p.c.Cwd != "" {
		if err := os.MkdirAll(p.c.Cwd, 0o700); err != nil {
			return nil, err
		}
	}
	outR, outW, err := os.Pipe()
	if err != nil {
		return nil, err
	}
	errR, errW, err := os.Pipe()
	if err != nil {
		outR.Close()
		outW.Close()
		return nil, err
	}
	cmd := exec.Command(p.c.Argv[0], p.c.Argv[1:]...)
	cmd.Env = p.c.Env.List()
	cmd.Dir = p.c.Cwd
	cmd.Stdout = outW
	cmd.Stderr = errW
	cmd.SysProcAttr = sysProcAttr()
	err = cmd.Start()
	outW.Close()
	errW.Close()
	if err != nil {
		outR.Close()
		errR.Close()
		return nil, err
	}
	s.out.Logf(logs.PrioInfo, "started %s (pid %d)", p.c.Name, cmd.Process.Pid)
	go s.pump(p.c, p.ring, outR, logs.Stdout)
	go s.pump(p.c, p.ring, errR, logs.Stderr)
	return cmd, nil
}

func (s *Supervisor) pump(c spec.Child, ring *logs.Ring, r *os.File, stream string) {
	defer r.Close()
	demux := logs.Demux{Fallback: c.Name}
	logs.Pump(r, func(line string, continued bool) {
		ring.Add(line)
		name := c.Name
		if c.SvcLogs {
			name = demux.Svc(line, continued)
		}
		s.out.Child(name, stream, line)
	})
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
	signal(pid, sig)
	timer := time.NewTimer(timeout)
	defer timer.Stop()
	select {
	case <-exited:
	case <-timer.C:
		s.out.Logf(logs.PrioWarning, "%s did not stop within %s, killing its process group", p.c.Name, timeout)
		killGroup(pid)
		<-exited
	}
	killGroup(pid)
	s.out.Logf(logs.PrioInfo, "stopped %s", p.c.Name)
}

func exitCode(ps *os.ProcessState) int {
	if ws, ok := ps.Sys().(syscall.WaitStatus); ok && ws.Signaled() {
		return 128 + int(ws.Signal())
	}
	return ps.ExitCode()
}
