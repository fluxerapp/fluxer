// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"errors"
	"slices"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

func init() {
	Register(Check{ID: "services", Phases: Installed, Run: runServices, Fix: fixServices, SafeFix: true})
}

type ServiceState struct {
	Name     string
	State    string
	Health   string
	ExitCode int
	OneShot  bool
}

func (s ServiceState) Running() bool { return s.State == "running" }

func (s ServiceState) OK() bool {
	if s.OneShot {
		return s.State == "running" || (s.State == "exited" && s.ExitCode == 0)
	}
	return s.State == "running" && s.Health != "unhealthy" && s.Health != "starting"
}

func (s ServiceState) Starting() bool {
	return s.State == "running" && s.Health == "starting"
}

func (s ServiceState) Unhealthy() bool {
	return s.State == "running" && s.Health == "unhealthy"
}

func expectedServices(p *engine.Project, containers []engine.Container) []string {
	if p.Model != nil {
		return p.Services()
	}
	seen := map[string]bool{}
	var out []string
	for _, c := range containers {
		if c.Service != "" && !seen[c.Service] {
			seen[c.Service] = true
			out = append(out, c.Service)
		}
	}
	slices.Sort(out)
	return out
}

func serviceStates(expected []string, containers []engine.Container) []ServiceState {
	byService := map[string]engine.Container{}
	for _, c := range containers {
		prev, seen := byService[c.Service]
		if !seen || (prev.State != "running" && c.State == "running") {
			byService[c.Service] = c
		}
	}
	out := make([]ServiceState, 0, len(expected))
	for _, name := range expected {
		s := ServiceState{Name: name, State: "missing", OneShot: stack.OneShot[name]}
		if c, ok := byService[name]; ok {
			s.State, s.Health, s.ExitCode = c.State, c.Health, c.ExitCode
		}
		out = append(out, s)
	}
	return out
}

func (e *Env) serviceStates(ctx context.Context) ([]ServiceState, error) {
	if e.Project == nil {
		return nil, errNoProject
	}
	containers, err := e.containers(ctx)
	if err != nil {
		return nil, err
	}
	return serviceStates(expectedServices(e.Project, containers), containers), nil
}

func runServices(ctx context.Context, e *Env) Result {
	states, err := e.serviceStates(ctx)
	if err != nil {
		if e.Project == nil {
			return skip("")
		}
		r := fail("rexctl can't see which Fluxer services are running.", "Run rexctl doctor --verbose to see why.")
		r.Details = []string{err.Error()}
		return r
	}
	var down, never, missing, unhealthy, starting []string
	running := 0
	for _, s := range states {
		switch {
		case s.State == "created" && !s.OK():
			never = append(never, s.Name)
		case s.State == "missing":
			missing = append(missing, s.Name)
		}
		switch {
		case s.Running():
			running++
			if s.Unhealthy() {
				unhealthy = append(unhealthy, s.Name)
			} else if s.Starting() {
				starting = append(starting, s.Name)
			}
		case !s.OK():
			down = append(down, s.Name)
		}
	}
	var ej *instance.EjectedError
	ejected := errors.As(e.Instance.Managed(), &ej)
	var r Result
	switch {
	case !ejected && len(never)+len(missing) > 0 && e.Instance != nil && e.Instance.State != nil && e.Instance.State.InstallUnfinished():
		r = fail("The install didn't finish.", "Run rexctl install again to finish it.")
		r.Details = append(never, missing...)
	case running == 0 && ejected:
		return fail("Fluxer isn't running.", ej.Start())
	case running == 0:
		return fixable(fail("Fluxer isn't running.", "Run rexctl start."))
	case len(down) > 0 && len(never) == len(down):
		r = fixable(fail(Services(never, "was never started.", "were never started."), startHint(never)))
		r.Details = never
	case len(down) > 0 && len(never)+len(missing) == len(down):
		r = fixable(fail(Services(down, "isn't running.", "aren't running."), startHint(down)))
		r.Details = down
	case len(down) > 0:
		r = fixable(fail(Services(down, "isn't running.", "aren't running."),
			"Run rexctl logs "+down[0]+" to see why, then run rexctl doctor --fix."))
		r.Details = down
	case len(unhealthy) > 0:
		r = fixable(fail(Services(unhealthy, "is unhealthy.", "are unhealthy."),
			"Run rexctl logs "+unhealthy[0]+" to see why, then run rexctl doctor --fix."))
		r.Details = unhealthy
	case len(starting) > 0:
		r = warn("Fluxer is still starting.", "Wait a minute, then run rexctl doctor.")
		r.Details = starting
	default:
		return pass("Fluxer is running.")
	}
	return r
}

func startHint(names []string) string {
	if len(names) == 1 {
		return "Run rexctl doctor --fix to start it."
	}
	return "Run rexctl doctor --fix to start them."
}

func fixServices(ctx context.Context, e *Env) error {
	states, err := e.serviceStates(ctx)
	if err != nil {
		return err
	}
	var restart []string
	for _, s := range states {
		if s.State != "missing" && !s.OneShot && (!s.Running() || s.Unhealthy()) {
			restart = append(restart, s.Name)
		}
	}
	if len(restart) > 0 {
		if err := e.Project.Restart(ctx, restart...); err != nil {
			return err
		}
	}
	if err := e.up(ctx); err != nil {
		return err
	}
	_ = e.Project.WaitReady(ctx, 5*time.Minute, nil)
	return ctx.Err()
}
