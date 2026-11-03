// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"os"
	"path/filepath"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
)

func init() {
	Register(Check{ID: "boot-unit", Phases: Installed, Run: runBootUnit, Fix: fixBootUnit, SafeFix: true})
}

func RunningBinary() (string, error) {
	bin, err := os.Executable()
	if err != nil {
		return "", err
	}
	if real, err := filepath.EvalSymlinks(bin); err == nil {
		bin = real
	}
	return filepath.Abs(bin)
}

func StopGrace(p *engine.Project) time.Duration {
	var longest time.Duration
	if p == nil || p.Model == nil {
		return longest
	}
	for _, svc := range p.Model.Services {
		if svc.StopGracePeriod != nil && time.Duration(*svc.StopGracePeriod) > longest {
			longest = time.Duration(*svc.StopGracePeriod)
		}
	}
	return longest
}

func BootUnitFor(inst *instance.Instance, e *engine.Engine, p *engine.Project) (host.BootUnit, error) {
	bin, err := RunningBinary()
	if err != nil {
		return host.BootUnit{}, err
	}
	dir, err := filepath.Abs(inst.Dir)
	if err != nil {
		return host.BootUnit{}, err
	}
	return host.BootUnit{
		Project:   inst.Project(),
		Dir:       dir,
		Binary:    bin,
		Podman:    e.Kind == engine.Podman,
		Rootless:  e.Rootless,
		StopGrace: StopGrace(p),
	}, nil
}

func (e *Env) bootUnit(ctx context.Context) (host.BootUnit, host.BootUnitState, bool) {
	st := e.state()
	if st == nil || st.Ejected != nil || e.Engine == nil || !host.BootUnitSupported() {
		return host.BootUnit{}, host.BootUnitState{}, false
	}
	u, err := BootUnitFor(e.Instance, e.Engine, e.Project)
	if err != nil {
		return host.BootUnit{}, host.BootUnitState{}, false
	}
	s, err := host.CheckBootUnit(ctx, u)
	if err != nil || !s.Supported {
		return host.BootUnit{}, host.BootUnitState{}, false
	}
	return u, s, true
}

func runBootUnit(ctx context.Context, e *Env) Result {
	_, s, ok := e.bootUnit(ctx)
	if !ok {
		return skip("")
	}
	switch {
	case s.Missing, s.Disabled, s.NoLinger:
		return fixable(warn("Fluxer won't start again after this machine restarts.", "Run rexctl doctor --fix to start it with the machine."))
	case s.Stale, s.CopyStale:
		return fixable(warn("The service that starts Fluxer with this machine points at an old copy of rexctl.", "Run rexctl doctor --fix to update it."))
	}
	return pass("Fluxer starts with this machine.")
}

func fixBootUnit(ctx context.Context, e *Env) error {
	u, s, ok := e.bootUnit(ctx)
	if !ok || !s.Needed() {
		return nil
	}
	return host.InstallBootUnit(ctx, u)
}
