// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"runtime"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
)

func init() {
	Register(Check{ID: "podman-subids", Phases: Preinstall | Installed, Run: runPodmanSubIDs})
	Register(Check{ID: "podman-setup", Phases: Installed, Run: runPodmanSetup, Fix: fixPodmanSetup, SafeFix: true})
}

func (e *Env) podmanSetup(ctx context.Context) (host.PodmanSetup, bool) {
	if e.Engine == nil || e.Engine.Kind != engine.Podman {
		return host.PodmanSetup{}, false
	}
	return host.CheckPodman(ctx, e.Engine.Rootless, false), true
}

func runPodmanSubIDs(ctx context.Context, e *Env) Result {
	if runtime.GOOS != "linux" || !e.podman() || !e.rootless() {
		return skip("")
	}
	if !host.HasSubIDs(ctx) {
		return fail("Your account has no subordinate user IDs, and Podman needs them to unpack Fluxer.", "Add your account to /etc/subuid and /etc/subgid, run podman system migrate, then run rexctl doctor.")
	}
	return pass("Your account has the subordinate user IDs Podman needs.")
}

func runPodmanSetup(ctx context.Context, e *Env) Result {
	s, ok := e.podmanSetup(ctx)
	if !ok {
		return skip("")
	}
	var r Result
	switch {
	case s.SocketMissing && s.Systemd:
		r = fixable(fail("Podman's API socket isn't running.", "Run rexctl doctor --fix to start it."))
	case s.SocketMissing:
		r = fail("Podman's API socket isn't running.", "Start podman.socket, then run rexctl doctor.")
	case s.NoLinger || s.NoSocketStart:
		r = fixable(warn("Fluxer won't start again after this machine restarts.", "Run rexctl doctor --fix to start it with the machine."))
	case s.EventsOn:
		r = fixable(warn("Podman writes every health check to the system log.", "Run rexctl doctor --fix to stop that."))
	default:
		return pass("Podman is set up for Fluxer.")
	}
	if s.Needed() && (s.Systemd || !s.SocketMissing) {
		r.Fixable = true
	}
	if cmds := s.ManualFix(); len(cmds) > 0 {
		r.Manual = "Run " + strings.Join(cmds, " and ") + ", then run rexctl doctor."
	}
	return r
}

func fixPodmanSetup(ctx context.Context, e *Env) error {
	s, ok := e.podmanSetup(ctx)
	if !ok {
		return nil
	}
	return host.FixPodman(ctx, s)
}
