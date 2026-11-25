// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"errors"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func init() {
	Register(Check{ID: "podman-api", Phases: Installed, Run: runPodmanAPI, Fix: fixPodmanAPI})
}

var podmanAPI = host.PodmanAPI

const PodmanServiceStopped = "Podman's API service stopped with this error."

func podmanServiceRow(err error, startable bool) (Result, bool) {
	var ce *host.CommandError
	if !errors.Is(err, host.ErrPodmanServiceFailed) || !errors.As(err, &ce) || strings.TrimSpace(ce.Output) == "" {
		return Result{}, false
	}
	msg := PodmanServiceStopped + " " + ui.Sentence(strings.TrimSpace(ce.Output))
	if !startable {
		return fail(msg, "Fix what it reports, then run rexctl doctor."), true
	}
	return fixable(fail(msg, "Fix what it reports, then run rexctl doctor --fix.")), true
}

func (e *Env) containerOnPodman() (rootless, ok bool) {
	st := e.state()
	if !e.Container || st == nil || st.Engine.Kind != string(engine.Podman) {
		return false, false
	}
	return st.Engine.Rootless(), true
}

func runPodmanAPI(ctx context.Context, e *Env) Result {
	rootless, ok := e.containerOnPodman()
	if !ok {
		return skip("")
	}
	s, err := podmanAPI(ctx, rootless)
	if err == nil {
		return pass("Podman's API is running.")
	}
	if r, ok := podmanServiceRow(err, s.SocketMissing && s.Systemd); ok {
		return r
	}
	switch {
	case !s.Systemd:
		return fail("Podman's API gives no reply.", "Start podman.socket, then run rexctl doctor.")
	case !s.SocketMissing:
		restart := "sudo systemctl restart podman.socket"
		if rootless {
			restart = "systemctl --user restart podman.socket"
		}
		return fail("Podman's API gives no reply, and podman.socket is running.", "Run "+restart+", then run rexctl doctor.")
	}
	r := fixable(fail("Podman's API socket isn't running.", "Run rexctl doctor --fix to start it."))
	r.Manual = "Run " + strings.Join(s.ManualFix(), " and ") + ", then run rexctl doctor."
	return r
}

func fixPodmanAPI(ctx context.Context, e *Env) error {
	rootless, ok := e.containerOnPodman()
	if !ok {
		return nil
	}
	s, err := podmanAPI(ctx, rootless)
	if err == nil || !s.SocketMissing || !s.Systemd {
		return nil
	}
	return e.run(ctx, s.Steps()...)
}
