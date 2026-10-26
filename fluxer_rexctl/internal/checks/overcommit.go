// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"runtime"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
)

func init() {
	Register(Check{ID: "memory-overcommit", Phases: Preinstall | Installed, Run: runOvercommit, Fix: fixOvercommit, SafeFix: true})
}

func runOvercommit(_ context.Context, _ *Env) Result {
	if runtime.GOOS != "linux" {
		return skip("")
	}
	if !host.OvercommitDisabled() {
		return pass("Memory overcommit is on.")
	}
	r := fixable(warn("Memory overcommit is off, so Fluxer's cache can fail to save data when memory runs low.", "Run rexctl doctor --fix to turn it on."))
	r.Manual = "Ask an administrator to add " + host.OvercommitSetting + " to " + host.OvercommitFile + " and run sudo sysctl --system."
	return r
}

func fixOvercommit(ctx context.Context, _ *Env) error {
	return host.EnableOvercommit(ctx)
}
