// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
)

var (
	overcommit       = host.Overcommit
	enableOvercommit = host.EnableOvercommit
)

const overcommitOff = "Memory overcommit is off, so Fluxer's cache can fail to save data when memory runs low."

func init() {
	Register(Check{ID: "memory-overcommit", Phases: Preinstall | Installed, Run: runOvercommit, Fix: fixOvercommit, SafeFix: true})
}

func runOvercommit(_ context.Context, _ *Env) Result {
	switch overcommit() {
	case host.OvercommitUnknown:
		return skip("")
	case host.OvercommitLocked:
		return warn(overcommitOff, "This machine is a container and can't change it. Run this on the container host: "+host.OvercommitCommand)
	case host.OvercommitOff:
		r := fixable(warn(overcommitOff, "Run rexctl doctor --fix to turn it on."))
		r.Manual = "Run this as an administrator, or on the container host if this machine is a container: " + host.OvercommitCommand
		return r
	}
	return pass("Memory overcommit is on.")
}

func fixOvercommit(ctx context.Context, _ *Env) error {
	return enableOvercommit(ctx)
}
