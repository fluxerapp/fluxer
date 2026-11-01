// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

var (
	overcommit       = host.Overcommit
	enableOvercommit = host.EnableOvercommit
	freeMemory       = host.FreeMemory
)

const (
	defaultValkeyMaxMemory = 192 << 20
	valkeyHeadroom         = 4
)

const overcommitOff = "Memory overcommit is off, so Fluxer's cache can fail to save data when memory runs low."

func init() {
	Register(Check{ID: "memory-overcommit", Phases: Preinstall | Installed, Run: runOvercommit, Fix: fixOvercommit, SafeFix: true})
}

func runOvercommit(_ context.Context, e *Env) Result {
	state := overcommit()
	if (state == host.OvercommitOff || state == host.OvercommitLocked) && enoughForValkey(e) {
		return pass("Memory overcommit is off, and this machine has enough free memory for Fluxer's cache to save its data.")
	}
	switch state {
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

func enoughForValkey(e *Env) bool {
	limit := uint64(defaultValkeyMaxMemory)
	if v := e.value("FLUXER_VALKEY_MAXMEMORY"); v != "" {
		n, ok := voice.ParseBytes(v)
		if !ok {
			return false
		}
		limit = n
	}
	free := freeMemory()
	return free > 0 && free >= limit*valkeyHeadroom
}
