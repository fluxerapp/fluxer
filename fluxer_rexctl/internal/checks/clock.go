// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"os/exec"
	"runtime"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
)

func init() {
	Register(Check{ID: "clock", Phases: Installed | Voice, Run: runClock, Fix: fixClock})
}

type clockSync struct {
	known  bool
	ntp    bool
	synced bool
}

func readClockSync(ctx context.Context) clockSync {
	if runtime.GOOS != "linux" {
		return clockSync{}
	}
	if _, err := exec.LookPath("timedatectl"); err != nil {
		return clockSync{}
	}
	out, err := exec.CommandContext(ctx, "timedatectl", "show", "-p", "NTP", "-p", "NTPSynchronized").Output()
	if err != nil {
		return clockSync{}
	}
	return parseClockSync(out)
}

func parseClockSync(out []byte) clockSync {
	m := map[string]string{}
	for _, line := range strings.Split(string(out), "\n") {
		if k, v, ok := strings.Cut(strings.TrimSpace(line), "="); ok {
			m[k] = v
		}
	}
	ntp, okNTP := m["NTP"]
	synced, okSynced := m["NTPSynchronized"]
	return clockSync{known: okNTP && okSynced, ntp: ntp == "yes", synced: synced == "yes"}
}

func runClock(ctx context.Context, e *Env) Result {
	c := readClockSync(ctx)
	switch {
	case !c.known:
		return skip("")
	case c.synced:
		return pass("This machine keeps its clock in sync.")
	case !c.ntp:
		return fixable(warn("This machine doesn't keep its clock in sync, so voice and sign-in links can fail once it drifts.", "Run rexctl doctor --fix to turn on time sync."))
	}
	return warn("This machine syncs its clock, but it isn't in sync yet.", "Check that this machine can connect to a time server on UDP port 123, then run rexctl doctor again in a few minutes.")
}

func fixClock(ctx context.Context, e *Env) error {
	return e.run(ctx, host.Step{
		Do:      "Turns on time sync for this machine's clock.",
		Command: "timedatectl set-ntp true",
		Undo:    "timedatectl set-ntp false",
		Run: func(ctx context.Context) error {
			return exec.CommandContext(ctx, "timedatectl", "set-ntp", "true").Run()
		},
	})
}
