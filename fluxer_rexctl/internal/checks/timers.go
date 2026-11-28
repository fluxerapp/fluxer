// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
)

func init() {
	Register(Check{ID: "host-timers", Phases: Installed, Run: runHostTimers})
}

var timerWhat = map[string]string{instance.TimersUpgrade: "automatic upgrades", instance.TimersBackup: "scheduled backups"}

func TimersMissing(st *instance.State) ([]string, []string, bool) {
	var what, configs []string
	if st == nil {
		return nil, nil, false
	}
	onlyElsewhere := true
	for _, kind := range []string{instance.TimersUpgrade, instance.TimersBackup} {
		t, ok := st.TimersNeeded[kind]
		if !ok {
			continue
		}
		missing, elsewhere := host.TimersOnMachine(t.Names, t.Runs)
		if !missing && !elsewhere {
			continue
		}
		onlyElsewhere = onlyElsewhere && !missing
		what = append(what, timerWhat[kind])
		configs = append(configs, t.Config)
	}
	return what, configs, len(what) > 0 && onlyElsewhere
}

func runHostTimers(_ context.Context, e *Env) Result {
	st := e.state()
	if st == nil || len(st.TimersNeeded) == 0 {
		return skip("")
	}
	what, configs, elsewhere := TimersMissing(st)
	if len(what) == 0 {
		var added []string
		for _, kind := range []string{instance.TimersUpgrade, instance.TimersBackup} {
			if _, ok := st.TimersNeeded[kind]; ok {
				added = append(added, timerWhat[kind])
			}
		}
		return pass("The timers for " + strings.Join(added, " and ") + " that you added yourself are on this machine.")
	}
	problem := "The timers for " + strings.Join(what, " and ") + " aren't on this machine, and rexctl can't add them, because " + host.SystemUnitDir() + " is read-only."
	fix := "Add the units that rexctl doctor --verbose shows to " + host.SystemUnitDir() + " through whatever manages it on this machine."
	if host.NixOS() {
		fix = "Add the lines that rexctl doctor --verbose shows inside the outer braces of /etc/nixos/configuration.nix, then run sudo nixos-rebuild switch."
	}
	if elsewhere {
		problem = "The timers for " + strings.Join(what, " and ") + " on this machine run rexctl for another folder, and rexctl can't change them, because " + host.SystemUnitDir() + " is read-only."
		fix = "Replace them with the units that rexctl doctor --verbose shows, through whatever manages " + host.SystemUnitDir() + "."
		if host.NixOS() {
			fix = "Replace their lines in /etc/nixos/configuration.nix with the ones that rexctl doctor --verbose shows, then run sudo nixos-rebuild switch."
		}
	}
	r := warn(problem, fix)
	for _, c := range configs {
		r.Details = append(r.Details, strings.Split(strings.TrimRight(c, "\n"), "\n")...)
	}
	return r
}
