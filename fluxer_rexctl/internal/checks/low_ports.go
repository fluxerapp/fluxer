// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"fmt"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
)

func init() {
	Register(Check{ID: "low-ports", Phases: Preinstall | Installed, Run: runLowPorts, Fix: fixLowPorts, SafeFix: true})
}

func runLowPorts(_ context.Context, e *Env) Result {
	if !e.rootless() || e.mode() != modeBundled {
		return skip("")
	}
	if host.LowPortsBlocked(true) {
		proxy := "run rexctl install --behind-proxy"
		if e.installed() {
			proxy = "run rexctl proxy on"
		}
		r := fixable(fail(fmt.Sprintf("Rootless %s can't use ports 80 and 443, because only root can use ports below 1024 on this machine.", e.engineName()), "Run rexctl doctor --fix to allow them, or "+proxy+" to use your own proxy."))
		r.Manual = "Ask an administrator to add " + host.LowPortsSetting + " to " + host.LowPortsFile + " and run sudo sysctl --system, or " + proxy + " to use your own proxy."
		return r
	}
	return pass(fmt.Sprintf("Rootless %s can use ports 80 and 443.", e.engineName()))
}

func fixLowPorts(ctx context.Context, _ *Env) error {
	return host.AllowLowPorts(ctx)
}
