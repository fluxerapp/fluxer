// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"fmt"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func init() {
	Register(Check{ID: "firewall", Phases: Installed, Run: runFirewall, Fix: fixFirewall})
}

func (e *Env) closedPorts(ctx context.Context) (host.Firewall, []host.Port) {
	if e.Instance == nil || e.Instance.State == nil {
		return host.FirewallNone, nil
	}
	fw, err := host.DetectFirewall(ctx)
	if err != nil || fw == host.FirewallNone {
		return host.FirewallNone, nil
	}
	var want []host.Port
	for _, p := range e.ports() {
		if e.mode() != modeBundled && (p.Number == 80 || p.Number == 443) {
			continue
		}
		want = append(want, host.Port{Number: p.Number, Proto: p.Proto})
	}
	closed, err := host.ClosedPorts(ctx, fw, want)
	if err != nil {
		return host.FirewallNone, nil
	}
	return fw, closed
}

func portNames(ports []host.Port) string {
	names := make([]string, len(ports))
	for i, p := range ports {
		names[i] = p.String()
	}
	return ui.JoinAnd(names)
}

func runFirewall(ctx context.Context, e *Env) Result {
	fw, closed := e.closedPorts(ctx)
	if fw == host.FirewallNone {
		return skip("")
	}
	if len(closed) == 0 {
		return pass(fmt.Sprintf("The ports Fluxer needs are open in %s.", fw))
	}
	r := fail(fmt.Sprintf("%s %s closed in %s.", portNames(closed), pick(len(closed), "is", "are"), fw), fmt.Sprintf("Run rexctl doctor --fix to open %s.", pick(len(closed), "it", "them")))
	r.Fixable = true
	return r
}

func fixFirewall(ctx context.Context, e *Env) error {
	fw, closed := e.closedPorts(ctx)
	if len(closed) == 0 {
		return nil
	}
	return e.run(ctx, host.Step{Do: fmt.Sprintf("Opens %s in %s.", portNames(closed), fw), Run: func(ctx context.Context) error {
		if err := host.OpenPorts(ctx, fw, closed); err != nil {
			return err
		}
		for _, p := range closed {
			e.Instance.State.Firewall = append(e.Instance.State.Firewall, instance.OpenedPort{Firewall: string(fw), Number: p.Number, Proto: p.Proto})
		}
		return e.Instance.Save()
	}})
}
