// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"fmt"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func init() {
	Register(Check{ID: "pending-change", Phases: Installed, Run: runPendingChange, Fix: fixPendingChange, SafeFix: true})
	Register(Check{ID: "upgrade-unfinished", Phases: Installed, Run: runUpgradeUnfinished})
}

func runPendingChange(ctx context.Context, e *Env) Result {
	st := e.state()
	if st == nil || st.Pending == nil {
		return skip("")
	}
	msg := "A settings change didn't finish."
	if len(st.Pending.Keys) > 0 {
		msg = fmt.Sprintf("The change to %s didn't finish.", ui.JoinAnd(st.Pending.Keys))
	}
	r := fixable(fail(msg, "Run rexctl doctor --fix to put the old settings back."))
	r.Details = st.Pending.Keys
	return r
}

func fixPendingChange(ctx context.Context, e *Env) error {
	if e.state() == nil || e.Engine == nil {
		return errNoProject
	}
	st := e.state()
	if st.Pending == nil {
		return nil
	}
	what := "the last change"
	if len(st.Pending.Keys) > 0 {
		what = "the change to " + ui.JoinAnd(st.Pending.Keys)
	}
	do := fmt.Sprintf("Puts back the settings from before %s, writes Fluxer's files from them and starts Fluxer.", what)
	if st.Stopped {
		do = fmt.Sprintf("Puts back the settings from before %s and writes Fluxer's files from them.", what)
	}
	var steps []host.Step
	if deploy.PendingIPv6(st) && !st.Stopped {
		steps = append(steps, host.Step{Do: "Stops Fluxer and its voice servers first, because the IPv6 setting changes."})
	}
	return e.run(ctx, append(steps, host.Step{
		Do: do,
		Run: func(ctx context.Context) error {
			if err := deploy.Resolve(ctx, e.Instance, e.Engine); err != nil {
				return err
			}
			e.env = nil
			return nil
		},
	})...)
}

func runUpgradeUnfinished(ctx context.Context, e *Env) Result {
	st := e.state()
	if st == nil || st.Upgrade == nil {
		return skip("")
	}
	return fail(fmt.Sprintf("The upgrade to %s didn't finish.", st.Upgrade.To), release.UnfinishedFix(st.Upgrade))
}
