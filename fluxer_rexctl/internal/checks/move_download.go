// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"os"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func init() {
	Register(Check{ID: "move-download", Phases: Installed, Run: runMoveDownload, Fix: fixMoveDownload, SafeFix: true})
}

func runMoveDownload(_ context.Context, e *Env) Result {
	if len(e.Downloads) == 0 {
		return skip("")
	}
	r := fixable(warn("A move that stopped left its download of Fluxer in "+ui.JoinAnd(e.Downloads)+".", "Run rexctl doctor --fix to remove it."))
	r.Details = e.Downloads
	return r
}

func fixMoveDownload(ctx context.Context, e *Env) error {
	steps := make([]host.Step, 0, len(e.Downloads))
	for _, dir := range e.Downloads {
		steps = append(steps, host.Step{
			Do:  "Removes " + dir + ", the download a move that stopped left behind.",
			Run: func(context.Context) error { return os.RemoveAll(dir) },
		})
	}
	return e.run(ctx, steps...)
}
