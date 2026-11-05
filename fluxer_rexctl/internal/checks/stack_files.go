// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"fmt"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func init() {
	Register(Check{ID: "stack-files", Phases: Installed, Run: runStackFiles, Fix: fixStackFiles, SafeFix: true})
}

func runStackFiles(ctx context.Context, e *Env) Result {
	if e.Instance == nil || e.Instance.State == nil {
		return skip("")
	}
	m := release.Missing(e.Instance)
	if m == nil {
		return pass("Fluxer's files are all in place.")
	}
	r := fail(m.Message, m.Fix)
	if m.Restorable {
		r = fixable(r)
	}
	r.Details = m.Files
	return r
}

func fixStackFiles(ctx context.Context, e *Env) error {
	missing := release.MissingFiles(e.Instance)
	var restored []string
	err := e.run(ctx, host.Step{
		Do: fmt.Sprintf("Writes %s again from Fluxer %s.", ui.JoinAnd(missing), e.Instance.State.Release),
		Run: func(ctx context.Context) error {
			var err error
			if restored, err = release.RestoreMissing(e.Instance); err != nil {
				return err
			}
			if err := e.Instance.Save(); err != nil {
				return err
			}
			e.reload(ctx)
			return nil
		},
	})
	if err != nil || e.Project == nil {
		return err
	}
	if err := e.up(ctx); err != nil {
		return err
	}
	if restart := release.RestartFor(missing); len(restart) > 0 {
		return e.run(ctx, host.Step{
			Do: fmt.Sprintf("Restarts %s, which read these files.", ui.JoinAnd(restart)),
			Run: func(ctx context.Context) error {
				if restart := release.RestartFor(restored); len(restart) > 0 {
					return e.Project.Restart(ctx, restart...)
				}
				return nil
			},
		})
	}
	return nil
}
