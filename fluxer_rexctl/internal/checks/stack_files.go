// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
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
	restored, err := release.RestoreMissing(e.Instance)
	if err != nil {
		return err
	}
	if err := e.Instance.Save(); err != nil {
		return err
	}
	e.Project = nil
	e.prepared = false
	e.prepare(ctx)
	if e.Project == nil {
		return nil
	}
	if err := e.up(ctx); err != nil {
		return err
	}
	if restart := release.RestartFor(restored); len(restart) > 0 {
		return e.Project.Restart(ctx, restart...)
	}
	return nil
}
