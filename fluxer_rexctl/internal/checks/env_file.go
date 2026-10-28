// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func init() {
	Register(Check{ID: "env-file", Phases: Installed, Run: runEnvFile, Fix: fixEnvFile})
	Register(Check{ID: "stack-edits", Phases: Installed, Run: runStackEdits, Fix: fixStackEdits})
}

func (e *Env) drift() (*instance.DriftReport, error) {
	if e.state() == nil {
		return nil, errNoProject
	}
	return instance.Drift(e.Instance)
}

func runEnvFile(ctx context.Context, e *Env) Result {
	if e.state() == nil || e.state().Ejected != nil {
		return skip("")
	}
	r, err := e.drift()
	if err != nil {
		r := fail("rexctl can't read the .env file in your instance's folder.", "Check that you can read it, then run rexctl doctor.")
		r.Details = []string{err.Error()}
		return r
	}
	switch {
	case r.Env == nil:
		return pass("rexctl keeps all of your instance's settings.")
	case r.Env.Same:
		return fixable(warn("Your instance has a .env file that only repeats settings rexctl keeps.", "Run rexctl doctor --fix to remove it."))
	}
	fix := r.Env.Fix()
	for _, c := range r.Env.Changes {
		fix += "\n    " + c.Command()
	}
	res := fail("Your instance has a .env file, and rexctl doesn't read it.", fix)
	if len(r.Env.Unused) > 0 {
		return res
	}
	return fixable(res)
}

func fixEnvFile(ctx context.Context, e *Env) error {
	r, err := e.drift()
	if err != nil || r.Env == nil || len(r.Env.Unused) > 0 {
		return err
	}
	if r.Env.Same {
		return instance.RemoveEnv(e.Instance)
	}
	for k, v := range r.Env.Settings() {
		e.stage(k, v)
	}
	for k, v := range r.Env.Values(instance.ChangeSecret) {
		e.stageSecret(k, v)
	}
	for k, v := range r.Env.Values(instance.ChangeAdmin) {
		e.stageAdmin(k, v)
	}
	e.importEnv = true
	return nil
}

func runStackEdits(ctx context.Context, e *Env) Result {
	if e.state() == nil {
		return skip("")
	}
	r, err := e.drift()
	if err == nil && len(r.Changed) == 0 && len(e.keptEdits) > 0 {
		return keptEditsResult(e.keptEdits)
	}
	if err == nil && len(r.Changed) == 0 {
		if copies := instance.UnseenEdits(e.Instance); len(copies) > 0 {
			res := warn("rexctl put back its own version of files you changed. Your copies are in "+filepath.Dir(copies[0])+".", "Put Compose changes in "+instance.CustomCompose+" with rexctl config edit, and Caddy directives in "+instance.CaddyDropInDir+".")
			res.Details = copies
			return res
		}
	}
	if err != nil || len(r.Changed) == 0 {
		return skip("")
	}
	msg := fmt.Sprintf("%s was changed by hand. rexctl writes this file and puts its own version back on the next change.", r.Changed[0])
	if len(r.Changed) > 1 {
		msg = fmt.Sprintf("%s were changed by hand. rexctl writes these files and puts its own versions back on the next change.", ui.JoinAnd(r.Changed))
	}
	res := fixable(warn(msg, "Run rexctl doctor --fix to put rexctl's version back now and keep a copy of yours. Put Compose changes in "+instance.CustomCompose+" with rexctl config edit, and Caddy directives in "+instance.CaddyDropInDir+"."))
	res.Details = r.Changed
	return res
}

func fixStackEdits(ctx context.Context, e *Env) error {
	edits, err := instance.KeepEdits(e.Instance)
	if err != nil {
		return err
	}
	for _, ed := range edits {
		e.keptEdits = append(e.keptEdits, [2]string{ed.Name, ed.Copy})
		if err := os.Remove(e.Instance.Generated(ed.Name)); err != nil && !errors.Is(err, os.ErrNotExist) {
			return err
		}
	}
	if _, err := release.RestoreMissing(e.Instance); err != nil {
		return err
	}
	if err := e.Instance.Save(); err != nil {
		return err
	}
	e.Project = nil
	e.prepared = false
	e.prepare(ctx)
	return e.up(ctx)
}

func keptEditsResult(kept [][2]string) Result {
	if len(kept) == 1 {
		return pass(fmt.Sprintf("Put rexctl's %s back. Your edited copy is in %s.", kept[0][0], kept[0][1]))
	}
	r := pass("Put rexctl's versions of your edited files back. Your copies are in " + filepath.Dir(kept[0][1]) + ".")
	for _, k := range kept {
		r.Details = append(r.Details, k[0]+" is in "+k[1])
	}
	return r
}
