// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"strings"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type doctorJSON struct {
	OK     bool            `json:"ok"`
	Checks []checks.Result `json:"checks"`
}

func newDoctorCmd(app *App) *cobra.Command {
	var fix bool
	c := &cobra.Command{
		Use:   "doctor",
		Short: "Check your instance and this machine for problems",
		Args:  noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runDoctor(c.Context(), app, fix)
		},
	}
	c.Flags().BoolVar(&fix, "fix", false, "fix the problems rexctl can fix safely")
	app.jsonFlag(c)
	return c
}

func runDoctor(ctx context.Context, app *App, fix bool) error {
	l, err := app.Locate()
	if err != nil {
		return err
	}
	dir := l.Dir
	env := &checks.Env{Dir: dir}
	phase := checks.Installed
	inst, err := instance.Find(dir)
	switch {
	case err == nil:
		env.Instance = inst
		app.UI.Info("Using %s (%s).", inst.Dir, l.Why())
	case errors.Is(err, instance.ErrNotFound):
		phase = checks.Preinstall
	case errors.Is(err, instance.ErrLegacy):
		return ui.Refused("rexctl doesn't manage the instance in "+dir+" yet.", "Run "+app.takeOverCommand()+" to hand it to rexctl.")
	default:
		return ui.Wrap(err, "rexctl can't read the instance in "+dir+".", "Run rexctl doctor as the account that installed Fluxer.")
	}
	if err := rootlessGuard(env.Instance); err != nil {
		return err
	}
	if env.Instance != nil {
		app.rememberFirst(env.Instance)
		if fix {
			if err := managed(env.Instance); err != nil {
				return err
			}
		}
		e, err := app.Engine(ctx, env.Instance)
		env.SetEngine(e, err)
		if err == nil {
			env.Admin = adminapi.New(adminapi.NewExec(e.Named(env.Instance.Project(), env.Instance.Dir)), app.Version)
		}
	}
	run := func() error {
		results := diagnose(ctx, app, env, phase, fix)
		if env.Instance != nil {
			warnUnusedRootFiles(ctx, app, env.Instance)
		}
		if env.Instance != nil && app.UI.Interactive() {
			instance.SeenEdits(env.Instance)
		}
		var ej *instance.EjectedError
		if errors.As(env.Instance.Managed(), &ej) && !app.Flags.JSON {
			defer app.UI.Info("%s %s", ej.Error(), ej.Fix())
		}
		return reportDoctor(app, results)
	}
	if fix && env.Instance != nil {
		return app.Locked(ctx, env.Instance, run)
	}
	return run()
}

func diagnose(ctx context.Context, app *App, env *checks.Env, phase checks.Phase, fix bool) []checks.Result {
	check := func() []checks.Result {
		var results []checks.Result
		_ = app.UI.Step(ctx, "Checking your instance", func(ctx context.Context, _ *ui.Step) error {
			results = checks.Run(ctx, env, phase)
			return nil
		})
		return results
	}
	results := check()
	if !fix {
		return results
	}
	fixed := map[string]bool{}
	skipped := map[string]string{}
	failed := map[string]bool{}
	for round := 0; round < 3; round++ {
		progressed := false
		for _, i := range fixOrder(results) {
			r := results[i]
			if !r.Fixable || skipped[r.ID] != "" {
				continue
			}
			c, _ := checks.Get(r.ID)
			if !c.SafeFix {
				if !app.UI.Interactive() && !app.UI.Yes() {
					skipped[r.ID] = "Run rexctl doctor --fix in a terminal, or pass --yes, to confirm this fix."
					continue
				}
				if !app.UI.Yes() {
					app.UI.Warn(r.Message, r.Fix)
				}
				ok, err := app.UI.Confirm("Fix this?", true)
				if err != nil || !ok {
					skipped[r.ID] = r.Fix
					continue
				}
			}
			var after checks.Result
			err := app.UI.Step(ctx, "Fixing it", func(ctx context.Context, _ *ui.Step) error {
				var err error
				after, err = checks.ApplyFix(ctx, env, r.ID)
				return err
			})
			if err != nil {
				app.UI.Detail("%v", err)
				results[i] = checks.Recheck(ctx, env, c)
				failed[r.ID] = true
				continue
			}
			delete(failed, r.ID)
			results[i] = after
			if after.Fixed {
				fixed[r.ID] = true
				progressed = true
			}
		}
		if env.Staged() {
			staged := env.StagedBy()
			err := app.UI.Step(ctx, "Applying the fixes", func(ctx context.Context, s *ui.Step) error {
				if err := checks.Commit(ctx, env, deploy.ApplyOptions{Wait: true}); err != nil {
					return err
				}
				s.Done("Applied the fixes.")
				return nil
			})
			if err != nil {
				app.UI.Detail("%v", err)
				for _, id := range staged {
					delete(fixed, id)
					failed[id] = true
				}
			} else {
				for _, id := range staged {
					fixed[id] = true
				}
				progressed = true
			}
		}
		if !progressed {
			break
		}
		results = check()
	}
	for i, r := range results {
		switch {
		case fixed[r.ID] && r.Status == checks.Pass:
			results[i].Fixed = true
		case skipped[r.ID] != "" && (r.Status == checks.Warn || r.Status == checks.Fail):
			results[i].Fix = skipped[r.ID]
		case failed[r.ID] && r.Fixable && r.Manual != "":
			results[i].Fix = "rexctl couldn't fix this. " + r.Manual
		case failed[r.ID] && r.Fixable && strings.Contains(r.Fix, "rexctl doctor --fix"):
			results[i].Fix = "rexctl couldn't fix this. Run rexctl doctor --fix --verbose to see why."
		case failed[r.ID] && r.Fixable:
			results[i].Fix = "rexctl couldn't fix this. " + r.Fix
		}
	}
	return results
}

func fixOrder(results []checks.Result) []int {
	var first, last []int
	for i, r := range results {
		if r.ID == "services" || r.ID == "stale-containers" {
			last = append(last, i)
		} else {
			first = append(first, i)
		}
	}
	return append(first, last...)
}

func reportDoctor(app *App, results []checks.Result) error {
	for _, r := range results {
		if app.Flags.JSON {
			break
		}
		switch r.Status {
		case checks.Pass:
			if r.Fixed {
				app.UI.OK("Fixed. %s", r.Message)
			} else {
				app.UI.OK("%s", r.Message)
			}
		case checks.Warn:
			app.UI.Warn(r.Message, r.Fix)
		case checks.Fail:
			app.UI.Fail(r.Message, r.Fix)
		case checks.Skip:
			if r.Message != "" {
				app.UI.Detail("%s", r.Message)
			}
		}
		for _, d := range r.Details {
			app.UI.Detail("  %s", d)
		}
	}
	if app.Flags.JSON {
		if err := app.UI.JSON(doctorJSON{OK: !checks.Failed(results), Checks: results}); err != nil {
			return err
		}
	}
	if checks.Failed(results) {
		app.SetExit(ui.ExitRefused)
		return nil
	}
	if checks.AllGood(results) {
		app.UI.AllGreen()
	}
	return nil
}
