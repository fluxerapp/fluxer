// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime/upgrade"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type rollbackFlags struct {
	DryRun    bool
	RestoreDB bool
	Confirm   string
	Resume    string
	NoSafety  bool
}

func newRollbackCmd(app *App) *cobra.Command {
	var f rollbackFlags
	c := &cobra.Command{
		Use:   "rollback",
		Short: "Go back to the release before the last upgrade",
		Args:  noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runRollback(c.Context(), app, &f)
		},
	}
	c.Flags().BoolVar(&f.DryRun, "dry-run", false, "show what would happen without changing anything")
	c.Flags().BoolVar(&f.RestoreDB, "restore-database", false, "also restore the backup taken before the upgrade, which rexctl does by itself once the newer release has changed your data")
	c.Flags().BoolVar(&f.NoSafety, "no-safety-backup", false, "don't back up the current instance before a restore replaces it")
	c.Flags().StringVar(&f.Resume, release.ResumeFlag, "", "")
	_ = c.Flags().MarkHidden(release.ResumeFlag)
	confirmFlag(c, &f.Confirm)
	return c
}

func runRollback(ctx context.Context, app *App, f *rollbackFlags) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	if err := managed(inst); err != nil {
		return err
	}
	native := rt.KindOf(inst) != rt.Compose
	if rt.KindOf(inst) == rt.Native && !f.DryRun {
		if err := app.nativeRoot(inst.Dir); err != nil {
			return err
		}
	}
	if !native && inst.State.Upgrade != nil && !f.DryRun {
		if handedOver, err := updateBeforeRecovery(ctx, app, f.Resume, "", "rexctl rollback"); err != nil || handedOver {
			return err
		}
	}
	eng, err := upgradeEngine(ctx, app, inst, nil)
	if err != nil {
		return err
	}
	if pend := inst.State.Upgrade; pend != nil {
		if f.DryRun {
			recoveryPlan(app, eng, pend)
			if phaseIndex(pend.Phase) < phaseIndex(release.PhaseVerified) && !f.RestoreDB {
				app.UI.Info("Dry run. Nothing changed.")
				return nil
			}
		} else {
			back := pend.From == pend.To || upgrade.Rollback(pend)
			res, err := recoverPending(ctx, app, inst, eng)
			if err != nil {
				return err
			}
			reportRecovery(app, inst, res)
			if back && res.Outcome == upgrade.Upgraded || res.Outcome != upgrade.Upgraded && !f.RestoreDB {
				return nil
			}
		}
	}
	return app.Locked(ctx, inst, func() error {
		if err := managed(inst); err != nil {
			return err
		}
		var e *engine.Engine
		if !native {
			var err error
			if f.DryRun {
				err = app.drift(inst, false, true)
			} else {
				e, err = app.guard(ctx, inst, true)
			}
			if err != nil {
				return err
			}
		}
		list, err := backup.List(inst.BackupDir())
		if err != nil {
			return ui.Wrap(err, "Couldn't read your backups.", "Run rexctl backup list to see why.")
		}
		choices := rollbackChoices(eng, inst, list, !native)
		restoreDB := f.RestoreDB || dataMoved(eng, inst, choices, native)
		if restoreDB && !f.RestoreDB {
			app.UI.Info("Fluxer %s has changed your data, so going back restores the backup from before the upgrade.", inst.State.Release)
		}
		c, err := pickRollback(app, eng, inst, choices, restoreDB)
		if err != nil {
			return err
		}
		cur := inst.State.Release
		schemaChanged := false
		if c.backup == nil {
			if err := checkRollbackRelease(ctx, inst, eng, e, native, c.to); err != nil {
				return err
			}
			schemaChanged = true
			if rel, err := release.Resolve(ctx, cur); err == nil {
				schemaChanged = rel.SchemaChanged
			}
		} else if !native && c.backup.WithoutUploads() {
			if e == nil {
				if e, err = app.Engine(ctx, inst); err != nil {
					return err
				}
			}
			if err := checkUploadsInPlace(ctx, inst.Dir, inst, e, c.backup.Path); err != nil {
				return err
			}
		}
		if f.DryRun {
			printRollbackPlan(app, cur, c, schemaChanged)
			return nil
		}
		if c.backup == nil {
			if schemaChanged {
				app.UI.Warn(fmt.Sprintf("Fluxer %s may have changed the database in a way %s can't read.", cur, c.to), "If it doesn't start, rexctl comes back to "+cur+", and rexctl rollback --restore-database goes back with the backup.")
				if with, ok := withBackup(choices, c.to); ok && app.UI.Interactive() {
					restore, err := app.UI.Confirm("Restore the backup from before the upgrade too?", false)
					if err != nil {
						return err
					}
					if restore {
						c = with
					}
				}
			}
		}
		if c.backup == nil {
			if !app.UI.Interactive() && !app.UI.Yes() {
				return &ui.ExitError{Code: ui.ExitUsage, Msg: fmt.Sprintf("This takes your instance back to Fluxer %s. Your database stays as it is.", c.to), Fix: "Pass --yes to go ahead."}
			}
			ok, err := app.UI.Confirm(fmt.Sprintf("Go back to Fluxer %s?", c.to), true)
			if err != nil {
				return err
			}
			if !ok {
				app.UI.Info("Nothing changed.")
				return nil
			}
		} else if err := app.UI.ConfirmDestructive(restoreWhat(c, time.Now()), inst.Domain(), f.Confirm); err != nil {
			return err
		}
		safety := ""
		if c.backup != nil {
			if safety, err = safetyBackup(ctx, app, inst, c.backup.WithoutUploads(), f.NoSafety); err != nil {
				if errors.Is(err, errDeclined) {
					app.UI.Info("Nothing changed.")
					return nil
				}
				return err
			}
			if err := noteRestore(inst, c.backup.Path, safety); err != nil {
				return withSafety(err, safety)
			}
		}
		switch {
		case c.backup != nil && !native:
			return withSafety(restoreComposeBackup(ctx, app, inst, e, c), safety)
		case c.backup != nil && c.to == cur:
			return withSafety(restoreInPlace(ctx, app, eng, c), safety)
		}
		archive, when := "", ""
		if c.backup != nil {
			archive, when = c.backup.Path, backupTime(c.backup.CreatedAt)
		}
		res, err := eng.Rollback(ctx, c.to, archive)
		if err != nil {
			return withSafety(err, safety)
		}
		if res.Outcome == upgrade.Upgraded && len(res.Put) > 0 {
			app.UI.Info("rexctl put back the settings Fluxer %s had, which are %s.", c.to, ui.JoinAnd(res.Put))
		}
		return reportRollback(app, res, when)
	})
}

func dataMoved(eng *upgrade.Engine, inst *instance.Instance, choices []rollbackChoice, native bool) bool {
	prev := inst.State.PreviousRelease
	if !native || prev == "" || !eng.DataNewer(prev) {
		return false
	}
	_, ok := defaultRollback(inst, choices, true)
	return ok
}

func checkRollbackRelease(ctx context.Context, inst *instance.Instance, eng *upgrade.Engine, e *engine.Engine, native bool, to string) error {
	if !native {
		if _, err := release.SnapshotState(inst, to); err != nil {
			return ui.Wrap(err, fmt.Sprintf("Couldn't read the settings of Fluxer %s.", to), inst.State.RestoreDatabaseFix())
		}
	}
	if err := eng.CheckBack(ctx, to); err != nil {
		return err
	}
	if e == nil {
		return nil
	}
	refs, _ := release.Images(inst, to)
	if sp, ok := checks.PullSpace(ctx, e, inst.Dir, refs, release.ImageSizes(inst, to)); ok && sp.Short() {
		return ui.Refused(
			fmt.Sprintf("Going back to Fluxer %s downloads its images again, which needs about %s of free disk space in %s, and %s is free.", to, ui.Bytes(int64(sp.Need)), sp.Path, ui.Bytes(int64(sp.Free))),
			spaceFix(e, sp, "rexctl rollback"),
		)
	}
	return nil
}

func withBackup(choices []rollbackChoice, to string) (rollbackChoice, bool) {
	for _, c := range choices {
		if c.to == to && c.backup != nil {
			return c, true
		}
	}
	return rollbackChoice{}, false
}

func printRollbackPlan(app *App, cur string, c rollbackChoice, schemaChanged bool) {
	switch {
	case c.backup == nil:
		app.UI.Info("Fluxer goes back to %s and restarts.", c.to)
		app.UI.Info("Your database stays as it is.")
		if schemaChanged {
			app.UI.Info("Fluxer %s may have changed the database in a way %s can't read.", cur, c.to)
		}
	case c.backup.WithoutUploads():
		app.UI.Info("Your instance goes back to the backup from %s, on Fluxer %s.", backupTime(c.backup.CreatedAt), c.to)
		app.UI.Info("Messages and changes since then are lost. Uploads stay as they are.")
	default:
		app.UI.Info("Your instance goes back to the backup from %s, on Fluxer %s.", backupTime(c.backup.CreatedAt), c.to)
		app.UI.Info("Messages, uploads and changes since then are lost.")
	}
	app.UI.Info("Dry run. Nothing changed.")
}

func restoreComposeBackup(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine, c rollbackChoice) error {
	when := backupTime(c.backup.CreatedAt)
	return app.UI.Step(ctx, "Restoring the backup from "+when, func(ctx context.Context, s *ui.Step) error {
		restored, err := backup.Restore(ctx, c.backup.Path, inst.Dir, e, backup.RestoreOptions{
			Progress: func(stage string) {
				if t := rt.RestoreStageTitle(stage); t != "" {
					s.Title(t)
				}
			},
			Adopted: matchRestoredSites(app),
		})
		if err != nil {
			if errors.Is(err, backup.ErrUploadsNotHere) {
				return uploadsNotHere(inst, c.backup.Domain, c.backup.Project, err)
			}
			return err
		}
		if err := e.Named(restored.Project(), restored.Dir).WaitReady(ctx, readyTimeout, rt.ShowReadiness(s, "Starting Fluxer "+c.to)); err != nil {
			return ui.Wrap(err, fmt.Sprintf("Restored the backup, but Fluxer %s didn't start.", c.to), "Run rexctl doctor to see what's wrong.")
		}
		s.Done("Went back to %s with the backup from %s. Your instance is running.", c.to, when)
		return nil
	})
}

func backupFor(inst *instance.Instance, list []backup.Info, version string) *backup.Info {
	if inst.State == nil {
		return backup.NewestOf(list, version)
	}
	if u := inst.State.Upgrade; u != nil && u.Backup != "" {
		if b := knownBackup(list, u.Backup); b != nil {
			return b
		}
	}
	return rollbackTarget(inst, list, version)
}

func rollbackTarget(inst *instance.Instance, list []backup.Info, version string) *backup.Info {
	if inst.State != nil && inst.State.PreviousBackup != "" {
		if b := knownBackup(list, inst.State.PreviousBackup); b != nil && b.Release == version {
			return b
		}
	}
	return backup.NewestOf(list, version)
}

func knownBackup(list []backup.Info, path string) *backup.Info {
	if b := backup.Find(list, path); b != nil {
		return b
	}
	b, err := backup.Stat(path)
	if err != nil {
		return nil
	}
	return b
}
