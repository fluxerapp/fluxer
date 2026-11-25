// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime/upgrade"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func upgradeEngine(ctx context.Context, app *App, inst *instance.Instance, f *upgradeFlags) (*upgrade.Engine, error) {
	r, err := app.Runtime(ctx, inst)
	if err != nil {
		return nil, err
	}
	eng := &upgrade.Engine{Inst: inst, RT: r, UI: app.UI}
	if r.Kind() == rt.Compose {
		if f == nil {
			f = &upgradeFlags{}
		}
		eng.Driver = &composeUpgrader{app: app, inst: inst, f: f}
		return eng, nil
	}
	eng.AfterRestore = func(ctx context.Context) error {
		if err := adoptSecrets(ctx, inst, r); err != nil {
			return err
		}
		if u := inst.State.Upgrade; u != nil && u.Backup != "" {
			if m, err := r.Backups().Verify(ctx, u.Backup); err == nil {
				return forgetRestoredMigrations(inst, m.CreatedAt)
			}
		}
		return nil
	}
	return eng, nil
}

func backupPolicy(inst *instance.Instance, f *upgradeFlags) string {
	switch {
	case f.NoBackup:
		return settings.BackupOff
	case f.Backup != "":
		return f.Backup
	}
	switch v := inst.State.Settings[settings.BackupsBefore]; v {
	case settings.BackupOff, settings.BackupWithoutUploads:
		return v
	}
	return settings.BackupFull
}

func upgradePolicy(f *upgradeFlags) string {
	switch {
	case f.Prefetch:
		return ""
	case f.NoBackup:
		return settings.BackupOff
	case f.Backup == settings.BackupWithoutUploads:
		return settings.BackupWithoutUploads
	}
	return settings.BackupFull
}

func enginePolicy(eng *upgrade.Engine, f *upgradeFlags) string {
	policy := upgradePolicy(f)
	if policy == settings.BackupWithoutUploads && eng.RT.Kind() != rt.Compose {
		return settings.BackupFull
	}
	return policy
}

func runEngine(ctx context.Context, app *App, inst *instance.Instance, eng *upgrade.Engine, rel *release.Release, f *upgradeFlags) error {
	policy := enginePolicy(eng, f)
	if policy != upgradePolicy(f) {
		app.UI.Info("A native backup always holds uploads, so this one does too.")
	}
	if policy == settings.BackupOff {
		app.UI.Warn("Skipped the backup.", "")
	}
	res, err := eng.Run(ctx, rel, upgrade.Options{Prefetch: f.Prefetch, Backup: policy})
	if err != nil {
		return err
	}
	return reportUpgrade(app, inst, res)
}

func reportUpgrade(app *App, inst *instance.Instance, res upgrade.Result) error {
	fromShort, toShort := res.From, res.To
	switch res.Outcome {
	case upgrade.Prefetched:
		app.UI.Info("Run rexctl upgrade to switch to Fluxer %s.", toShort)
	case upgrade.Upgraded:
		pruneBackups(app, inst)
		app.outcome = fmt.Sprintf("Upgraded to Fluxer %s.", toShort)
		done := ui.Success{Title: fmt.Sprintf("Your instance is on Fluxer %s.", toShort), Next: []string{"rexctl status", "rexctl rollback"}}
		if res.Backup == "" {
			done.Next = []string{"rexctl status"}
		}
		if res.Stopped {
			done.Title += " It stays stopped until you run rexctl start."
			done.Next = []string{"rexctl start"}
		}
		if res.Backup != "" {
			done.Rows = []ui.KV{{Key: "Backup", Value: filepath.Base(res.Backup), Hint: "in " + filepath.Dir(res.Backup)}}
		}
		app.UI.Success(done)
	case upgrade.WentBack:
		msg := wentBackLine(res.Cause, toShort, fromShort)
		app.outcome = msg
		app.UI.Fail(msg, "")
		app.UI.Detail("%v", res.Cause)
		if res.Restored {
			why := "Run rexctl logs to see why."
			if res.Log != "" {
				why = "Read " + res.Log + " to see why."
			}
			fixLine(app, why+" rexctl restored the backup from before the upgrade, so your data is as it was then.")
			app.SetExit(ui.ExitFailed)
			return nil
		}
		if res.Log != "" {
			fixLine(app, fmt.Sprintf("Read %s to see why. Your database stays as Fluxer %s left it.", res.Log, toShort))
		} else {
			fixLine(app, "Run rexctl logs to see why. Your database stays as Fluxer "+toShort+" left it.")
		}
		if res.Backup != "" {
			fixLine(app, fmt.Sprintf("If %s doesn't work with it, run rexctl rollback --restore-database. That restores the backup from before the upgrade and loses what changed since.", fluxerName(fromShort)))
		}
		app.SetExit(ui.ExitFailed)
	}
	return nil
}

func wentBackLine(cause error, toShort, fromShort string) string {
	switch {
	case errors.Is(cause, upgrade.ErrInterrupted):
		return fmt.Sprintf("The upgrade to Fluxer %s stopped before it finished, so rexctl went back to %s.", toShort, fluxerName(fromShort))
	case errors.Is(cause, upgrade.ErrStart):
		return fmt.Sprintf("Fluxer %s didn't start, so rexctl went back to %s.", toShort, fluxerName(fromShort))
	case errors.Is(cause, upgrade.ErrRestore):
		return fmt.Sprintf("The backup didn't restore, so rexctl went back to %s.", fluxerName(fromShort))
	}
	return fmt.Sprintf("rexctl couldn't switch to Fluxer %s, so it went back to %s.", toShort, fluxerName(fromShort))
}
