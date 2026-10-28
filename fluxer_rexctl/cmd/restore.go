// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"runtime"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const restoreReadyTimeout = 10 * time.Minute

func newRestoreCmd(app *App) *cobra.Command {
	var confirm, engineFlag string
	c := &cobra.Command{
		Use:   "restore <file>",
		Short: "Restore your instance from a backup",
		Long:  "Restore your instance from a backup. This works on a new machine too. Copy the backup file there and run this command.",
		Args:  exactArgs("the backup file"),
		RunE: func(c *cobra.Command, args []string) error {
			return runRestore(c.Context(), app, args[0], confirm, engineFlag)
		},
	}
	confirmFlag(c, &confirm)
	c.Flags().StringVar(&engineFlag, "engine", "", "move the instance to another engine while restoring (docker)")
	return c
}

func runRestore(ctx context.Context, app *App, arg, confirm, engineFlag string) error {
	if engineFlag != "" && engineFlag != string(engine.Docker) {
		return ui.Usage(fmt.Sprintf("rexctl can restore onto docker, not %s.", engineFlag))
	}
	toDocker := engineFlag == string(engine.Docker)
	path := resolveBackupPath(app, arg)
	m, err := backup.ReadManifest(path)
	if err != nil {
		return backupFileErr(path, err)
	}
	dir, err := app.Dir()
	if err != nil {
		return err
	}
	existing, err := instance.Find(dir)
	switch {
	case err == nil:
	case errors.Is(err, instance.ErrNotFound):
		existing = nil
	case errors.Is(err, instance.ErrLegacy):
		return ui.Refused(
			fmt.Sprintf("The Fluxer instance in %s isn't managed by rexctl yet.", dir),
			fmt.Sprintf("Run %s first, or pass --dir to restore into another folder.", app.takeOverCommand()),
		)
	default:
		return err
	}
	if err := rootlessGuard(existing); err != nil {
		return err
	}
	if existing != nil {
		if err := managed(existing); err != nil {
			return err
		}
	}

	made := backupTime(m.CreatedAt)
	domain := m.Domain
	what := fmt.Sprintf("This restores %s in %s from the backup made on %s.", m.Domain, dir, made)
	if existing != nil {
		domain = existing.Domain()
		what = fmt.Sprintf("This replaces everything on %s with the backup made on %s.", domain, made)
	}
	old := oldEngineRef(existing)
	if toDocker && old.Kind == string(engine.Podman) {
		what += " Fluxer stops on Podman and starts on Docker. Podman keeps its copy of your data."
	}
	if err := app.UI.ConfirmDestructive(what, domain, confirm); err != nil {
		return err
	}

	run := func() error {
		e, err := restoreEngine(ctx, app, existing, dir, toDocker)
		if err != nil {
			return err
		}
		if toDocker && old.ID != "" && old.ID != e.ID {
			stopOnOldEngine(ctx, app, existing, old)
		}
		return restoreInto(ctx, app, e, path, dir, m, existing, old)
	}
	if existing != nil {
		return app.Locked(ctx, existing, run)
	}
	return run()
}

func oldEngineRef(inst *instance.Instance) instance.EngineRef {
	if inst == nil || inst.State == nil {
		return instance.EngineRef{}
	}
	return inst.State.Engine
}

func stopOnOldEngine(ctx context.Context, app *App, inst *instance.Instance, old instance.EngineRef) {
	err := app.UI.Step(ctx, "Stopping Fluxer on "+engine.Kind(old.Kind).Name(), func(ctx context.Context, s *ui.Step) error {
		e, err := engine.Connect(ctx, engine.ConnectOptions{Host: old.Host, ID: old.ID, AllowOld: true})
		if err != nil {
			return err
		}
		defer e.Close()
		if err := e.Named(inst.Project(), inst.Dir).Stop(context.WithoutCancel(ctx)); err != nil {
			return err
		}
		s.Done("Stopped Fluxer on %s.", engine.Kind(old.Kind).Name())
		return nil
	})
	if err != nil && !errors.Is(err, context.Canceled) {
		app.UI.Detail("%v", err)
	}
}

func restoreInto(ctx context.Context, app *App, e *engine.Engine, path, dir string, m *backup.Manifest, existing *instance.Instance, old instance.EngineRef) error {
	var inst *instance.Instance
	err := app.UI.Step(ctx, "Checking the backup", func(ctx context.Context, s *ui.Step) error {
		var err error
		inst, err = backup.Restore(ctx, path, dir, e, backup.RestoreOptions{
			Progress: func(stage string) {
				if t := restoreStageTitle(stage); t != "" {
					s.Title(t)
				}
			},
			OnEvent: func(ev engine.Event) {
				if ev.Total > 0 {
					s.Progress(ev.Current, ev.Total)
				}
			},
		})
		if err != nil {
			return err
		}
		s.Done("Restored %s from the backup made on %s.", m.Domain, backupTime(m.CreatedAt))
		return nil
	})
	if err != nil {
		return restoreErr(path, err, restoreFailureLog(ctx, e, dir, err))
	}
	warnBackupSkipped(app, m)

	p, err := deploy.Load(ctx, inst, e)
	if err != nil {
		return engineFailure(err)
	}
	restoreBootUnit(ctx, app, inst, e, p, existing, old)
	app.remember(inst.Dir)
	err = app.UI.Step(ctx, "Starting Fluxer", func(ctx context.Context, s *ui.Step) error {
		if err := p.WaitReady(ctx, restoreReadyTimeout, showReadiness(s, "Starting Fluxer")); err != nil {
			return err
		}
		s.Done("Started Fluxer.")
		return nil
	})
	if err != nil {
		if errors.Is(err, context.Canceled) {
			return err
		}
		return ui.Wrap(err, "Fluxer didn't start after the restore.", "Run rexctl doctor to find the problem.")
	}
	if origin := inst.Origin(); origin != "" {
		app.UI.Highlight(origin)
	}
	return nil
}

func restoreBootUnit(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine, p *engine.Project, existing *instance.Instance, old instance.EngineRef) {
	if !host.BootUnitSupported() {
		return
	}
	u, err := checks.BootUnitFor(inst, e, p)
	if err != nil {
		app.UI.Detail("%v", err)
		return
	}
	if existing != nil && old.Kind != "" && (old.Kind != string(e.Kind) || old.ID != e.ID) {
		prev := u
		prev.Podman = old.Kind == string(engine.Podman)
		prev.Rootless = strings.Contains(old.Host, "/run/user/")
		if prev.Rootless != u.Rootless {
			if err := host.RemoveBootUnit(ctx, prev); err != nil {
				app.UI.Detail("%v", err)
			}
		}
	}
	if err := host.InstallBootUnit(ctx, u); err != nil {
		app.UI.Warn("Fluxer won't start again after this machine restarts.", "Run rexctl doctor --fix to start it with the machine.")
		app.UI.Detail("%v", err)
	}
}

func restoreStageTitle(stage string) string {
	switch stage {
	case backup.StageVerifying:
		return "Checking the backup"
	case backup.StageClearing:
		return "Removing the current instance"
	case backup.StageFiles:
		return "Restoring your settings"
	case backup.StagePulling:
		return "Downloading Fluxer"
	case backup.StageDatabase:
		return "Restoring the database"
	case backup.StageVolumes:
		return "Restoring uploads and search data"
	case backup.StageStarting:
		return "Starting Fluxer"
	}
	return ""
}

func restoreEngine(ctx context.Context, app *App, existing *instance.Instance, dir string, toDocker bool) (*engine.Engine, error) {
	if toDocker {
		e, err := engine.Connect(ctx, engine.ConnectOptions{Prefer: engine.Docker})
		if err == nil && e.Kind == engine.Docker {
			return e, nil
		}
		if err == nil {
			e.Close()
		}
		if engine.ClassOf(err) == engine.ClassDenied {
			return nil, engineFailureFor(engine.Docker, err)
		}
		return installDockerForRestore(ctx, app, dir)
	}
	e, err := app.Engine(ctx, existing)
	switch {
	case err == nil:
		return e, nil
	case errors.Is(err, context.Canceled):
		return nil, err
	case existing == nil && runtime.GOOS == "linux" && engine.ClassOf(err) == engine.ClassUnreachable:
		return installDockerForRestore(ctx, app, dir)
	}
	return nil, err
}

func installDockerForRestore(ctx context.Context, app *App, dir string) (*engine.Engine, error) {
	manual := ui.Refused("Docker isn't installed or isn't running.", engine.Docker.InstallHint()+", then run rexctl restore again.")
	info, err := host.Detect(ctx, dir)
	if err != nil || !host.CanInstallDocker(info) {
		return nil, manual
	}
	app.UI.Warn("Docker isn't installed.", "")
	ok, err := app.UI.Confirm("Install Docker?", true)
	if err != nil {
		return nil, err
	}
	if !ok {
		return nil, manual
	}
	err = app.UI.Step(ctx, "Installing Docker", func(ctx context.Context, s *ui.Step) error {
		if err := host.InstallDocker(ctx, info, s.Log); err != nil {
			return err
		}
		s.Done("Installed Docker.")
		return nil
	})
	if errors.Is(err, host.ErrRelogin) {
		return nil, ui.Refused("Docker is installed, and your account can use it after you sign in again.", "Sign out and back in, then run rexctl restore again.")
	}
	if err != nil {
		return nil, ui.Wrap(err, "Couldn't install Docker.", "Install Docker, then run rexctl restore again.")
	}
	e, err := engine.Connect(ctx, engine.ConnectOptions{Prefer: engine.Docker})
	if err != nil || e.Kind != engine.Docker {
		return nil, ui.Wrap(err, "Docker was installed but isn't answering.", "Start Docker, then run rexctl restore again.")
	}
	return e, nil
}

func restoreFailureLog(ctx context.Context, e *engine.Engine, dir string, err error) string {
	var part *backup.RestoreError
	if !errors.As(err, &part) || errors.Is(err, context.Canceled) {
		return ""
	}
	inst, ferr := instance.Find(dir)
	if ferr != nil {
		return ""
	}
	cause := err
	if part.Output != "" {
		cause = fmt.Errorf("%w\n%s", err, part.Output)
	}
	return writeFailureLog(ctx, inst, e.Named(inst.Project(), inst.Dir), "restore.log", cause, nil)
}

func restoreErr(path string, err error, logPath string) error {
	var inUse *backup.ProjectInUseError
	var part *backup.RestoreError
	var pull *backup.PullError
	partFix := "Run rexctl restore again with --verbose to see why."
	if logPath != "" {
		partFix = fmt.Sprintf("Read %s to see why, then run rexctl restore again.", logPath)
	}
	switch {
	case errors.As(err, &pull) && !errors.Is(err, context.Canceled):
		if engine.ClassOf(pull.Err) != engine.ClassOther {
			return engineFailure(pull.Err)
		}
		return ui.Wrap(pull.Err, "Couldn't download Fluxer.", "Run rexctl restore again with --verbose to see why.")
	case errors.As(err, &inUse):
		return ui.Refused(
			fmt.Sprintf("Another Fluxer instance on this machine is in %s.", inUse.Dir),
			"Restore into that folder with --dir, or uninstall it first.",
		)
	case errors.As(err, &part) && part.Part == "database":
		return ui.Wrap(fmt.Errorf("%w %s", err, part.Output), "Couldn't restore the database.", partFix)
	case errors.As(err, &part) && part.Part == "containers":
		return ui.Wrap(fmt.Errorf("%w %s", err, part.Output), "Couldn't set up Fluxer from the backup.", partFix)
	case errors.As(err, &part):
		return ui.Wrap(fmt.Errorf("%w %s", err, part.Output), "Couldn't restore uploads and search data.", partFix)
	case errors.Is(err, backup.ErrDamaged), errors.Is(err, backup.ErrNotBackup), errors.Is(err, backup.ErrNewer):
		return backupFileErr(path, err)
	}
	var ee *ui.ExitError
	if errors.As(err, &ee) || errors.Is(err, context.Canceled) {
		return err
	}
	if engine.ClassOf(err) != engine.ClassOther {
		return engineFailure(err)
	}
	return ui.Wrap(err, "Couldn't restore your instance.", "Run rexctl restore again with --verbose to see why.")
}
