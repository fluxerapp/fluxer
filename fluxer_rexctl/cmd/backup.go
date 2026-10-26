// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func newBackupCmd(app *App) *cobra.Command {
	c := &cobra.Command{
		Use:   "backup",
		Short: "Back up your instance",
		Long:  "Back up your instance. Fluxer pauses while uploads are copied.",
		Args:  noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runBackup(c.Context(), app)
		},
	}
	list := &cobra.Command{
		Use:   "list",
		Short: "List your backups",
		Args:  noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runBackupList(app)
		},
	}
	app.jsonFlag(list)
	verify := &cobra.Command{
		Use:   "verify <file>",
		Short: "Check that a backup is complete and readable",
		Args:  exactArgs("the backup file"),
		RunE: func(c *cobra.Command, args []string) error {
			return runBackupVerify(c.Context(), app, args[0])
		},
	}
	location := &cobra.Command{
		Use:   "location [folder]",
		Short: "Show or change the folder rexctl saves backups in",
		Long:  "Show the folder rexctl saves backups in, or pass a full path to save new backups there. Backups that are already saved stay where they are.",
		Args:  maxArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			if len(args) == 0 {
				return runBackupLocationShow(app)
			}
			return runBackupLocation(c.Context(), app, args[0])
		},
	}
	keep := &cobra.Command{
		Use:   "keep [count|all]",
		Short: "Show or change how many backups rexctl keeps",
		Long:  "Show or change how many backups rexctl keeps. After each backup, rexctl deletes the oldest ones beyond this number, except the backup that rexctl rollback needs. Pass all to keep every backup.",
		Args:  maxArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			if len(args) == 0 {
				return runBackupKeepShow(app)
			}
			return runBackupKeep(c.Context(), app, args[0])
		},
	}
	app.jsonFlag(location)
	app.jsonFlag(keep)
	c.AddCommand(list, verify, location, keep)
	return c
}

func runBackup(ctx context.Context, app *App) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	p, err := app.Named(ctx, inst)
	if err != nil {
		return err
	}
	return app.Locked(ctx, inst, func() error {
		running, down := runningParts(ctx, p)
		if running {
			app.UI.Info("Fluxer stops for about a minute while rexctl copies your uploads.")
		}
		if !running || inst.State.Stopped {
			down = nil
		}
		var info *backup.Info
		paused := false
		err := app.UI.Step(ctx, "Backing up your instance", func(ctx context.Context, s *ui.Step) error {
			var err error
			info, err = backup.Create(ctx, inst, p, backup.CreateOptions{
				Reason:  "manual",
				OnEvent: app.progress(s, "Starting"),
				Progress: func(stage string) {
					if stage == backup.StageStarting {
						paused = true
					}
					if t := backupStageTitle(stage); t != "" {
						s.Title(t)
					}
				},
			})
			if err != nil {
				return err
			}
			s.Done("Backed up your instance (%s).", ui.Bytes(info.SizeBytes))
			return nil
		})
		if err != nil {
			return backupCreateErr(err)
		}
		if paused {
			err := app.UI.Step(ctx, "Waiting for Fluxer to start again", func(ctx context.Context, s *ui.Step) error {
				if err := p.WaitReady(ctx, engine.DefaultReadyTimeout, showReadiness(s, "Starting Fluxer again"), down...); err != nil {
					return err
				}
				s.Done("Fluxer is running again.")
				return nil
			})
			if errors.Is(err, context.Canceled) {
				return err
			}
			if err != nil {
				fix := "Run rexctl doctor to find out why."
				var nr *engine.NotReadyError
				if errors.As(err, &nr) {
					if path := keepStartLog(ctx, inst, p, err, nr); path != "" {
						fix = "Read " + path + " to see why."
					}
				}
				return ui.Wrap(err, fmt.Sprintf("Saved the backup to %s, but Fluxer didn't start again.", info.Path), fix)
			}
		}
		if m, err := backup.ReadManifest(info.Path); err == nil {
			warnBackupSkipped(app, m)
		}
		if len(down) > 0 {
			app.UI.Warn(fmt.Sprintf("These services weren't running before the backup and are still stopped: %s.", strings.Join(down, ", ")), "Run rexctl start to start them.")
		}
		pruneBackups(app, inst)
		app.UI.Result(info.Path)
		return nil
	})
}

func backupStageTitle(stage string) string {
	switch stage {
	case backup.StageFiles:
		return "Copying your settings"
	case backup.StageDatabase:
		return "Saving the database"
	case backup.StageStopping:
		return "Pausing Fluxer to copy uploads"
	case backup.StageVolumes:
		return "Copying uploads and search data while Fluxer is paused"
	case backup.StageStarting:
		return "Starting Fluxer again"
	case backup.StageCompressing:
		return "Compressing the backup while Fluxer runs"
	}
	return ""
}

func warnBackupSkipped(app *App, m *backup.Manifest) {
	if m.HasSkipped(backup.SkippedDatabase) {
		app.UI.Warn("This backup leaves out your database, which runs outside Fluxer.", "")
	}
	if m.HasSkipped(backup.SkippedUploads) {
		app.UI.Warn("This backup leaves out uploads, which are stored outside Fluxer.", "")
	}
}

func backupCreateErr(err error) error {
	var ee *ui.ExitError
	var disk *backup.DiskError
	var missing *backup.MissingDataError
	var dump *backup.DumpError
	var stopped *backup.InterruptedError
	var compress *backup.CompressError
	switch {
	case errors.As(err, &stopped) && stopped.Restarted:
		return &ui.ExitError{Code: ui.ExitInterrupted, Msg: "Cancelled the backup. Fluxer is running again.", Err: err}
	case errors.As(err, &compress):
		return ui.Wrap(err, "Couldn't finish the backup while compressing it. Fluxer is running again.", "Free up some space or check the backup folder, then run rexctl backup again.")
	case errors.As(err, &ee), errors.Is(err, context.Canceled):
		return err
	case errors.Is(err, backup.ErrNeverStarted):
		return ui.Refused("Fluxer hasn't started on this machine yet, and there's nothing to back up.", "Run rexctl start, then run rexctl backup.")
	case errors.As(err, &missing):
		return ui.Failed(
			fmt.Sprintf("rexctl can't find your database data. It looked for the Docker volume %s and for a bind mount on the postgres service.", missing.Volume),
			"Run docker volume ls to see your volumes, then run rexctl backup again.",
		)
	case errors.As(err, &disk):
		return ui.Failed(
			fmt.Sprintf("There isn't enough free space for a backup. It needs about %s and %s is free.", ui.Bytes(int64(disk.Need)), ui.Bytes(int64(disk.Available))),
			"Free up some space, then run rexctl backup again.",
		)
	case errors.As(err, &dump):
		return ui.Wrap(fmt.Errorf("%w %s", err, dump.Output), "Couldn't save the database.", "Run rexctl doctor to find the problem.")
	case engine.ClassOf(err) != engine.ClassOther:
		return engineFailure(err)
	}
	return ui.Wrap(err, "Couldn't back up your instance.", "Run rexctl backup again with --verbose to see why.")
}

func runningParts(ctx context.Context, p *engine.Project) (bool, []string) {
	cs, err := p.PS(ctx)
	if err != nil {
		return true, nil
	}
	running := false
	var stopped []string
	for _, c := range cs {
		switch {
		case c.State == "running":
			running = true
		case !stack.OneShot[c.Service] && !slices.Contains(stopped, c.Service):
			stopped = append(stopped, c.Service)
		}
	}
	return running, stopped
}

func runBackupList(app *App) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	list, err := backup.List(inst.BackupDir())
	if err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't read %s.", inst.BackupDir()), "Check that you can open that folder, then try again.")
	}
	if app.Flags.JSON {
		return app.UI.JSON(list)
	}
	if len(list) == 0 {
		app.UI.Info("You don't have any backups yet. Run rexctl backup to make one.")
		return nil
	}
	rows := make([][]string, 0, len(list))
	for _, b := range list {
		rows = append(rows, []string{b.File, backupTime(b.CreatedAt), b.Release, ui.Bytes(b.SizeBytes)})
	}
	app.UI.Table([]string{"File", "Made", "Version", "Size"}, rows)
	return nil
}

func runBackupVerify(ctx context.Context, app *App, arg string) error {
	path := resolveBackupPath(app, arg)
	return app.UI.Step(ctx, "Checking the backup", func(ctx context.Context, s *ui.Step) error {
		m, err := backup.Verify(path)
		if err != nil {
			return backupFileErr(path, err)
		}
		s.Done("The backup is complete. It has %s on %s, made %s.", m.Domain, fluxerName(m.Release), backupTime(m.CreatedAt))
		return nil
	})
}

func resolveBackupPath(app *App, arg string) string {
	if _, err := os.Stat(arg); err == nil || strings.ContainsAny(arg, `/\`) {
		if abs, err := filepath.Abs(arg); err == nil {
			return abs
		}
		return arg
	}
	if inst, err := app.Instance(); err == nil {
		p := filepath.Join(inst.BackupDir(), arg)
		if _, err := os.Stat(p); err == nil {
			return p
		}
	}
	if abs, err := filepath.Abs(arg); err == nil {
		return abs
	}
	return arg
}

func backupFileErr(path string, err error) error {
	var ee *ui.ExitError
	switch {
	case errors.As(err, &ee), errors.Is(err, context.Canceled):
		return err
	case errors.Is(err, os.ErrNotExist):
		return ui.Failed(fmt.Sprintf("There's no file at %s.", path), "Run rexctl backup list to see your backups.")
	case errors.Is(err, backup.ErrNewer):
		return ui.Refused("This backup was made by a newer version of rexctl.", "Run rexctl upgrade, then try again.")
	case errors.Is(err, backup.ErrNotBackup):
		return ui.Failed(fmt.Sprintf("%s isn't a Fluxer backup, or it was cut short.", filepath.Base(path)), "Check that you picked the right file and that it copied in full.")
	case errors.Is(err, backup.ErrDamaged):
		return ui.Wrap(err, fmt.Sprintf("%s is damaged.", filepath.Base(path)), "Use another backup, or make a new one with rexctl backup.")
	}
	return ui.Wrap(err, fmt.Sprintf("Couldn't read %s.", path), "Check that you can open the file, then try again.")
}

func backupTime(t time.Time) string {
	return t.UTC().Format("2 January 2006 at 15:04 UTC")
}

func pruneBackups(app *App, inst *instance.Instance) {
	keep := inst.KeepBackups()
	if keep == instance.KeepAllBackups || inst.State == nil {
		return
	}
	dir := inst.BackupDir()
	var protect []string
	if u := inst.State.Upgrade; u != nil {
		protect = append(protect, u.Backup)
	}
	if list, err := backup.List(dir); err == nil {
		if b := backup.NewestOf(list, inst.State.PreviousRelease); b != nil {
			protect = append(protect, b.Path)
		}
	}
	removed, err := backup.Prune(dir, keep, protect...)
	if len(removed) > 0 {
		app.UI.Info("Deleted %s to keep the newest %d. Run rexctl backup keep to change how many rexctl keeps.", plural(len(removed), "older backup", "older backups"), keep)
	}
	if err != nil {
		app.UI.Warn(fmt.Sprintf("Couldn't delete older backups in %s.", dir), "Check that you can change files in that folder.")
	}
}

func runBackupLocationShow(app *App) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	if app.Flags.JSON {
		return app.UI.JSON(map[string]string{"location": inst.BackupDir()})
	}
	app.UI.Result(inst.BackupDir())
	return nil
}

func runBackupLocation(ctx context.Context, app *App, arg string) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	if !filepath.IsAbs(arg) {
		return ui.Usage("Pass the full path of the folder, for example /mnt/backups/fluxer.")
	}
	dir := filepath.Clean(arg)
	if why := backupLocationProblem(ctx, app, inst, dir); why != "" {
		return ui.Refused(why, "Pick a folder outside your instance's data, then try again.")
	}
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't make %s.", dir), "Check that you can make folders there, then try again.")
	}
	if err := os.Chmod(dir, 0o700); err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't change who can open %s.", dir), "Check that you own that folder, then try again.")
	}
	probe, err := os.CreateTemp(dir, ".rexctl-write-")
	if err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't save files in %s.", dir), "Check that you can change files in that folder, then try again.")
	}
	probe.Close()
	os.Remove(probe.Name())
	old := inst.BackupDir()
	return app.Locked(ctx, inst, func() error {
		inst.State.BackupDir = dir
		if dir == filepath.Clean(inst.Path(instance.BackupsDir)) {
			inst.State.BackupDir = ""
		}
		if err := inst.Save(); err != nil {
			return ui.Wrap(err, "Couldn't save your instance's state.", "Check that you can write to "+inst.Dir+", then try again.")
		}
		app.UI.OK("New backups go to %s.", dir)
		if old != dir {
			if list, err := backup.List(old); err == nil && len(list) == 1 {
				app.UI.Info("Your backup in %s stays there. Move it to %s if you want rexctl to list it and use it for rollback.", old, dir)
			} else if err == nil && len(list) > 1 {
				app.UI.Info("Your %d backups in %s stay there. Move them to %s if you want rexctl to list them and use them for rollback.", len(list), old, dir)
			}
		}
		return nil
	})
}

func backupLocationProblem(ctx context.Context, app *App, inst *instance.Instance, dir string) string {
	if filepath.Clean(inst.Dir) == dir {
		return fmt.Sprintf("%s is your instance folder, and every backup copies the files in it.", dir)
	}
	for _, sub := range []string{instance.StateDirName, instance.CertsDir, instance.CaddyDropInDir} {
		if within(inst.Path(sub), dir) {
			return fmt.Sprintf("%s is inside %s, which every backup copies.", dir, inst.Path(sub))
		}
	}
	p, err := app.Named(ctx, inst)
	if err != nil {
		return ""
	}
	vols, err := p.Engine.Volumes(ctx, p.Name)
	if err != nil {
		return ""
	}
	for _, v := range vols {
		for _, root := range []string{v.Mountpoint, v.Device} {
			if root != "" && within(root, dir) {
				return fmt.Sprintf("%s is inside your instance's %s volume.", dir, v.Name)
			}
		}
	}
	return ""
}

func within(root, dir string) bool {
	rel, err := filepath.Rel(filepath.Clean(root), dir)
	return err == nil && filepath.IsLocal(rel)
}

func runBackupKeepShow(app *App) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	keep := inst.KeepBackups()
	if app.Flags.JSON {
		return app.UI.JSON(map[string]int{"keep": keep})
	}
	if keep == instance.KeepAllBackups {
		app.UI.Result("all")
		return nil
	}
	app.UI.Result(strconv.Itoa(keep))
	return nil
}

func runBackupKeep(ctx context.Context, app *App, arg string) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	keep := instance.KeepAllBackups
	if !strings.EqualFold(arg, "all") {
		n, err := strconv.Atoi(arg)
		if err != nil || n < 1 || n > 1000 {
			return ui.Usage("Pass a number from 1 to 1000, or all.")
		}
		keep = n
	}
	return app.Locked(ctx, inst, func() error {
		inst.State.KeepBackups = keep
		if keep == instance.DefaultKeepBackups {
			inst.State.KeepBackups = 0
		}
		if err := inst.Save(); err != nil {
			return ui.Wrap(err, "Couldn't save your instance's state.", "Check that you can write to "+inst.Dir+", then try again.")
		}
		if keep == instance.KeepAllBackups {
			app.UI.OK("rexctl keeps every backup.")
			return nil
		}
		app.UI.OK("rexctl keeps the newest %d backups.", keep)
		pruneBackups(app, inst)
		return nil
	})
}
