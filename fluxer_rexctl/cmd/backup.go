// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func newBackupCmd(app *App) *cobra.Command {
	var scheduled bool
	var stamp string
	c := &cobra.Command{
		Use:   "backup",
		Short: "Back up your instance",
		Long:  "Back up your instance in one file that restores on this machine or a new one, with the database, search data and uploads that Fluxer stores itself. Fluxer pauses during the copy.",
		Args:  noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			if scheduled {
				return runScheduledBackup(c.Context(), app, stamp)
			}
			return runBackup(c.Context(), app, "manual")
		},
	}
	list := &cobra.Command{
		Use:   "list",
		Short: "List your backups",
		Args:  noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runBackupList(c.Context(), app)
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
		Long:  "Show or change how many backups rexctl keeps. After each backup, rexctl deletes the oldest ones beyond this number, except the backup that rexctl rollback needs and the newest backup with uploads. Pass all to keep every backup.",
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
	c.AddCommand(list, verify, location, keep, newBackupScheduleCmd(app), newBackupCopyCmd(app))
	c.Flags().BoolVar(&scheduled, "scheduled", false, "mark the backup as one the schedule made")
	_ = c.Flags().MarkHidden("scheduled")
	c.Flags().StringVar(&stamp, "for", "", "the instance a scheduled backup belongs to")
	_ = c.Flags().MarkHidden("for")
	return c
}

func runBackup(ctx context.Context, app *App, reason string) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	if inst.State.IsVoice() {
		return voiceMachineBackup(app, inst)
	}
	return app.Locked(ctx, inst, func() error {
		r, err := app.Runtime(ctx, inst)
		if err != nil {
			return err
		}
		path, err := backupWith(ctx, app, inst, r, rt.BackupOptions{Reason: reason}, nil, "Backing up your instance")
		if err != nil {
			return err
		}
		pruneBackups(app, inst)
		app.UI.Result(path)
		return nil
	})
}

func backupWith(ctx context.Context, app *App, inst *instance.Instance, r rt.Runtime, o rt.BackupOptions, plan *rt.BackupPlan, title string) (string, error) {
	if plan == nil {
		if p, err := r.Backups().Plan(ctx, o); err == nil {
			plan = &p
		}
	}
	if plan != nil && len(plan.PauseParts) > 0 {
		what := "Fluxer's " + ui.JoinAnd(plan.PauseParts) + " parts pause while rexctl copies uploads and search data, then start again by themselves."
		switch {
		case len(plan.PauseParts) == 1:
			what = "Fluxer's " + plan.PauseParts[0] + " part pauses while rexctl copies uploads and search data, then starts again by itself."
		case len(plan.PauseParts) > 4:
			what = "Fluxer pauses while rexctl copies uploads and search data, then starts again by itself."
		}
		app.UI.Card(ui.Card{
			Title: "Fluxer pauses for about " + aboutDuration(plan.PauseFor),
			Tone:  ui.ToneWarn,
			Body:  []string{what + " The rest of the backup runs while Fluxer is up."},
		})
	}
	var path string
	err := app.UI.Step(ctx, title, func(ctx context.Context, s *ui.Step) error {
		var err error
		if path, err = r.Backups().Create(ctx, s, o); err != nil {
			return err
		}
		size := int64(0)
		if info, err := os.Stat(path); err == nil {
			size = info.Size()
		}
		s.Done("Backed up your instance (%s).", ui.Bytes(size))
		return nil
	})
	var again *rt.StartAgainError
	switch {
	case errors.As(err, &again) && errors.Is(err, context.Canceled):
		return "", &ui.ExitError{Code: ui.ExitInterrupted, Msg: fmt.Sprintf("Saved the backup to %s, then stopped waiting for Fluxer to start again.", again.Path), Fix: "Run rexctl status to check that Fluxer is running.", Err: err}
	case errors.As(err, &again):
		return "", ui.Wrap(err, fmt.Sprintf("Saved the backup to %s, but Fluxer didn't start again.", again.Path), startAgainFix(ctx, app, inst, again.Err))
	case err != nil:
		return "", backupCreateErr(inst, err)
	}
	if m, err := backup.ReadManifest(path); err == nil {
		warnBackupSkipped(app, m)
	}
	return path, nil
}

func startAgainFix(ctx context.Context, app *App, inst *instance.Instance, err error) string {
	var nr *engine.NotReadyError
	if !errors.As(err, &nr) {
		return "Run rexctl doctor to find out why."
	}
	if p, perr := app.Named(ctx, inst); perr == nil {
		if path := keepStartLog(ctx, inst, p, err, nr); path != "" {
			return "Read " + path + " to see why."
		}
	}
	return "Run rexctl logs to see why."
}

func aboutDuration(d time.Duration) string {
	minutes := int((d + 30*time.Second) / time.Minute)
	switch {
	case minutes <= 1:
		return "a minute"
	case minutes < 60:
		return fmt.Sprintf("%d minutes", minutes)
	case minutes < 90:
		return "an hour"
	}
	return fmt.Sprintf("%d hours", (minutes+30)/60)
}

func warnBackupSkipped(app *App, m *backup.Manifest) {
	if m.HasSkipped(backup.SkippedDatabase) {
		app.UI.Warn("This backup leaves out your database, which runs outside Fluxer.", "")
	}
	if m.HasSkipped(backup.SkippedUploads) {
		app.UI.Warn("This backup leaves out uploads, which are stored outside Fluxer.", "")
	}
}

func backupCreateErr(inst *instance.Instance, err error) error {
	var ee *ui.ExitError
	var disk *backup.DiskError
	var missing *backup.MissingDataError
	var dump *backup.DumpError
	var stopped *backup.InterruptedError
	var compress *backup.CompressError
	var unknown *backup.SpaceUnknownError
	switch {
	case errors.As(err, &stopped) && stopped.Restarted:
		return &ui.ExitError{Code: ui.ExitInterrupted, Msg: "Cancelled the backup. Fluxer is running again.", Err: err}
	case errors.As(err, &compress):
		return ui.Wrap(err, "Couldn't finish the backup while compressing it. Fluxer is running again.", "Free up some space or check the backup folder, then run rexctl backup again.")
	case errors.As(err, &ee), errors.Is(err, context.Canceled):
		return err
	case errors.Is(err, backup.ErrNeverStarted):
		return &ui.ExitError{Code: ui.ExitRefused, Msg: "Fluxer hasn't started on this machine yet, and there's nothing to back up.", Fix: "Run rexctl start, then run rexctl backup.", Err: err}
	case errors.As(err, &missing):
		name := engine.Kind(inst.State.Engine.Kind).Name()
		return ui.Failed(
			fmt.Sprintf("rexctl can't find your database data. It looked for the %s volume %s and for a bind mount on the postgres service.", name, missing.Volume),
			fmt.Sprintf("Run %s volume ls to see your volumes, then run rexctl backup again.", strings.ToLower(name)),
		)
	case errors.As(err, &unknown):
		return ui.Wrap(err, fmt.Sprintf("rexctl can't read how much free disk space %s has.", unknown.Dir), "Check that the folder is on a mounted disk you can open, then run rexctl backup again.")
	case errors.As(err, &disk):
		return ui.Failed(
			fmt.Sprintf("There isn't enough free space for a backup. It needs about %s and %s is free.", ui.Bytes(int64(disk.Need)), ui.Bytes(int64(disk.Available))),
			"Free up some space, then run rexctl backup again."+keepFewerFix(inst, disk.Need-min(disk.Available, disk.Need)),
		)
	case errors.As(err, &dump):
		return ui.Wrap(fmt.Errorf("%w %s", err, dump.Output), "Couldn't save the database.", "Run rexctl doctor to find the problem.")
	case engine.ClassOf(err) != engine.ClassOther:
		return rt.EngineError("", err)
	}
	return ui.Wrap(err, "Couldn't back up your instance.", "Run rexctl backup again with --verbose to see why.")
}

func fetchOwnBackups(ctx context.Context, app *App, inst *instance.Instance) {
	if k := rt.KindOf(inst); k != rt.Container && k != rt.Native || inst.State.Ejected != nil {
		return
	}
	r, err := app.Runtime(ctx, inst)
	if err != nil {
		app.UI.Detail("%v", err)
		return
	}
	got, err := rt.FetchOwnBackups(ctx, r, inst.BackupDir())
	if err != nil {
		app.UI.Warn("Couldn't copy Fluxer's own backups into "+inst.BackupDir()+".", "Run the command again with --verbose to see why.")
		app.UI.Detail("%v", err)
		return
	}
	if len(got) > 0 && !app.Flags.JSON {
		app.UI.Info("Copied %d of Fluxer's own backups to %s.", len(got), inst.BackupDir())
	}
}

func runBackupList(ctx context.Context, app *App) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	fetchOwnBackups(ctx, app, inst)
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
		rows = append(rows, []string{b.File, backupTime(b.CreatedAt), b.Release, backupReason(b), ui.Bytes(b.SizeBytes)})
	}
	app.UI.Table([]string{"File", "Made", "Version", "Reason", "Size"}, rows)
	return nil
}

func backupReason(b backup.Info) string {
	if !b.WithoutUploads() {
		return b.Reason
	}
	if b.Reason == "" {
		return "without uploads"
	}
	return b.Reason + ", without uploads"
}

func runBackupVerify(ctx context.Context, app *App, arg string) error {
	path := resolveBackupPath(app, arg)
	return app.UI.Check(ctx, "Checking the backup", func(ctx context.Context, s *ui.Step) error {
		m, err := backup.Verify(path)
		if err != nil {
			return backupFileErr(path, err)
		}
		if m.WithoutUploads() {
			s.Done("The backup is complete. It has %s on %s without uploads, made %s.", ui.Inline(m.Domain), fluxerName(ui.Inline(m.Release)), backupTime(m.CreatedAt))
			return nil
		}
		s.Done("The backup is complete. It has %s on %s, made %s.", ui.Inline(m.Domain), fluxerName(ui.Inline(m.Release)), backupTime(m.CreatedAt))
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
		return ui.Refused(fmt.Sprintf("There's no file at %s.", path), "Run rexctl backup list to see your backups.")
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
	return t.Local().Format("2 January 2006 at 15:04 MST")
}

func pruneBackups(app *App, inst *instance.Instance) {
	keep := inst.KeepBackups()
	if keep == instance.KeepAllBackups || inst.State == nil || inst.State.Restoring != "" {
		return
	}
	dir := inst.BackupDir()
	removed, err := backup.Prune(dir, keep, protectedBackups(inst, dir)...)
	if len(removed) > 0 {
		msg := fmt.Sprintf("Deleted %s to keep the newest %d.", plural(len(removed), "older backup", "older backups"), keep)
		if left, lerr := backup.List(dir); lerr == nil && len(left) > keep {
			msg += " rexctl also keeps the newest backup with uploads, the one rexctl rollback needs, the one from before rexctl migrate changed anything and the ones from the last restore."
		}
		app.UI.Info("%s Run rexctl backup keep to change how many rexctl keeps.", msg)
	}
	if err != nil {
		app.UI.Warn(fmt.Sprintf("Couldn't delete older backups in %s.", dir), "Check that you can change files in that folder.")
	}
}

func keepFewerFix(inst *instance.Instance, short uint64) string {
	if inst == nil || inst.State == nil || short == 0 {
		return ""
	}
	dir := inst.BackupDir()
	list, err := backup.List(dir)
	if err != nil {
		return ""
	}
	protected := map[string]bool{}
	for _, p := range protectedBackups(inst, dir) {
		if b := backup.Find(list, p); b != nil {
			protected[b.Path] = true
		}
	}
	keep := inst.KeepBackups()
	for n := len(list) - 1; n >= 1; n-- {
		if keep != instance.KeepAllBackups && n >= keep {
			continue
		}
		var freed uint64
		var removed int
		for _, b := range list[n:] {
			if !protected[b.Path] {
				freed += uint64(max(b.SizeBytes, 0))
				removed++
			}
		}
		if removed > 0 && freed >= short {
			return fmt.Sprintf(" Or run rexctl backup keep %d, which deletes %s now to free about %s. From then on, rexctl keeps the newest %d.", n, plural(removed, "older backup", "older backups"), ui.Bytes(int64(freed)), n)
		}
	}
	return ""
}

func protectedBackups(inst *instance.Instance, dir string) []string {
	var protect []string
	if u := inst.State.Upgrade; u != nil {
		protect = append(protect, u.Backup)
	}
	if inst.State.MigrateBackup != "" && migrateUndoable(inst) {
		protect = append(protect, inst.State.MigrateBackup)
	}
	if m := inst.State.MovedFrom; m != nil {
		protect = append(protect, m.Backup)
	}
	if r := inst.State.Restored; r != nil {
		protect = append(protect, r.From, r.Safety)
	}
	list, err := backup.List(dir)
	if err != nil {
		return protect
	}
	if b := rollbackTarget(inst, list, inst.State.PreviousRelease); b != nil {
		protect = append(protect, b.Path)
	}
	if b := backup.NewestWithUploads(list); b != nil {
		protect = append(protect, b.Path)
	}
	return protect
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
	return app.Locked(ctx, inst, func() error {
		r, err := app.Runtime(ctx, inst)
		if err != nil {
			return err
		}
		if err := backupLocationProblem(ctx, app, inst, r, dir); err != nil {
			return err
		}
		if err := makeBackupDir(dir); err != nil {
			return err
		}
		old := inst.BackupDir()
		inst.State.BackupDir = dir
		if dir == filepath.Clean(inst.Path(instance.BackupsDir)) {
			inst.State.BackupDir = ""
		}
		if err := saveInstance(inst); err != nil {
			return err
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

func makeBackupDir(dir string) error {
	info, err := os.Stat(dir)
	switch {
	case err == nil && !info.IsDir():
		return ui.Refused(fmt.Sprintf("%s is a file, not a folder.", dir), "Pick a folder, then try again.")
	case errors.Is(err, os.ErrNotExist):
		if err := os.MkdirAll(dir, 0o700); err != nil {
			return ui.Wrap(err, fmt.Sprintf("Couldn't make %s.", dir), "Check that you can make folders there, then try again.")
		}
		if err := os.Chmod(dir, 0o700); err != nil {
			return ui.Wrap(err, fmt.Sprintf("Couldn't change who can open %s.", dir), "Check that you own that folder, then try again.")
		}
	case err != nil:
		return ui.Wrap(err, fmt.Sprintf("Couldn't open %s.", dir), "Check that you can open that folder, then try again.")
	}
	probe, err := os.CreateTemp(dir, ".rexctl-write-")
	if err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't save files in %s.", dir), "Check that you can change files in that folder, then try again.")
	}
	probe.Close()
	os.Remove(probe.Name())
	return nil
}

func backupLocationProblem(ctx context.Context, app *App, inst *instance.Instance, r rt.Runtime, dir string) error {
	refuse := func(why string) error {
		return ui.Refused(why, "Pick a folder outside your instance's data, then try again.")
	}
	real := instance.RealPath(dir)
	if instance.RealPath(inst.Dir) == real {
		return refuse(fmt.Sprintf("%s is your instance folder, and every backup copies the files in it.", dir))
	}
	for _, sub := range []string{instance.StateDirName, instance.CertsDir, instance.GeoIPDir, instance.CADir, instance.CaddyDropInDir} {
		if within(instance.RealPath(inst.Path(sub)), real) {
			return refuse(fmt.Sprintf("%s is inside %s, which every backup copies.", dir, inst.Path(sub)))
		}
	}
	if r.Kind() == rt.Container {
		return nil
	}
	if r.Kind() == rt.Native {
		data := r.Info(ctx).DataDir
		if within(instance.RealPath(data), real) {
			return refuse(fmt.Sprintf("%s is inside %s, where Fluxer keeps its data.", dir, data))
		}
		if p := instance.RootPath(dir, false); p != "" {
			return ui.Refused(fmt.Sprintf("Backups hold every secret of your instance, and accounts other than root can change %s.", p), "Pick a folder that root owns and only root can change.")
		}
		return nil
	}
	p, err := app.Named(ctx, inst)
	if err != nil {
		return nil
	}
	vols, err := p.Engine.Volumes(ctx, p.Name)
	if err != nil {
		return nil
	}
	for _, v := range vols {
		for _, root := range []string{v.Mountpoint, v.Device} {
			if root != "" && within(instance.RealPath(root), real) {
				return refuse(fmt.Sprintf("%s is inside your instance's %s volume.", dir, v.Name))
			}
		}
	}
	return nil
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
		if err := saveInstance(inst); err != nil {
			return err
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
