// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"os"
	"os/user"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func newBackupScheduleCmd(app *App) *cobra.Command {
	c := &cobra.Command{
		Use:   "schedule [daily|weekly|off]",
		Short: "Show or change when rexctl backs up your instance by itself",
		Long:  "Show or change when rexctl backs up your instance by itself. With daily it backs up every night at about 03:00, with weekly every Sunday at about 03:00, and off stops it. Each run works like rexctl backup. rexctl keeps as many as rexctl backup keep says.",
		Example: "rexctl backup schedule\n" +
			"rexctl backup schedule daily\n" +
			"rexctl backup schedule off",
		Args: maxArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			if len(args) == 0 {
				return runBackupScheduleShow(c.Context(), app)
			}
			return runBackupSchedule(c.Context(), app, args[0])
		},
	}
	app.jsonFlag(c)
	return c
}

func scheduleIn(dir, project string, rootless, podman bool) (host.ScheduledBackup, error) {
	bin, err := host.RunningBinary()
	if err != nil {
		return host.ScheduledBackup{}, err
	}
	abs, err := filepath.Abs(dir)
	if err != nil {
		return host.ScheduledBackup{}, err
	}
	return host.ScheduledBackup{
		Scheduler: host.UpgradeScheduler(),
		Project:   project,
		Dir:       abs,
		Binary:    bin,
		For:       project,
		Podman:    podman,
		Rootless:  rootless,
	}, nil
}

func backupScheduleFor(r rt.Runtime, inst *instance.Instance) (host.ScheduledBackup, error) {
	rootless, podman := false, false
	if r.Kind() != rt.Native {
		eng := inst.State.Engine
		podman = eng.Kind == string(engine.Podman)
		rootless = runtime.GOOS == "linux" && os.Geteuid() != 0
		if eng.Host != "" {
			rootless = strings.Contains(eng.Host, "/run/user/")
		}
	}
	return scheduleIn(inst.Dir, inst.Project(), rootless, podman)
}

func hostSchedule(r rt.Runtime, inst *instance.Instance) string {
	s, err := backupScheduleFor(r, inst)
	if err != nil {
		return host.ScheduleOff
	}
	return s.Current()
}

func backupScheduleOf(r rt.Runtime, inst *instance.Instance) string {
	every := hostSchedule(r, inst)
	if r.Kind() == rt.Compose {
		return every
	}
	inside := fluxerdSchedule(inst)
	switch {
	case inside == host.ScheduleOff:
		return every
	case every == host.ScheduleOff:
		return inside + inFluxerd
	}
	return every + ", and " + inside + inFluxerd
}

func twoSchedules(r rt.Runtime, inst *instance.Instance, changes map[string]*string) error {
	v, ok := changes[fluxerdScheduleVar]
	if !ok || v == nil || r.Kind() == rt.Compose {
		return nil
	}
	switch strings.ToLower(strings.TrimSpace(*v)) {
	case host.ScheduleDaily, host.ScheduleWeekly:
	default:
		return nil
	}
	every := hostSchedule(r, inst)
	if every == host.ScheduleOff {
		return nil
	}
	return ui.Refused("rexctl already backs up this instance "+every+", so Fluxer's own schedule would back it up twice.", "Run rexctl backup schedule off first, or leave "+fluxerdScheduleVar+" off.")
}

const (
	fluxerdScheduleVar = "FLUXER_BACKUP_SCHEDULE"
	fluxerdKeepVar     = "FLUXER_BACKUP_KEEP"
	fluxerdHourVar     = "FLUXER_BACKUP_HOUR"
	inFluxerd          = ", inside Fluxer"
)

func fluxerdSchedule(inst *instance.Instance) string {
	switch v := strings.ToLower(strings.TrimSpace(inst.State.Settings[fluxerdScheduleVar])); v {
	case host.ScheduleDaily, host.ScheduleWeekly:
		return v
	}
	return host.ScheduleOff
}

func setFluxerdSchedule(ctx context.Context, app *App, inst *instance.Instance, r rt.Runtime, every string) error {
	changes := map[string]*string{fluxerdScheduleVar: &every}
	if keep := inst.KeepBackups(); keep != instance.KeepAllBackups && every != host.ScheduleOff {
		n := fmt.Sprint(keep)
		changes[fluxerdKeepVar] = &n
	}
	if every == host.ScheduleOff && inst.State.Settings[fluxerdScheduleVar] == "" {
		return nil
	}
	_, err := applyDeployment(ctx, app, inst, r, changes, "Changing when Fluxer backs itself up")
	return err
}

func runBackupScheduleShow(ctx context.Context, app *App) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	r, err := app.Runtime(ctx, inst)
	if err != nil {
		return err
	}
	every := backupScheduleOf(r, inst)
	if app.Flags.JSON {
		return app.UI.JSON(map[string]string{"schedule": every})
	}
	app.UI.Result(every)
	return nil
}

func noBackupScheduler() error {
	return ui.Refused("rexctl can back up Fluxer by itself only on Linux with systemd or cron.", "Run rexctl backup from your own scheduler.")
}

func runBackupSchedule(ctx context.Context, app *App, arg string) error {
	every := strings.ToLower(strings.TrimSpace(arg))
	if every != host.ScheduleDaily && every != host.ScheduleWeekly && every != host.ScheduleOff {
		return ui.Usage("Pass daily, weekly or off.")
	}
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	if runtime.GOOS != "linux" && rt.KindOf(inst) != rt.Container {
		return noBackupScheduler()
	}
	return app.Locked(ctx, inst, func() error {
		inside, err := setBackupSchedule(ctx, app, inst, every)
		if err != nil {
			return err
		}
		if inside {
			if every == host.ScheduleOff {
				app.UI.OK("Fluxer no longer backs itself up.")
				return nil
			}
			app.UI.OK("Fluxer backs itself up %s.", fluxerdWords(inst, every))
			app.UI.Info("Those backups stay in Fluxer's volume %s. rexctl backup list copies them to %s, and rexctl backup copy-to sends the newest one to another machine.", rt.VolumeName(inst), inst.BackupDir())
			return nil
		}
		if every == host.ScheduleOff {
			app.UI.OK("rexctl no longer backs up your instance by itself.")
			return nil
		}
		app.UI.OK("rexctl backs up your instance %s.", scheduleWords(every))
		app.UI.Info("Backups go to %s, and %s Run rexctl backup keep to change that.", inst.BackupDir(), keptBackups(inst))
		return nil
	})
}

func fluxerdWords(inst *instance.Instance, every string) string {
	hour := 3
	if n, err := strconv.Atoi(strings.TrimSpace(inst.State.Settings[fluxerdHourVar])); err == nil && n >= 0 && n < 24 {
		hour = n
	}
	at := fmt.Sprintf("%02d:00 UTC", hour)
	if every == host.ScheduleDaily {
		return "every night at about " + at
	}
	return "once a week at about " + at
}

func scheduleWords(every string) string {
	if every == host.ScheduleDaily {
		return "every night at about 03:00"
	}
	return "every Sunday at about 03:00"
}

func setBackupSchedule(ctx context.Context, app *App, inst *instance.Instance, every string) (bool, error) {
	r, err := app.Runtime(ctx, inst)
	if err != nil {
		return false, err
	}
	if r.Kind() == rt.Native && os.Geteuid() != 0 {
		return false, ui.Refused("A systemd install is managed as root.", "Run "+sudoRun(inst.Dir, "backup", "schedule", every)+".")
	}
	if r.Kind() == rt.Container && runtime.GOOS != "linux" {
		return true, setFluxerdSchedule(ctx, app, inst, r, every)
	}
	s, err := backupScheduleFor(r, inst)
	if err != nil {
		return false, ui.Wrap(err, "rexctl can't find its own path to run from the schedule.", "Run rexctl from where it's installed, then try again.")
	}
	err = s.Set(ctx, every)
	switch {
	case errors.Is(err, host.ErrNoScheduler) && r.Kind() == rt.Container:
		return true, setFluxerdSchedule(ctx, app, inst, r, every)
	case errors.Is(err, host.ErrNoScheduler):
		return false, noBackupScheduler()
	case errors.Is(err, host.ErrNotLingering):
		u, _ := user.Current()
		name := "your account"
		if u != nil {
			name = u.Username
		}
		app.UI.Warn("Scheduled backups run only while you're signed in to this machine.", "Ask an administrator to run sudo loginctl enable-linger "+name+" so they run after you sign out too.")
	case err != nil:
		return false, schedulerError(err, "Couldn't change when rexctl backs up your instance.", "rexctl backup schedule "+every, inst.Dir)
	}
	if every != host.ScheduleOff && fluxerdSchedule(inst) != host.ScheduleOff && r.Kind() != rt.Compose {
		off := host.ScheduleOff
		if _, err := applyDeployment(ctx, app, inst, r, map[string]*string{fluxerdScheduleVar: &off}, "Turning off Fluxer's own backup schedule"); err != nil {
			return false, err
		}
	}
	return false, nil
}

func keptBackups(inst *instance.Instance) string {
	if keep := inst.KeepBackups(); keep != instance.KeepAllBackups {
		return fmt.Sprintf("rexctl keeps the newest %d.", keep)
	}
	return "rexctl keeps every backup."
}

func removeBackupSchedule(ctx context.Context, inst *instance.Instance) error {
	r, err := rt.Open(ctx, inst, nil)
	if err != nil {
		return err
	}
	s, err := backupScheduleFor(r, inst)
	if err != nil {
		return err
	}
	return s.Set(ctx, host.ScheduleOff)
}

func runScheduledBackup(ctx context.Context, app *App, stamp string) error {
	dir, err := app.Dir()
	if err != nil {
		return err
	}
	inst, err := instance.Find(dir)
	switch {
	case err == nil && scheduleCovers(inst, stamp):
		return runBackup(ctx, app, "schedule")
	case err != nil && !errors.Is(err, instance.ErrNotFound):
		return err
	}
	if stamp == "" {
		return err
	}
	removed := false
	s, serr := scheduleIn(dir, stamp, runtime.GOOS == "linux" && os.Geteuid() != 0, false)
	if serr == nil {
		removed, serr = s.Remove(ctx)
	}
	if serr != nil || !removed {
		app.UI.Info("The instance this schedule backed up is no longer in %s, so this run did nothing.", dir)
		return nil
	}
	app.UI.Info("The instance this schedule backed up is no longer in %s, so rexctl turned the schedule off.", dir)
	return nil
}

func scheduleCovers(inst *instance.Instance, stamp string) bool {
	return stamp == "" || stamp == inst.Project()
}
