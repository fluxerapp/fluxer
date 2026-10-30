// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	autoUpgradeLog     = "auto-upgrade.log"
	autoUpgradeDefault = "04:00"
	notifyTimeout      = 15 * time.Second
	uploadsBackupAge   = 35 * 24 * time.Hour
)

type autoUpgradeJSON struct {
	On         bool   `json:"on"`
	Scheduler  string `json:"scheduler,omitempty"`
	At         string `json:"at,omitempty"`
	Day        string `json:"day,omitempty"`
	Backup     string `json:"backup,omitempty"`
	FullBackup string `json:"full_backup,omitempty"`
	NextRun    string `json:"next_run,omitempty"`
	LastRun    string `json:"last_run,omitempty"`
	ExitCode   *int   `json:"exit_code,omitempty"`
	Running    bool   `json:"running"`
	Notify     string `json:"notify,omitempty"`
	Log        string `json:"log,omitempty"`
}

type upgradeNotice struct {
	Instance string `json:"instance"`
	Status   string `json:"status"`
	Release  string `json:"release"`
	Text     string `json:"text"`
	Content  string `json:"content"`
}

func newAutoUpgradeCmd(app *App) *cobra.Command {
	c := &cobra.Command{
		Use:   "auto-upgrade",
		Short: "Show whether rexctl upgrades Fluxer by itself",
		Long:  "Show whether rexctl upgrades Fluxer, or the voice servers on a voice machine, by itself, when it runs next and how the last run went. Each run is rexctl upgrade --yes and works like an upgrade you run yourself.",
		Args:  noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runAutoUpgradeShow(c.Context(), app)
		},
	}
	app.jsonFlag(c)
	var o autoUpgradeOptions
	on := &cobra.Command{
		Use:   "on",
		Short: "Upgrade Fluxer by itself every day or once a week",
		Long: "Run rexctl upgrade --yes by itself with a systemd timer, or with cron on machines without systemd. It runs every day, or once a week on the day you pass to --day. Each run backs up your instance first and goes back to the release you had if the new one doesn't start. It skips that run if another rexctl command is changing the instance, and tries again at the next one. A weekly run that is skipped or fails waits a week, and you can run rexctl upgrade yourself at any time. With --full-backup monthly it also makes a full backup with uploads on the 1st of every month, 3 hours before the upgrade time. Run it again to change the day, the time, the backups or the address. Leaving out a flag sets it back to its default, and leaving out --notify stops the notifications.\n\n" +
			"With --notify, rexctl posts JSON to the address when an upgrade installs a new release or fails: instance, status (upgraded or failed), release, and the message as both text and content, which Slack and Discord webhooks show as they are. For ntfy, add ?tpl=1&m={{.text}} to the topic address.",
		Args: noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runAutoUpgradeOn(c.Context(), app, o)
		},
	}
	on.Flags().StringVar(&o.At, "at", autoUpgradeDefault, "time of day to upgrade, as HH:MM in this machine's time zone")
	on.Flags().StringVar(&o.Day, "day", "", "run once a week on this day, like sun. Leave it out to run every day.")
	on.Flags().StringVar(&o.Backup, "backup", backup.ScopeFull, "what the backup before each upgrade holds, full or without-uploads. When an upgrade changes how uploads are stored, the backup includes them anyway.")
	on.Flags().StringVar(&o.FullBackup, "full-backup", "", "pass monthly to also make a full backup with uploads on the 1st of every month")
	on.Flags().StringVar(&o.Notify, "notify", "", "web address rexctl posts to when an upgrade installs a new release or fails")
	off := &cobra.Command{
		Use:   "off",
		Short: "Stop upgrading Fluxer by itself",
		Long:  "Stop upgrading Fluxer by itself. This also stops the monthly full backup that --full-backup turned on.",
		Args:  noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runAutoUpgradeOff(c.Context(), app)
		},
	}
	c.AddCommand(on, off)
	return c
}

func autoUpgradeFor(inst *instance.Instance, e *engine.Engine) (host.AutoUpgrade, error) {
	bin, err := checks.RunningBinary()
	if err != nil {
		return host.AutoUpgrade{}, err
	}
	dir, err := filepath.Abs(inst.Dir)
	if err != nil {
		return host.AutoUpgrade{}, err
	}
	a := host.AutoUpgrade{
		Scheduler: host.UpgradeScheduler(),
		Project:   inst.Project(),
		Domain:    inst.Domain(),
		Dir:       dir,
		Binary:    bin,
		Rootless:  runtime.GOOS == "linux" && os.Geteuid() != 0,
		Log:       filepath.Join(dir, instance.StateDirName, autoUpgradeLog),
	}
	if e != nil {
		a.Podman, a.Rootless = e.Kind == engine.Podman, e.Rootless
	}
	return a, nil
}

func noScheduler() error {
	return ui.Refused("rexctl can upgrade Fluxer by itself only on Linux with systemd or cron.", "Run rexctl upgrade --yes every day from your own scheduler.")
}

type autoUpgradeOptions struct {
	At         string
	Day        string
	Backup     string
	FullBackup string
	Notify     string
}

func parseFullBackup(s string) (string, error) {
	switch strings.ToLower(strings.TrimSpace(s)) {
	case "":
		return "", nil
	case host.FullBackupMonthly:
		return host.FullBackupMonthly, nil
	}
	return "", ui.Usage("Pass --full-backup as monthly, or leave it out.")
}

func parseDay(s string) (string, error) {
	if s == "" {
		return "", nil
	}
	d, ok := host.Weekday(s)
	if !ok {
		return "", ui.Usage("Pass --day as a day of the week like sun or sunday.")
	}
	return time.Weekday(d).String()[:3], nil
}

func parseBackupScope(s string) (string, error) {
	switch s {
	case "", backup.ScopeFull:
		return backup.ScopeFull, nil
	case backup.ScopeWithoutUploads:
		return s, nil
	}
	return "", ui.Usage("Pass --backup as full or without-uploads.")
}

func runAutoUpgradeOn(ctx context.Context, app *App, o autoUpgradeOptions) error {
	t, err := time.Parse("15:04", o.At)
	if err != nil {
		return ui.Usage("Pass --at as a time of day like 04:00.")
	}
	at, notify := t.Format("15:04"), o.Notify
	day, err := parseDay(o.Day)
	if err != nil {
		return err
	}
	scope, err := parseBackupScope(o.Backup)
	if err != nil {
		return err
	}
	fullBackup, err := parseFullBackup(o.FullBackup)
	if err != nil {
		return err
	}
	if notify != "" {
		if u, err := url.Parse(notify); err != nil || (u.Scheme != "https" && u.Scheme != "http") || u.Host == "" {
			return ui.Usage("Pass --notify as a web address like https://ntfy.sh/your-topic.")
		}
	}
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	if inst.State.IsVoice() && (scope != backup.ScopeFull || fullBackup != "") {
		return ui.Usage("A voice machine has nothing to back up. Leave out --backup and --full-backup.")
	}
	if err := managed(inst); err != nil {
		return err
	}
	e, err := app.Engine(ctx, inst)
	if err != nil {
		return err
	}
	a, err := autoUpgradeFor(inst, e)
	if err != nil {
		return err
	}
	if a.Scheduler == "" {
		return noScheduler()
	}
	a.At, a.Day, a.Backup, a.FullBackup = at, day, scope, fullBackup
	return app.Locked(ctx, inst, func() error {
		if err := host.InstallAutoUpgrade(ctx, a); err != nil {
			return schedulerError(err, "Couldn't turn on automatic upgrades.", "rexctl auto-upgrade on")
		}
		if inst.State.NotifyURL != notify {
			inst.State.NotifyURL = notify
			if err := inst.Save(); err != nil {
				return ui.Wrap(err, "Couldn't save your instance's state.", "Check that you can write to "+inst.Dir+", then try again.")
			}
		}
		startDay, start, err := a.Start()
		if err != nil {
			return ui.Usage("Pass --at as a time of day like 04:00.")
		}
		app.UI.OK("rexctl upgrades %s %s.", autoUpgradeTarget(inst), autoUpgradeWhen(a.Scheduler, startDay, start))
		if fullBackup != "" {
			backupAt, _ := a.BackupStart()
			app.UI.Info("%s", fullBackupWhen(backupAt))
		}
		if scope == backup.ScopeWithoutUploads && fullBackup == "" {
			leanBackupNote(app, host.AutoUpgradeState{Day: day, At: at, Backup: scope}, notify)
		}
		if notify != "" {
			app.UI.Info("rexctl tells %s when an upgrade installs a new release or fails.", maskURL(notify))
		}
		app.UI.Info("Run rexctl auto-upgrade to see when it runs next and how the last run went.")
		return nil
	})
}

func runAutoUpgradeOff(ctx context.Context, app *App) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	e, err := app.Engine(ctx, inst)
	if err != nil {
		return err
	}
	return app.Locked(ctx, inst, func() error {
		removed, err := removeAutoUpgrade(ctx, inst, e)
		if err != nil {
			return schedulerError(err, "Couldn't turn off automatic upgrades.", "rexctl auto-upgrade off")
		}
		if inst.State.NotifyURL != "" {
			inst.State.NotifyURL = ""
			if err := inst.Save(); err != nil {
				return ui.Wrap(err, "Couldn't save your instance's state.", "Check that you can write to "+inst.Dir+", then try again.")
			}
		}
		if !removed {
			app.UI.OK("Automatic upgrades are already off.")
			return nil
		}
		app.UI.OK("Turned off automatic upgrades. Run rexctl upgrade when you want a new release.")
		return nil
	})
}

func removeAutoUpgrade(ctx context.Context, inst *instance.Instance, e *engine.Engine) (bool, error) {
	a, err := autoUpgradeFor(inst, e)
	if err != nil || a.Scheduler == "" {
		return false, err
	}
	return host.RemoveAutoUpgrade(ctx, a)
}

func schedulerError(err error, msg, command string) error {
	if errors.Is(err, host.ErrNeedsRoot) {
		return ui.Refused(msg+" This needs root.", "Run sudo "+command+".")
	}
	return ui.Wrap(err, msg, "Run "+command+" --verbose to see why.")
}

func runAutoUpgradeShow(ctx context.Context, app *App) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	e, err := app.Engine(ctx, inst)
	if err != nil {
		return err
	}
	a, err := autoUpgradeFor(inst, e)
	if err != nil {
		return err
	}
	if a.Scheduler == "" {
		if app.Flags.JSON {
			return app.UI.JSON(autoUpgradeJSON{})
		}
		return noScheduler()
	}
	s, err := host.CheckAutoUpgrade(ctx, a, time.Now())
	if err != nil {
		return ui.Wrap(err, "Couldn't read the automatic upgrade schedule.", "Run rexctl auto-upgrade --verbose to see why.")
	}
	return showAutoUpgrade(app, inst, a, s, time.Now())
}

func showAutoUpgrade(app *App, inst *instance.Instance, a host.AutoUpgrade, s host.AutoUpgradeState, now time.Time) error {
	out := autoUpgradeJSON{On: s.On}
	if s.On {
		out.Scheduler, out.At, out.NextRun, out.LastRun, out.Running = string(a.Scheduler), s.At, s.Next, s.Last, s.Running
		out.Day, out.Backup, out.FullBackup = strings.ToLower(s.Day), s.Backup, s.FullBackup
		out.Log = autoUpgradeLogs(a)
		if inst.State.NotifyURL != "" {
			out.Notify = maskURL(inst.State.NotifyURL)
		}
		if s.Ran {
			code := s.ExitCode
			out.ExitCode = &code
		}
	}
	if app.Flags.JSON {
		return app.UI.JSON(out)
	}
	if !s.On {
		app.UI.Info("Automatic upgrades are off. Run rexctl auto-upgrade on to upgrade %s every day or once a week.", autoUpgradeTarget(inst))
		return nil
	}
	app.UI.OK("rexctl upgrades %s %s.", autoUpgradeTarget(inst), autoUpgradeWhen(a.Scheduler, s.Day, s.At))
	if s.FullBackup != "" {
		app.UI.Info("%s", fullBackupWhen(s.FullBackupAt))
	}
	if s.Backup == backup.ScopeWithoutUploads {
		uploadsBackupNote(app, inst, a, s, now)
	}
	switch {
	case s.Running:
		app.UI.Info("An automatic upgrade is running now.")
	case s.Ran && s.ExitCode != 0:
		app.UI.Warn(fmt.Sprintf("The last automatic upgrade stopped with exit code %d.", s.ExitCode), "Read its log to see why.")
	}
	rows := [][]string{{"Next run", s.Next}, {"Last run", s.Last}, {"Notify", out.Notify}, {"Log", out.Log}}
	for _, r := range rows {
		if r[1] != "" {
			app.UI.Result(fmt.Sprintf("%-9s %s", r[0], r[1]))
		}
	}
	return nil
}

func autoUpgradeTarget(inst *instance.Instance) string {
	if inst.State.IsVoice() {
		return "the voice servers on this machine"
	}
	return "Fluxer"
}

func autoUpgradeWhen(s host.Scheduler, day, at string) string {
	every := "every day"
	if long := host.DayLong(day); long != "" {
		every = "every " + long
	}
	t, err := time.Parse("15:04", at)
	if err != nil || s != host.Systemd {
		return every + " at " + at
	}
	return fmt.Sprintf("%s between %s and %s", every, at, t.Add(host.UpgradeWindow).Format("15:04"))
}

func leanBackupNote(app *App, s host.AutoUpgradeState, notify string) {
	s.FullBackup = host.FullBackupMonthly
	app.UI.Info("Backups before upgrades leave out uploads. Run %s to keep a full backup as well, or run rexctl backup yourself now and then.", scheduleCommand(s))
	if notify != "" {
		app.UI.Info("Add --notify with your address to that command to keep the notifications.")
	}
}

func uploadsBackupNote(app *App, inst *instance.Instance, a host.AutoUpgrade, s host.AutoUpgradeState, now time.Time) {
	list, err := backup.List(inst.BackupDir())
	if err != nil {
		app.UI.Detail("%v", err)
		return
	}
	b := backup.NewestWithUploads(list)
	if b != nil {
		app.UI.Info("The newest backup with uploads is from %s.", b.CreatedAt.In(now.Location()).Format("2 January 2006 at 15:04 MST"))
	}
	if b == nil || now.Sub(b.CreatedAt) > uploadsBackupAge {
		full := s
		full.Day, full.At = a.Requested(s.Day, s.At)
		full.FullBackup = host.FullBackupMonthly
		fix := fmt.Sprintf("Run rexctl backup now, and run %s.", scheduleCommand(full))
		if inst.State != nil && inst.State.NotifyURL != "" {
			fix += " Add --notify with your address to that command to keep the notifications."
		}
		switch {
		case s.FullBackup != "" && s.FullBackupRan:
			fix = fmt.Sprintf("Run rexctl backup now, and read %s to see why the monthly backup failed.", fullBackupLogs(a))
		case s.FullBackup != "":
			fix = "Run rexctl backup now. The monthly backup hasn't run yet."
		}
		app.UI.Warn("No backup from the last 5 weeks has your uploads.", fix)
	}
}

func fullBackupWhen(at string) string {
	if at == "" {
		return "rexctl makes a full backup with uploads once a month."
	}
	return fmt.Sprintf("rexctl makes a full backup with uploads on the 1st of every month at %s.", at)
}

func fullBackupLogs(a host.AutoUpgrade) string {
	if a.Scheduler == host.Cron {
		return a.BackupLog()
	}
	if a.Rootless {
		return "journalctl --user -u " + a.BackupServiceName()
	}
	return "journalctl -u " + a.BackupServiceName()
}

func autoUpgradeLogs(a host.AutoUpgrade) string {
	if a.Scheduler == host.Cron {
		return a.Log
	}
	if a.Rootless {
		return "journalctl --user -u " + a.ServiceName()
	}
	return "journalctl -u " + a.ServiceName()
}

func maskURL(raw string) string {
	u, err := url.Parse(raw)
	if err != nil || u.Host == "" {
		return "your notification address"
	}
	masked := u.Scheme + "://" + u.Host
	if strings.Trim(u.Path, "/") != "" || u.RawQuery != "" {
		masked += "/…"
	}
	return masked
}

func notifyUpgrade(ctx context.Context, app *App, runErr error) {
	if ctx.Err() != nil || errors.Is(runErr, context.Canceled) {
		return
	}
	dir, err := app.Dir()
	if err != nil {
		return
	}
	inst, err := instance.Find(dir)
	if err != nil || inst.State.NotifyURL == "" {
		return
	}
	n := upgradeNotice{Instance: inst.Domain(), Release: inst.State.Release}
	switch {
	case runErr != nil:
		n.Status, n.Text = "failed", noticeText(runErr)
	case app.exit != 0:
		n.Status, n.Text = "failed", app.outcome
	case app.outcome != "":
		n.Status, n.Text = "upgraded", app.outcome
	default:
		return
	}
	n.Text = strings.Join(append([]string{n.Text}, app.putBack...), " ")
	if n.Instance != "" {
		n.Text = n.Instance + ": " + n.Text
	}
	n.Content = n.Text
	if err := postNotice(ctx, inst.State.NotifyURL, n); err != nil {
		app.UI.Warn(fmt.Sprintf("Couldn't tell %s how the upgrade went.", maskURL(inst.State.NotifyURL)), "")
		app.UI.Detail("%v", err)
	}
}

func noticeText(err error) string {
	var ee *ui.ExitError
	if errors.As(err, &ee) && ee.Msg != "" {
		return strings.TrimSpace(ee.Msg + " " + ee.Fix)
	}
	return capitalise(err.Error())
}

func postNotice(ctx context.Context, rawURL string, n upgradeNotice) error {
	body, err := json.Marshal(n)
	if err != nil {
		return err
	}
	ctx, cancel := context.WithTimeout(ctx, notifyTimeout)
	defer cancel()
	req, err := http.NewRequestWithContext(ctx, http.MethodPost, rawURL, bytes.NewReader(body))
	if err != nil {
		return err
	}
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("User-Agent", "rexctl")
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	if resp.StatusCode < 200 || resp.StatusCode >= 300 {
		return fmt.Errorf("notify: status %d", resp.StatusCode)
	}
	return nil
}
