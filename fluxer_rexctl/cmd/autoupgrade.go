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
	"slices"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
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
	Message  string `json:"message"`
	Body     string `json:"body"`
	Title    string `json:"title"`
	Type     string `json:"type"`
}

const noticeReadOnlyStatus = "read-only"

func (n *upgradeNotice) fill() {
	if n.Instance != "" {
		n.Text = n.Instance + ": " + n.Text
	}
	n.Content, n.Message, n.Body = n.Text, n.Text, n.Text
	switch n.Status {
	case "upgraded":
		n.Title, n.Type = "Fluxer upgraded", "success"
	case "failed":
		n.Title, n.Type = "Fluxer upgrade failed", "failure"
	case "needs-move":
		n.Title, n.Type = "Fluxer upgrade waits for you", "warning"
	case noticeReadOnlyStatus:
		n.Title, n.Type = "Uploads on Fluxer are read-only", "info"
	default:
		n.Title, n.Type = "Fluxer test notice", "info"
	}
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
		Long: "Run rexctl upgrade --yes by itself with a systemd timer, or with cron on machines without systemd. On a terminal rexctl asks how often, at what time, what the backup before each upgrade holds and where to send notices. Scripts can pass those choices as flags, and a flag you leave out keeps what you chose before.\n\n" +
			"Each run backs up your instance first, and goes back to the release you had when the new one doesn't start. A scheduled run is skipped while another rexctl command is changing the instance, and the next one tries again.\n\n" +
			"With --notify, rexctl posts JSON to the address when an upgrade installs a new release, fails, or waits for something from you first.\n\n" +
			"The JSON holds the instance, the status (upgraded, failed or needs-move), the release, the title, the type of notice (success, failure, warning or info), and the message in each of the fields text, content, message and body.\n\n" +
			"Slack and Discord webhooks, Gotify and the Apprise API show it as it is. Pass a Gotify address like https://gotify.example.com/message?token=... or an Apprise address like https://apprise.example.com/notify/your-key for those two services.\n\n" +
			"For ntfy, add ?tpl=1&m={{.text}} to the topic address. Put an address with ? or & in it in single quotes, so the shell passes it whole. Pass --notify off to stop the notices.",
		Example: "rexctl auto-upgrade on\n" +
			"rexctl auto-upgrade on --at 04:00 --day sun\n" +
			"rexctl auto-upgrade on --notify 'https://ntfy.sh/your-topic?tpl=1&m={{.text}}'",
		Args: noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runAutoUpgradeOn(c.Context(), app, c, o)
		},
	}
	on.Flags().StringVar(&o.At, "at", "", "time of day to upgrade, as HH:MM in this machine's time zone. Separate several times with commas, like 06:00,18:00.")
	on.Flags().StringVar(&o.Day, "day", "", "run once a week on this day, like sun. Pass daily to run every day.")
	on.Flags().StringVar(&o.Backup, "backup", "", "what the backup before each upgrade holds, full, without-uploads or off")
	on.Flags().StringVar(&o.FullBackup, "full-backup", "", "pass monthly to also make a full backup with uploads on the 1st of every month, or off")
	on.Flags().StringVar(&o.Notify, "notify", "", "web address rexctl posts to when an upgrade installs a new release, fails or waits for you, or off")
	off := &cobra.Command{
		Use:   "off",
		Short: "Stop upgrading Fluxer by itself",
		Long:  "Stop upgrading Fluxer by itself, along with the monthly full backup from --full-backup.",
		Args:  noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runAutoUpgradeOff(c.Context(), app)
		},
	}
	test := &cobra.Command{
		Use:   "test-notify",
		Short: "Send a test notice to the --notify address",
		Long:  "Post a test notice to the address you passed to rexctl auto-upgrade on --notify, with status test. It looks like the notice after an upgrade, so you can check that notices get to you without waiting for a new release.",
		Args:  noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runAutoUpgradeTestNotify(c.Context(), app)
		},
	}
	c.AddCommand(on, off, test)
	return c
}

func runAutoUpgradeTestNotify(ctx context.Context, app *App) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	if inst.State.NotifyURL == "" {
		return ui.Refused("rexctl has no address to send notices to.", "Run rexctl auto-upgrade on with --notify and your address.")
	}
	n := upgradeNotice{Instance: inst.Domain(), Status: "test", Release: inst.State.Release, Text: "This is a test notice from rexctl. It posts here when an upgrade installs a new release, fails or waits for you."}
	n.fill()
	if err := postNotice(ctx, inst.State.NotifyURL, n); err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't send a test notice to %s.", maskURL(inst.State.NotifyURL)), "Check the address, then run rexctl auto-upgrade on with --notify and the right one.")
	}
	app.UI.OK("Sent a test notice to %s.", maskURL(inst.State.NotifyURL))
	return nil
}

func autoUpgradeFor(inst *instance.Instance, e *engine.Engine) (host.AutoUpgrade, error) {
	bin, err := host.RunningBinary()
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
		Unit:      bootUnitName(inst),
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

func autoUpgradeScope(ctx context.Context, app *App, inst *instance.Instance, need bool) ([]host.AutoUpgrade, error) {
	if rt.KindOf(inst) == rt.Native {
		a, err := autoUpgradeFor(inst, nil)
		return []host.AutoUpgrade{a}, err
	}
	e, err := app.anyEngine(ctx, inst)
	if err == nil {
		a, err := autoUpgradeFor(inst, e)
		return []host.AutoUpgrade{a}, err
	}
	if need {
		return nil, err
	}
	app.UI.Detail("%v", err)
	a, ferr := autoUpgradeFor(inst, nil)
	if ferr != nil {
		return nil, ferr
	}
	if !a.Rootless {
		return []host.AutoUpgrade{a}, nil
	}
	system := a
	system.Rootless = false
	return []host.AutoUpgrade{a, system}, nil
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
	case "", "off":
		return "", nil
	case host.FullBackupMonthly:
		return host.FullBackupMonthly, nil
	}
	return "", ui.Usage("Pass --full-backup as monthly or off.")
}

func parseDay(s string) (string, error) {
	switch strings.ToLower(strings.TrimSpace(s)) {
	case "", "daily", "every day":
		return "", nil
	}
	d, ok := host.Weekday(s)
	if !ok {
		return "", errors.New("Pick a day of the week like sun or sunday, or daily.")
	}
	return time.Weekday(d).String()[:3], nil
}

func parseTimes(s string) (string, error) {
	var times []string
	for _, part := range strings.Split(s, ",") {
		t, err := time.Parse("15:04", strings.TrimSpace(part))
		if err != nil {
			return "", errors.New("Pick a time of day like 04:00, or several times like 06:00,18:00.")
		}
		at := t.Format("15:04")
		if slices.Contains(times, at) {
			return "", fmt.Errorf("Pass %s only once.", at)
		}
		times = append(times, at)
	}
	slices.Sort(times)
	return strings.Join(times, ","), nil
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

func parsePolicy(s string) error {
	switch s {
	case settings.BackupFull, settings.BackupWithoutUploads, settings.BackupOff:
		return nil
	}
	return errors.New("Pick full, without-uploads or off.")
}

func parseNotify(s string) error {
	if s == "" || s == "off" {
		return nil
	}
	if u, err := url.Parse(s); err != nil || (u.Scheme != "https" && u.Scheme != "http") || u.Host == "" {
		return errors.New("Pass a web address like https://ntfy.sh/your-topic, or off.")
	}
	return nil
}

func runAutoUpgradeOn(ctx context.Context, app *App, c *cobra.Command, o autoUpgradeOptions) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	if err := managed(inst); err != nil {
		return err
	}
	if rt.KindOf(inst) == rt.Native {
		if err := app.nativeRoot(inst.Dir); err != nil {
			return err
		}
	}
	scopes, err := autoUpgradeScope(ctx, app, inst, true)
	if err != nil {
		return err
	}
	a := scopes[0]
	if a.Scheduler == "" {
		return noScheduler()
	}
	if euid() == 0 {
		if err := rootBinary(a.Binary, inst.Dir); err != nil {
			return err
		}
	}
	day, at, full := "", autoUpgradeDefault, ""
	policy := backupPolicy(inst, &upgradeFlags{})
	if cur, err := host.CheckAutoUpgrade(ctx, a, time.Now()); err == nil && cur.On {
		day, at = a.Requested(cur.Day, cur.At)
		full = cur.FullBackup
		if _, set := inst.State.Settings[settings.BackupsBefore]; !set && parsePolicy(cur.Backup) == nil {
			policy = cur.Backup
		}
	}
	often := "daily"
	if day != "" {
		often = "weekly"
	}
	if c.Flags().Changed("day") {
		if d, err := parseDay(o.Day); err != nil {
			return ui.Usage("Pass --day as a day of the week like sun or sunday, or daily.")
		} else if d == "" {
			often = "daily"
		} else {
			often = "weekly"
		}
	}
	if !c.Flags().Changed("day") {
		o.Day = strings.ToLower(day)
	}
	if !c.Flags().Changed("at") {
		o.At = at
	}
	if !c.Flags().Changed("full-backup") {
		o.FullBackup = full
	}
	if c.Flags().Changed("backup") {
		policy = o.Backup
	}
	notify := inst.State.NotifyURL
	if c.Flags().Changed("notify") {
		notify = o.Notify
	}
	fields := []ui.Field{
		{Title: "How often should rexctl upgrade Fluxer?", Value: &often, Kind: ui.KindSelect, Options: []ui.Option{{Label: "Every day", Value: "daily"}, {Label: "Once a week", Value: "weekly"}}, Hide: func() bool { return c.Flags().Changed("day") }},
		{Flag: "day", FlagLabel: "--day", Title: "On which day?", Value: &o.Day, Default: "sun", Validate: func(v string) error { _, err := parseDay(v); return err }, Hide: func() bool { return often != "weekly" }},
		{Flag: "at", FlagLabel: "--at", Title: "At what time?", Value: &o.At, Validate: func(v string) error { _, err := parseTimes(v); return err }, Description: "In this machine's time zone, like 04:00."},
	}
	if !inst.State.IsVoice() {
		fields = append(fields, ui.Field{Flag: "backup", FlagLabel: "--backup", Title: "What should the backup before each upgrade hold?", Value: &policy, Kind: ui.KindSelect, Validate: parsePolicy, Options: []ui.Option{
			{Label: "Everything", Value: settings.BackupFull},
			{Label: "Everything except uploads", Value: settings.BackupWithoutUploads, Hint: "smaller and faster"},
			{Label: "No backup", Value: settings.BackupOff, Hint: "nothing to go back to if an upgrade breaks something"},
		}})
	}
	fields = append(fields, ui.Field{Flag: "notify", FlagLabel: "--notify", Title: "Where should rexctl send a notice after each upgrade?", Value: &notify, Optional: true, Validate: parseNotify, Description: "A web address like https://ntfy.sh/your-topic. Leave it empty for no notices."})
	if err := app.UI.Ask(c, fields...); err != nil {
		return err
	}
	if often == "daily" {
		o.Day = ""
	}
	day, err = parseDay(o.Day)
	if err != nil {
		return ui.Usage("Pass --day as a day of the week like sun or sunday, or daily.")
	}
	at, err = parseTimes(o.At)
	if err != nil {
		return ui.Usage("Pass --at as a time of day like 04:00, or several times like 06:00,18:00.")
	}
	fullBackup, err := parseFullBackup(o.FullBackup)
	if err != nil {
		return err
	}
	if notify == "off" {
		notify = ""
	}
	if inst.State.IsVoice() && fullBackup != "" {
		return ui.Usage("A voice machine has nothing to back up. Leave out --full-backup.")
	}
	a.At, a.Day, a.FullBackup = at, day, fullBackup
	return app.Locked(ctx, inst, func() error {
		if err := host.InstallAutoUpgrade(ctx, a); err != nil {
			return schedulerError(err, "Couldn't turn on automatic upgrades.", "rexctl auto-upgrade on", inst.Dir)
		}
		if !inst.State.IsVoice() {
			if inst.State.Settings == nil {
				inst.State.Settings = map[string]string{}
			}
			if policy == settings.BackupFull {
				delete(inst.State.Settings, settings.BackupsBefore)
			} else {
				inst.State.Settings[settings.BackupsBefore] = policy
			}
		}
		inst.State.NotifyURL = notify
		if err := saveInstance(inst); err != nil {
			return err
		}
		startDay, start, err := a.Start()
		if err != nil {
			return ui.Usage("Pass --at as a time of day like 04:00, or several times like 06:00,18:00.")
		}
		app.UI.OK("rexctl upgrades %s %s.", autoUpgradeTarget(inst), autoUpgradeWhen(a.Scheduler, startDay, start))
		if fullBackup != "" {
			backupAt, _ := a.BackupStart()
			app.UI.Info("%s", fullBackupWhen(backupAt))
		}
		switch {
		case inst.State.IsVoice():
		case policy == settings.BackupOff:
			app.UI.Warn("Upgrades run without a backup first.", "Run rexctl backup now and then, so there is something to go back to.")
		case policy == settings.BackupWithoutUploads && fullBackup == "":
			app.UI.Info("Backups before upgrades leave out uploads. Run rexctl auto-upgrade on --full-backup monthly to keep a full backup as well.")
		}
		if notify != "" {
			testNotify(ctx, app, inst)
		}
		if !inst.State.IsVoice() {
			app.UI.Info("Each run asks fluxer.dev for the newest release itself, so it doesn't need FLUXER_UPDATE_CHECK.")
		}
		app.UI.Info("Run rexctl auto-upgrade to see when it runs next and how the last run went.")
		return nil
	})
}

func testNotify(ctx context.Context, app *App, inst *instance.Instance) {
	n := upgradeNotice{Instance: inst.Domain(), Status: "test", Release: inst.State.Release, Text: "This is a test notice from rexctl. It posts here when an upgrade installs a new release, fails or waits for you."}
	n.fill()
	if err := postNotice(ctx, inst.State.NotifyURL, n); err != nil {
		app.UI.Warn(fmt.Sprintf("Couldn't send a test notice to %s (%v).", maskURL(inst.State.NotifyURL), err), "Check the address, then run rexctl auto-upgrade on --notify with the right one.")
		return
	}
	app.UI.OK("Sent a test notice to %s.", maskURL(inst.State.NotifyURL))
}

func runAutoUpgradeOff(ctx context.Context, app *App) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	if rt.KindOf(inst) == rt.Native {
		if err := app.nativeRoot(inst.Dir); err != nil {
			return err
		}
	}
	scopes, err := autoUpgradeScope(ctx, app, inst, false)
	if err != nil {
		return err
	}
	return app.Locked(ctx, inst, func() error {
		removed := false
		for _, a := range scopes {
			if a.Scheduler == "" {
				continue
			}
			done, err := host.RemoveAutoUpgrade(ctx, a)
			if err != nil {
				return schedulerError(err, "Couldn't turn off automatic upgrades.", "rexctl auto-upgrade off", inst.Dir)
			}
			removed = removed || done
		}
		if inst.State.NotifyURL != "" {
			inst.State.NotifyURL = ""
			if err := saveInstance(inst); err != nil {
				return err
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

func rootBinary(binary, dir string) error {
	p := instance.RootPath(binary, false)
	if p == "" {
		return nil
	}
	if p == binary {
		p = "it"
	}
	return ui.Refused(fmt.Sprintf("Automatic upgrades need a rexctl that only root can change. This one is %s, and accounts other than root can change %s.", binary, p), "Install rexctl for root with curl -fsSL https://fluxer.dev/install.sh | sudo sh and then run "+sudoLine("/usr/local/bin/rexctl", os.Args[1:], dir)+".")
}

func schedulerError(err error, msg, command, dir string) error {
	if errors.Is(err, host.ErrNeedsRoot) {
		return ui.Refused(msg+" This needs root, and this machine has no sudo.", "Run "+rootAgain(dir)+" as root.")
	}
	return ui.Wrap(err, msg, "Run "+command+" --verbose to see why.")
}

func runAutoUpgradeShow(ctx context.Context, app *App) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	scopes, err := autoUpgradeScope(ctx, app, inst, false)
	if err != nil {
		return err
	}
	if scopes[0].Scheduler == "" {
		if app.Flags.JSON {
			return app.UI.JSON(autoUpgradeJSON{})
		}
		return noScheduler()
	}
	var a host.AutoUpgrade
	var s host.AutoUpgradeState
	for i, cand := range scopes {
		st, err := host.CheckAutoUpgrade(ctx, cand, time.Now())
		if err != nil {
			return ui.Wrap(err, "Couldn't read the automatic upgrade schedule.", "Run rexctl auto-upgrade --verbose to see why.")
		}
		if i == 0 || st.On {
			a, s = cand, st
		}
		if st.On {
			break
		}
	}
	return showAutoUpgrade(app, inst, a, s, time.Now())
}

func showAutoUpgrade(app *App, inst *instance.Instance, a host.AutoUpgrade, s host.AutoUpgradeState, now time.Time) error {
	out := autoUpgradeJSON{On: s.On}
	policy := backupPolicy(inst, &upgradeFlags{})
	if s.Backup == backup.ScopeWithoutUploads {
		policy = settings.BackupWithoutUploads
	}
	if s.On {
		out.Scheduler, out.At, out.NextRun, out.LastRun, out.Running = string(a.Scheduler), s.At, s.Next, s.Last, s.Running
		out.Day, out.FullBackup = strings.ToLower(s.Day), s.FullBackup
		if !inst.State.IsVoice() {
			out.Backup = policy
		}
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
	switch {
	case inst.State.IsVoice():
	case policy == settings.BackupOff:
		app.UI.Warn("Upgrades run without a backup first.", "Run rexctl auto-upgrade on --backup full to back up before each upgrade.")
	case policy == settings.BackupWithoutUploads:
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
	times := host.Times(at)
	if len(times) == 0 {
		return every + " at " + at
	}
	parts := make([]string, 0, len(times))
	for _, t := range times {
		part := "at " + t.At
		if c, err := time.Parse("15:04", t.At); err == nil && s == host.Systemd {
			part = fmt.Sprintf("between %s and %s", t.At, c.Add(host.UpgradeWindow).Format("15:04"))
		}
		if long := host.DayLong(t.Day); long != "" {
			part = "on " + long + " " + part
		}
		parts = append(parts, part)
	}
	return every + " " + ui.JoinAnd(parts)
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
	if b != nil && now.Sub(b.CreatedAt) <= uploadsBackupAge {
		return
	}
	fix := "Run rexctl backup now, and run rexctl auto-upgrade on --full-backup monthly."
	switch {
	case s.FullBackup != "" && s.FullBackupRan:
		fix = fmt.Sprintf("Run rexctl backup now, and read %s to see why the monthly backup failed.", fullBackupLogs(a))
	case s.FullBackup != "":
		fix = "Run rexctl backup now. The monthly backup hasn't run yet."
	}
	app.UI.Warn("No backup from the last 5 weeks has your uploads.", fix)
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
		if app.outcome != "" {
			app.UI.Info("rexctl was stopped, so it didn't send the notice.")
		}
		return
	}
	if runErr != nil && ui.CodeOf(runErr) == ui.ExitUsage {
		return
	}
	dir, err := app.Dir()
	if err != nil {
		return
	}
	inst, err := instance.Find(dir)
	if err != nil {
		if runErr == nil && app.outcome != "" {
			app.UI.Warn(fmt.Sprintf("Couldn't load the notify address, so rexctl didn't send the notice (%v).", err), "")
		}
		return
	}
	if inst.State.NotifyURL == "" {
		return
	}
	if runErr != nil && ui.CodeOf(runErr) == ui.ExitRefused && inst.Busy() {
		return
	}
	n := upgradeNotice{Instance: inst.Domain(), Release: inst.State.Release}
	switch {
	case runErr == nil && app.moveNotice != "":
		n.Status = "needs-move"
		n.Text = legacyNeedsMoveLine(inst.State.Release, app.moveNotice) + " Run rexctl migrate."
		inst.State.MoveNotice = app.moveNotice
		if err := inst.Save(); err != nil {
			app.UI.Detail("recording the notice for Fluxer %s: %v", app.moveNotice, err)
		}
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
	n.fill()
	if err := postNotice(ctx, inst.State.NotifyURL, n); err != nil {
		app.UI.Warn(fmt.Sprintf("Couldn't tell %s how the upgrade went (%v).", maskURL(inst.State.NotifyURL), err), "")
		return
	}
	app.UI.Info("Told %s how the upgrade went.", maskURL(inst.State.NotifyURL))
}

func noticeText(err error) string {
	var ee *ui.ExitError
	if errors.As(err, &ee) && ee.Msg != "" {
		return strings.TrimSpace(ee.Msg + " " + ee.Fix)
	}
	return ui.Capitalise(err.Error())
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
		var ue *url.Error
		if errors.As(err, &ue) {
			return ue.Err
		}
		return err
	}
	defer resp.Body.Close()
	if resp.StatusCode < 200 || resp.StatusCode >= 300 {
		return fmt.Errorf("notify: status %d", resp.StatusCode)
	}
	return nil
}
