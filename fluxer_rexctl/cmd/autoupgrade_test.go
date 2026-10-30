// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func notifyRun(t *testing.T, notify bool) (*App, chan upgradeNotice) {
	t.Helper()
	got := make(chan upgradeNotice, 1)
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		var n upgradeNotice
		if r.Method != http.MethodPost || r.Header.Get("Content-Type") != "application/json" || json.NewDecoder(r.Body).Decode(&n) != nil {
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		got <- n
	}))
	t.Cleanup(srv.Close)
	inst, err := instance.Create(t.TempDir(), "fluxer")
	if err != nil {
		t.Fatal(err)
	}
	inst.State.Release = "2026.1027.120000"
	if notify {
		inst.State.NotifyURL = srv.URL + "/hooks/secret"
	}
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	out := &bytes.Buffer{}
	app := &App{
		Flags: GlobalFlags{Dir: inst.Dir},
		UI:    ui.New(ui.Options{Stdin: strings.NewReader(""), Stdout: out, Stderr: out}),
	}
	return app, got
}

func TestNotifyUpgrade(t *testing.T) {
	ctx := context.Background()

	app, got := notifyRun(t, true)
	app.outcome = "Upgraded to Fluxer 2026.1027.120000."
	notifyUpgrade(ctx, app, nil)
	n := <-got
	if n.Status != "upgraded" || n.Release != "2026.1027.120000" || n.Text != n.Content || !strings.HasSuffix(n.Text, "Upgraded to Fluxer 2026.1027.120000.") {
		t.Errorf("upgraded notice = %+v", n)
	}

	app, got = notifyRun(t, true)
	notifyUpgrade(ctx, app, ui.Refused("Another rexctl command is changing this instance.", "Wait for it to finish, then try again."))
	n = <-got
	if n.Status != "failed" || !strings.HasSuffix(n.Text, "Another rexctl command is changing this instance. Wait for it to finish, then try again.") {
		t.Errorf("failed notice = %+v", n)
	}

	app, got = notifyRun(t, true)
	app.outcome = "Fluxer 2026.1027.120000 didn't start. Went back to Fluxer 2026.1020.100000."
	app.SetExit(ui.ExitFailed)
	notifyUpgrade(ctx, app, nil)
	if n = <-got; n.Status != "failed" {
		t.Errorf("went-back notice = %+v", n)
	}

	app, got = notifyRun(t, true)
	notifyUpgrade(ctx, app, nil)
	notifyUpgrade(ctx, app, context.Canceled)
	app, _ = notifyRun(t, false)
	app.outcome = "Upgraded to Fluxer 2026.1027.120000."
	notifyUpgrade(ctx, app, nil)
	select {
	case n := <-got:
		t.Errorf("sent %+v for a run with nothing to tell", n)
	default:
	}
}

func TestMaskURL(t *testing.T) {
	cases := map[string]string{
		"https://discord.com/api/webhooks/1/secret": "https://discord.com/…",
		"https://ntfy.sh/topic?tpl=1":               "https://ntfy.sh/…",
		"https://hooks.example.com":                 "https://hooks.example.com",
	}
	for in, want := range cases {
		if got := maskURL(in); got != want {
			t.Errorf("maskURL(%s) = %s, want %s", in, got, want)
		}
	}
}

func TestAutoUpgradeWhen(t *testing.T) {
	if got := autoUpgradeWhen(host.Systemd, "", "23:45"); got != "every day between 23:45 and 00:15" {
		t.Errorf("systemd: %s", got)
	}
	if got := autoUpgradeWhen(host.Cron, "", "04:00"); got != "every day at 04:00" {
		t.Errorf("cron: %s", got)
	}
	if got := autoUpgradeWhen(host.Systemd, "Sun", "04:00"); got != "every Sunday between 04:00 and 04:30" {
		t.Errorf("weekly systemd gives %q", got)
	}
	if got := autoUpgradeWhen(host.Cron, "Mon", "00:10"); got != "every Monday at 00:10" {
		t.Errorf("weekly cron gives %q", got)
	}
}

func TestUnknownDayIsRefused(t *testing.T) {
	app, _ := notifyRun(t, false)
	for _, day := range []string{"funday", "su", "8", "sundays"} {
		err := runAutoUpgradeOn(context.Background(), app, autoUpgradeOptions{At: "04:00", Day: day})
		var ee *ui.ExitError
		if !errors.As(err, &ee) || ee.Code != ui.ExitUsage || !strings.Contains(ee.Msg, "--day") {
			t.Errorf("--day %s returned %v, want a usage error about --day", day, err)
		}
	}
	err := runAutoUpgradeOn(context.Background(), app, autoUpgradeOptions{At: "04:00", Backup: "some"})
	var ee *ui.ExitError
	if !errors.As(err, &ee) || ee.Code != ui.ExitUsage || !strings.Contains(ee.Msg, "--backup") {
		t.Errorf("--backup some returned %v, want a usage error about --backup", err)
	}
}

func TestVoiceMachineRefusesBackupOptions(t *testing.T) {
	inst := voiceMachine(t)
	app, _ := testVoiceApp()
	app.Flags.Dir = inst.Dir
	for _, o := range []autoUpgradeOptions{{At: "04:00", FullBackup: "monthly"}, {At: "04:00", Backup: backup.ScopeWithoutUploads}} {
		err := runAutoUpgradeOn(context.Background(), app, o)
		var ee *ui.ExitError
		if !errors.As(err, &ee) || ee.Code != ui.ExitUsage || !strings.Contains(ee.Msg, "voice machine") {
			t.Errorf("%+v returned %v, want a usage error about the voice machine", o, err)
		}
	}
}

func TestDayAcceptsFullNames(t *testing.T) {
	for in, want := range map[string]string{"sun": "Sun", "Sunday": "Sun", "MONDAY": "Mon", "thu": "Thu", "saturday": "Sat", "": ""} {
		got, err := parseDay(in)
		if err != nil || got != want {
			t.Errorf("parseDay(%q) = %q, %v, want %q", in, got, err, want)
		}
	}
}

func scheduledStatus(t *testing.T, a host.AutoUpgrade) host.AutoUpgradeState {
	t.Helper()
	if runtime.GOOS == "windows" {
		t.Skip("needs a shell to fake systemctl")
	}
	root := t.TempDir()
	bin := filepath.Join(root, "bin")
	if err := os.MkdirAll(bin, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(bin, "systemctl"), []byte("#!/bin/sh\nexit 0\n"), 0o755); err != nil {
		t.Fatal(err)
	}
	t.Setenv("PATH", bin)
	t.Setenv("XDG_CONFIG_HOME", filepath.Join(root, "config"))
	if err := host.InstallAutoUpgrade(context.Background(), a); err != nil {
		t.Fatal(err)
	}
	s, err := host.CheckAutoUpgrade(context.Background(), a, time.Now())
	if err != nil {
		t.Fatal(err)
	}
	return s
}

func autoUpgradeStatus(t *testing.T, a host.AutoUpgrade) map[string]any {
	t.Helper()
	app, _ := notifyRun(t, false)
	app.Flags.JSON = true
	out := app.UI.Stdout().(*bytes.Buffer)
	inst, err := instance.Find(app.Flags.Dir)
	if err != nil {
		t.Fatal(err)
	}
	a.Dir, a.Binary = inst.Dir, filepath.Join(inst.Dir, "rexctl")
	s := scheduledStatus(t, a)
	if err := showAutoUpgrade(app, inst, a, s, time.Now()); err != nil {
		t.Fatal(err)
	}
	var got map[string]any
	if err := json.Unmarshal(out.Bytes(), &got); err != nil {
		t.Fatalf("status JSON %q doesn't decode (%v)", out.String(), err)
	}
	return got
}

func TestStatusJSONShowsDayAndBackup(t *testing.T) {
	a := host.AutoUpgrade{Scheduler: host.Systemd, Project: "fluxer", Rootless: true, At: "04:00", Day: "Sun", Backup: backup.ScopeWithoutUploads}
	got := autoUpgradeStatus(t, a)
	if got["day"] != "sun" || got["backup"] != "without-uploads" || got["at"] != "04:00" || got["on"] != true {
		t.Errorf("weekly status JSON = %v", got)
	}
	if _, ok := got["full_backup"]; ok {
		t.Errorf("status JSON without a monthly backup = %v", got)
	}
	a.Day, a.Backup = "", backup.ScopeFull
	got = autoUpgradeStatus(t, a)
	if _, ok := got["day"]; ok || got["backup"] != "full" {
		t.Errorf("daily status JSON = %v", got)
	}
	a.FullBackup = host.FullBackupMonthly
	if got = autoUpgradeStatus(t, a); got["full_backup"] != "monthly" {
		t.Errorf("status JSON with a monthly backup = %v", got)
	}
}

func TestFullBackupAcceptsOnlyMonthly(t *testing.T) {
	for in, want := range map[string]string{"": "", "monthly": "monthly", "Monthly": "monthly"} {
		if got, err := parseFullBackup(in); err != nil || got != want {
			t.Errorf("parseFullBackup(%q) = %q, %v, want %q", in, got, err, want)
		}
	}
	app, _ := notifyRun(t, false)
	err := runAutoUpgradeOn(context.Background(), app, autoUpgradeOptions{At: "04:00", FullBackup: "weekly"})
	var ee *ui.ExitError
	if !errors.As(err, &ee) || ee.Code != ui.ExitUsage || !strings.Contains(ee.Msg, "--full-backup") {
		t.Errorf("--full-backup weekly returned %v, want a usage error about --full-backup", err)
	}
}

func TestStatusWarnsWithoutRecentUploadsBackup(t *testing.T) {
	now := time.Date(2026, 11, 20, 12, 0, 0, 0, time.UTC)
	app, _ := notifyRun(t, false)
	out := app.UI.Stdout().(*bytes.Buffer)
	inst, err := instance.Find(app.Flags.Dir)
	if err != nil {
		t.Fatal(err)
	}
	a := host.AutoUpgrade{Project: "fluxer", Dir: "/opt/fluxer", Binary: "/usr/local/bin/rexctl", Podman: true}
	const warning = "No backup from the last 5 weeks has your uploads."
	weekly := host.AutoUpgradeState{On: true, Day: "Sun", At: "04:00", Backup: backup.ScopeWithoutUploads}
	uploadsBackupNote(app, inst, a, weekly, now)
	if s := out.String(); !strings.Contains(s, warning) || !strings.Contains(s, "Run rexctl backup now, and run rexctl auto-upgrade on --day sun --backup without-uploads --full-backup monthly.") || strings.Contains(s, "cron") || strings.Contains(s, "--notify") {
		t.Errorf("with no backups the status says %q", s)
	}
	out.Reset()
	cron := a
	cron.Scheduler, cron.Domain = host.Cron, "chat.example.com"
	cron.At, cron.Day = "03:30", "Sat"
	day, at, err := cron.Start()
	if err != nil {
		t.Fatal(err)
	}
	inst.State.NotifyURL = "https://ntfy.sh/fluxer-upgrades"
	uploadsBackupNote(app, inst, cron, host.AutoUpgradeState{On: true, Day: day, At: at, Backup: backup.ScopeWithoutUploads}, now)
	inst.State.NotifyURL = ""
	if s := out.String(); !strings.Contains(s, "run rexctl auto-upgrade on --day sat --at 03:30 --backup without-uploads --full-backup monthly. Add --notify with your address to that command to keep the notifications.") {
		t.Errorf("with a cron schedule and notifications the status says %q", s)
	}
	out.Reset()
	for _, tc := range []struct {
		scheduler host.Scheduler
		rootless  bool
		want      string
	}{
		{host.Systemd, false, "read journalctl -u fluxer-backup.service to see why the monthly backup failed"},
		{host.Systemd, true, "read journalctl --user -u fluxer-backup.service to see why"},
		{host.Cron, false, "read /opt/fluxer/.rexctl/full-backup.log to see why"},
	} {
		out.Reset()
		scheduled := a
		scheduled.Scheduler, scheduled.Rootless, scheduled.FullBackup, scheduled.Log = tc.scheduler, tc.rootless, host.FullBackupMonthly, "/opt/fluxer/.rexctl/auto-upgrade.log"
		uploadsBackupNote(app, inst, scheduled, host.AutoUpgradeState{FullBackup: host.FullBackupMonthly}, now)
		if s := out.String(); !strings.Contains(s, "Run rexctl backup now. The monthly backup hasn't run yet.") {
			t.Errorf("with a monthly %s backup that never ran the status says %q", tc.scheduler, s)
		}
		out.Reset()
		uploadsBackupNote(app, inst, scheduled, host.AutoUpgradeState{FullBackup: host.FullBackupMonthly, FullBackupRan: true}, now)
		if s := out.String(); !strings.Contains(s, warning) || !strings.Contains(s, tc.want) {
			t.Errorf("with a monthly %s backup and none recent the status says %q", tc.scheduler, s)
		}
	}
	out.Reset()
	writeTestBackup(t, inst, "2026.1020.100000", time.Date(2026, 10, 12, 4, 0, 0, 0, time.UTC), "")
	writeTestBackup(t, inst, "2026.1020.100000", time.Date(2026, 11, 15, 4, 0, 0, 0, time.UTC), backup.ScopeWithoutUploads)
	out.Reset()
	uploadsBackupNote(app, inst, a, host.AutoUpgradeState{}, now)
	if !strings.Contains(out.String(), "The newest backup with uploads is from 12 October 2026 at 04:00 UTC.") || !strings.Contains(out.String(), warning) {
		t.Errorf("with an old full backup the status says %q", out.String())
	}
	out.Reset()
	uploadsBackupNote(app, inst, a, host.AutoUpgradeState{}, time.Date(2026, 10, 20, 12, 0, 0, 0, time.UTC))
	if strings.Contains(out.String(), warning) || !strings.Contains(out.String(), "12 October 2026") {
		t.Errorf("with a recent full backup the status says %q", out.String())
	}
}

func TestLeanBackupNoteKeepsTheSchedule(t *testing.T) {
	app, _ := notifyRun(t, false)
	out := app.UI.Stdout().(*bytes.Buffer)
	leanBackupNote(app, host.AutoUpgradeState{Day: "Sun", At: "04:00", Backup: backup.ScopeWithoutUploads}, "")
	if s := out.String(); !strings.Contains(s, "Run rexctl auto-upgrade on --day sun --backup without-uploads --full-backup monthly to keep a full backup as well") || strings.Contains(s, "--notify") {
		t.Errorf("auto-upgrade on --day sun --backup without-uploads says %q", s)
	}
	out.Reset()
	leanBackupNote(app, host.AutoUpgradeState{At: "02:15", Backup: backup.ScopeWithoutUploads}, "https://ntfy.sh/fluxer-upgrades")
	if s := out.String(); !strings.Contains(s, "Run rexctl auto-upgrade on --at 02:15 --backup without-uploads --full-backup monthly to keep") || !strings.Contains(s, "Add --notify with your address to that command to keep the notifications.") {
		t.Errorf("auto-upgrade on --at 02:15 with --notify says %q", s)
	}
}

func TestMonthlyBackupOnTheFirstIsRecentAllMonth(t *testing.T) {
	app, _ := notifyRun(t, false)
	out := app.UI.Stdout().(*bytes.Buffer)
	inst, err := instance.Find(app.Flags.Dir)
	if err != nil {
		t.Fatal(err)
	}
	stockholm, err := time.LoadLocation("Europe/Stockholm")
	if err != nil {
		t.Skip(err)
	}
	writeTestBackup(t, inst, "2026.1020.100000", time.Date(2026, 10, 1, 3, 0, 0, 0, stockholm), "")
	uploadsBackupNote(app, inst, host.AutoUpgrade{Project: "fluxer"}, host.AutoUpgradeState{}, time.Date(2026, 11, 1, 2, 59, 0, 0, stockholm))
	if strings.Contains(out.String(), "No backup from the last") {
		t.Errorf("a backup from 1 October counts as too old on 1 November before 03:00, output %q", out.String())
	}
	app, _ = notifyRun(t, false)
	out = app.UI.Stdout().(*bytes.Buffer)
	if inst, err = instance.Find(app.Flags.Dir); err != nil {
		t.Fatal(err)
	}
	writeTestBackup(t, inst, "2026.1020.100000", time.Date(2026, 10, 1, 1, 0, 0, 0, stockholm), "")
	uploadsBackupNote(app, inst, host.AutoUpgrade{Project: "fluxer"}, host.AutoUpgradeState{}, time.Date(2026, 10, 2, 12, 0, 0, 0, stockholm))
	if want := "The newest backup with uploads is from 1 October 2026 at 01:00 CEST."; !strings.Contains(out.String(), want) {
		t.Errorf("a monthly backup at 01:00 on the 1st shows as %q, want %q", out.String(), want)
	}
}
