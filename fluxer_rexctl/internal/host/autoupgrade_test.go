// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
	"time"
)

func TestAutoUpgradeService(t *testing.T) {
	a := AutoUpgrade{Scheduler: Systemd, Project: "fluxer", Dir: "/opt/fluxer", Binary: "/usr/local/bin/rexctl", At: "04:00"}
	want := `[Unit]
Description=Upgrade Fluxer in /opt/fluxer
Wants=network-online.target
After=network-online.target fluxer.service

[Service]
Type=oneshot
ExecStart=/usr/local/bin/rexctl upgrade --yes --dir /opt/fluxer
`
	if got := a.service(""); got != want {
		t.Errorf("service as root:\n%s\nwant:\n%s", got, want)
	}
	a.Dir, a.Binary = "/home/alice/my fluxer", "/home/alice/.local/bin/rexctl"
	want = `[Unit]
Description=Upgrade Fluxer in /home/alice/my fluxer
Wants=network-online.target
After=network-online.target fluxer.service

[Service]
Type=oneshot
User=alice
ExecStart=/home/alice/.local/bin/rexctl upgrade --yes --dir "/home/alice/my fluxer"
ExecStopPost=-+/usr/bin/install -D -m 0755 /home/alice/.local/bin/rexctl /usr/local/libexec/rexctl/rexctl
`
	if got := a.service("alice"); got != want {
		t.Errorf("service as owner:\n%s\nwant:\n%s", got, want)
	}
	a.Rootless = true
	if got := a.service(""); strings.Contains(got, "network-online") || !strings.Contains(got, "After=fluxer.service\n") {
		t.Errorf("rootless service waits for the wrong units:\n%s", got)
	}
}

func TestUpgradeWaitsForMonthlyBackup(t *testing.T) {
	a := AutoUpgrade{Scheduler: Systemd, Project: "fluxer", Dir: "/opt/fluxer", Binary: "/usr/local/bin/rexctl", At: "04:00"}
	if got := a.service(""); !strings.Contains(got, "\nAfter=network-online.target fluxer.service\n") {
		t.Errorf("without a monthly backup the upgrade waits for:\n%s", got)
	}
	a.FullBackup = FullBackupMonthly
	if got := a.service(""); !strings.Contains(got, "\nAfter=network-online.target fluxer.service fluxer-backup.service\n") {
		t.Errorf("with a monthly backup the upgrade doesn't wait for it:\n%s", got)
	}
	a.Rootless = true
	if got := a.service(""); !strings.Contains(got, "\nAfter=fluxer.service fluxer-backup.service\n") {
		t.Errorf("rootless, with a monthly backup the upgrade doesn't wait for it:\n%s", got)
	}
}

func TestAutoUpgradeTimer(t *testing.T) {
	a := AutoUpgrade{Scheduler: Systemd, Project: "blue", Dir: "/srv/blue", Binary: "/usr/local/bin/rexctl", At: "23:05"}
	hour, minute, err := a.clock()
	if err != nil {
		t.Fatal(err)
	}
	want := `[Unit]
Description=Upgrade Fluxer in /srv/blue every day

[Timer]
OnCalendar=*-*-* 23:05:00
RandomizedDelaySec=30min
Persistent=true

[Install]
WantedBy=timers.target
`
	got := a.timer(-1, hour, minute)
	if got != want {
		t.Errorf("timer:\n%s\nwant:\n%s", got, want)
	}
	if day, at := timerSchedule([]byte(got)); day != "" || at != "23:05" {
		t.Errorf("timerSchedule = %q %q, want every day at 23:05", day, at)
	}
	if a.TimerName() != "blue-upgrade.timer" || a.ServiceName() != "blue-upgrade.service" {
		t.Errorf("unit names are %s and %s", a.TimerName(), a.ServiceName())
	}
}

func TestAutoUpgradeCron(t *testing.T) {
	a := AutoUpgrade{Scheduler: Cron, Project: "fluxer", Dir: "/srv/it's 100%", Binary: "/usr/local/bin/rexctl", At: "04:07", Log: "/srv/it's 100%/.rexctl/auto-upgrade.log"}
	want := `SHELL=/bin/sh
PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
7 4 * * * root '/usr/local/bin/rexctl' upgrade --yes --dir '/srv/it'\''s 100\%' >'/srv/it'\''s 100\%/.rexctl/auto-upgrade.log' 2>&1; echo "rexctl exited with $?" >>'/srv/it'\''s 100\%/.rexctl/auto-upgrade.log'
`
	got := a.cron(-1, 4, 7, "")
	if got != want {
		t.Errorf("cron:\n%s\nwant:\n%s", got, want)
	}
	if day, at := cronSchedule([]byte(got)); day != "" || at != "04:07" {
		t.Errorf("cronSchedule = %q %q, want every day at 04:07", day, at)
	}
	if !strings.Contains(a.cron(-1, 4, 7, "alice"), " * * * alice ") {
		t.Error("cron doesn't run as the instance owner")
	}
	paths, err := a.Paths()
	if err != nil || len(paths) != 1 || paths[0] != "/etc/cron.d/fluxer-upgrade" {
		t.Errorf("Paths() = %v, %v", paths, err)
	}
}

func TestAutoUpgradeRefusesBadInput(t *testing.T) {
	for _, a := range []AutoUpgrade{
		{Scheduler: Systemd, Project: "fluxer", Dir: "fluxer", Binary: "/usr/local/bin/rexctl", At: "04:00"},
		{Scheduler: Systemd, Project: "fluxer", Dir: "/opt/fluxer", Binary: "/usr/local/bin/rexctl", At: "4am"},
		{Scheduler: Cron, Project: "fluxer", Dir: "/opt/fluxer", Binary: "/usr/local/bin/rexctl", At: "04:00"},
		{Project: "fluxer", Dir: "/opt/fluxer", Binary: "/usr/local/bin/rexctl", At: "04:00"},
	} {
		if _, err := a.files(); err == nil {
			t.Errorf("files() accepted %+v", a)
		}
	}
}

func TestAutoUpgradeCronStart(t *testing.T) {
	a := AutoUpgrade{Scheduler: Cron, Project: "fluxer", Domain: "chat.example.com", Dir: "/srv/fluxer", Binary: "/usr/local/bin/rexctl", At: "04:00", Log: "/srv/fluxer/.rexctl/auto-upgrade.log"}
	_, start, err := a.Start()
	if err != nil {
		t.Fatal(err)
	}
	if _, again, _ := a.Start(); again != start {
		t.Errorf("Start() = %q then %q, want the same time every time", start, again)
	}
	if start < "04:00" || start >= "04:30" {
		t.Errorf("Start() = %q, want a time from 04:00 to 04:29", start)
	}
	files, err := a.files()
	if _, at := cronSchedule(files[0]); err != nil || at != start {
		t.Errorf("cron file runs at %q, want %q (%v)", at, start, err)
	}
	spread := func(vary func(*AutoUpgrade, int)) int {
		starts := map[string]bool{}
		for i := range 8 {
			b := a
			vary(&b, i)
			_, s, _ := b.Start()
			starts[s] = true
		}
		return len(starts)
	}
	if n := spread(func(b *AutoUpgrade, i int) { b.Domain = fmt.Sprintf("chat%d.example.com", i) }); n < 2 {
		t.Errorf("eight servers with the default project and folder start at %d time, want them spread out", n)
	}
	if n := spread(func(b *AutoUpgrade, i int) { b.Dir = fmt.Sprintf("/srv/fluxer%d", i) }); n < 2 {
		t.Errorf("eight instances on one host start at %d time, want them spread out", n)
	}
	late := a
	late.At = "23:59"
	if _, s, _ := late.Start(); s >= "00:29" && s < "23:59" {
		t.Errorf("Start() for 23:59 = %q, want it to wrap past midnight", s)
	}
	sd := a
	sd.Scheduler = Systemd
	if _, s, _ := sd.Start(); s != "04:00" {
		t.Errorf("systemd Start() = %q, want 04:00 because the timer adds its own delay", s)
	}
}

func TestCronExit(t *testing.T) {
	log := filepath.Join(t.TempDir(), "auto-upgrade.log")
	if ran, _ := cronExit(log); ran {
		t.Error("a missing log counts as a run")
	}
	if err := os.WriteFile(log, []byte("✓ Fluxer is up to date.\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if ran, _ := cronExit(log); ran {
		t.Error("a log without an exit line counts as a finished run")
	}
	if err := os.WriteFile(log, []byte("✗ Another rexctl command is changing this instance.\nrexctl exited with 3\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if ran, code := cronExit(log); !ran || code != 3 {
		t.Errorf("cronExit = %v, %d, want true, 3", ran, code)
	}
}

func TestNextRunDaily(t *testing.T) {
	loc := time.FixedZone("CET", 3600)
	now := time.Date(2026, 10, 27, 5, 0, 0, 0, loc)
	if got := nextRun(now, "", 4, 0); !got.Equal(time.Date(2026, 10, 28, 4, 0, 0, 0, loc)) {
		t.Errorf("after the time: %v", got)
	}
	if got := nextRun(now, "", 6, 30); !got.Equal(time.Date(2026, 10, 27, 6, 30, 0, 0, loc)) {
		t.Errorf("before the time: %v", got)
	}
}

func TestNextRunWeeklyAcrossDST(t *testing.T) {
	loc, err := time.LoadLocation("Europe/Stockholm")
	if err != nil {
		t.Skip("no time zone data on this machine")
	}
	now := time.Date(2026, 10, 18, 5, 0, 0, 0, loc)
	got := nextRun(now, "Sun", 4, 0)
	want := time.Date(2026, 10, 25, 4, 0, 0, 0, loc)
	if !got.Equal(want) || got.Hour() != 4 {
		t.Errorf("next Sunday run across the clock change = %v, want %v", got, want)
	}
	if got.Sub(now) == 7*24*time.Hour-time.Hour {
		t.Error("the next run was counted in hours and lost the clock change")
	}
	if got := nextRun(time.Date(2026, 10, 25, 3, 0, 0, 0, loc), "sun", 4, 0); !got.Equal(want) {
		t.Errorf("earlier the same Sunday = %v, want %v", got, want)
	}
	if got := nextRun(time.Date(2026, 10, 26, 3, 0, 0, 0, loc), "Mon", 4, 0); got.Weekday() != time.Monday || got.Day() != 26 {
		t.Errorf("Monday run = %v, want Monday 26 October", got)
	}
}

func TestTimerDailyIsUnchanged(t *testing.T) {
	a := AutoUpgrade{Scheduler: Systemd, Project: "fluxer", Dir: "/opt/fluxer", Binary: "/usr/local/bin/rexctl", At: "04:00"}
	files, err := a.files()
	if err != nil {
		t.Fatal(err)
	}
	want := `[Unit]
Description=Upgrade Fluxer in /opt/fluxer every day

[Timer]
OnCalendar=*-*-* 04:00:00
RandomizedDelaySec=30min
Persistent=true

[Install]
WantedBy=timers.target
`
	if string(files[1]) != want {
		t.Errorf("daily timer:\n%s\nwant:\n%s", files[1], want)
	}
	a.Backup = BackupFull
	full, err := a.files()
	if err != nil || string(full[0]) != string(files[0]) || string(full[1]) != want {
		t.Errorf("a full backup changed the units (%v)", err)
	}
}

func TestTimerWeeklyAddsWeekday(t *testing.T) {
	a := AutoUpgrade{Scheduler: Systemd, Project: "fluxer", Dir: "/opt/fluxer", Binary: "/usr/local/bin/rexctl", At: "04:00", Day: "Sun"}
	files, err := a.files()
	if err != nil {
		t.Fatal(err)
	}
	timer := string(files[1])
	for _, line := range []string{"Description=Upgrade Fluxer in /opt/fluxer every Sunday\n", "OnCalendar=Sun *-*-* 04:00:00\n", "RandomizedDelaySec=30min\n", "Persistent=true\n"} {
		if !strings.Contains(timer, line) {
			t.Errorf("weekly timer has no %q:\n%s", line, timer)
		}
	}
	if day, at := timerSchedule(files[1]); day != "Sun" || at != "04:00" {
		t.Errorf("timerSchedule = %q %q, want Sun 04:00", day, at)
	}
	day, start, err := a.Start()
	if err != nil || day != "Sun" || start != "04:00" {
		t.Errorf("Start() = %q %q %v, want Sun 04:00", day, start, err)
	}
	a.Day = "Funday"
	if _, err := a.files(); err == nil {
		t.Error("files() accepted a day that doesn't exist")
	}
}

func TestCronWeeklyDayField(t *testing.T) {
	a := AutoUpgrade{Scheduler: Cron, Project: "fluxer", Dir: "/srv/fluxer", Binary: "/usr/local/bin/rexctl", At: "04:00", Day: "Wed", Log: "/srv/fluxer/.rexctl/auto-upgrade.log"}
	got := a.cron(3, 4, 7, "")
	if !strings.Contains(got, "\n7 4 * * 3 root ") {
		t.Errorf("weekly cron line:\n%s", got)
	}
	if day, at := cronSchedule([]byte(got)); day != "Wed" || at != "04:07" {
		t.Errorf("cronSchedule = %q %q, want Wed 04:07", day, at)
	}
	files, err := a.files()
	if err != nil {
		t.Fatal(err)
	}
	day, start, _ := a.Start()
	if d, at := cronSchedule(files[0]); d != day || at != start || day != "Wed" {
		t.Errorf("cron file runs %q %q, Start() says %q %q", d, at, day, start)
	}
}

func TestCronOffsetPastMidnightMovesWeekday(t *testing.T) {
	a := AutoUpgrade{Scheduler: Cron, Project: "fluxer", Dir: "/srv/fluxer", Binary: "/usr/local/bin/rexctl", Day: "sun", At: "23:50", Log: "/srv/fluxer/.rexctl/auto-upgrade.log"}
	found := false
	for i := range 64 {
		b := a
		b.Domain = fmt.Sprintf("chat%d.example.com", i)
		day, hour, minute, err := b.start()
		if err != nil {
			t.Fatal(err)
		}
		if hour*60+minute >= 23*60+50 {
			if day != 0 {
				t.Errorf("%s starts at %02d:%02d on day %d, want Sunday", b.Domain, hour, minute, day)
			}
			continue
		}
		found = true
		if day != 1 {
			t.Errorf("%s starts at %02d:%02d on day %d, want Monday", b.Domain, hour, minute, day)
		}
		files, err := b.files()
		if err != nil {
			t.Fatal(err)
		}
		want := fmt.Sprintf("\n%d %d * * 1 root ", minute, hour)
		if !strings.Contains(string(files[0]), want) {
			t.Errorf("cron file has no %q:\n%s", want, files[0])
		}
	}
	if !found {
		t.Fatal("no domain pushed the start past midnight")
	}
	sat := a
	sat.Day = "Saturday"
	for i := range 64 {
		sat.Domain = fmt.Sprintf("chat%d.example.com", i)
		if day, hour, _, _ := sat.start(); hour == 0 && day != 0 {
			t.Errorf("Saturday 23:50 moved past midnight to day %d, want Sunday", day)
		}
	}
}

func TestScheduleReadsOlderFiles(t *testing.T) {
	timer := "[Unit]\nDescription=Upgrade Fluxer in /opt/fluxer every day\n\n[Timer]\nOnCalendar=*-*-* 04:00:00\nRandomizedDelaySec=30min\nPersistent=true\n"
	if day, at := timerSchedule([]byte(timer)); day != "" || at != "04:00" {
		t.Errorf("older timer reads as %q %q", day, at)
	}
	cron := "SHELL=/bin/sh\nPATH=/usr/bin:/bin\n12 4 * * * root '/usr/local/bin/rexctl' upgrade --yes --dir '/opt/fluxer' >'/opt/fluxer/.rexctl/auto-upgrade.log' 2>&1\n"
	if day, at := cronSchedule([]byte(cron)); day != "" || at != "04:12" {
		t.Errorf("older cron reads as %q %q", day, at)
	}
	if b := serviceBackup([]byte(cron)); b != BackupFull {
		t.Errorf("older cron reads as backup %q", b)
	}
	if day, _ := cronSchedule([]byte("0 4 * * 7 root rexctl upgrade --yes\n")); day != "Sun" {
		t.Errorf("cron day 7 reads as %q, want Sun", day)
	}
}

func TestServiceAddsBackupFlagOnlyWhenLean(t *testing.T) {
	a := AutoUpgrade{Scheduler: Systemd, Project: "fluxer", Dir: "/opt/fluxer", Binary: "/usr/local/bin/rexctl", At: "04:00"}
	if got := a.service(""); strings.Contains(got, "--backup") {
		t.Errorf("default service passes --backup:\n%s", got)
	}
	a.Backup = "without-uploads"
	got := a.service("")
	if !strings.Contains(got, "ExecStart=/usr/local/bin/rexctl upgrade --yes --backup without-uploads --dir /opt/fluxer\n") {
		t.Errorf("lean service:\n%s", got)
	}
	if b := serviceBackup([]byte(got)); b != "without-uploads" {
		t.Errorf("serviceBackup = %q", b)
	}
	c := AutoUpgrade{Scheduler: Cron, Project: "fluxer", Dir: "/opt/fluxer", Binary: "/usr/local/bin/rexctl", Backup: "without-uploads", Log: "/opt/fluxer/.rexctl/auto-upgrade.log"}
	line := c.cron(-1, 4, 0, "")
	if !strings.Contains(line, "upgrade --yes --backup without-uploads --dir '/opt/fluxer'") || serviceBackup([]byte(line)) != "without-uploads" {
		t.Errorf("lean cron line:\n%s", line)
	}
}

func TestServiceWithoutBackupFlagReadsFull(t *testing.T) {
	a := AutoUpgrade{Scheduler: Systemd, Project: "fluxer", Dir: "/opt/fluxer", Binary: "/usr/local/bin/rexctl", At: "04:00"}
	if b := serviceBackup([]byte(a.service(""))); b != BackupFull {
		t.Errorf("serviceBackup = %q, want full", b)
	}
	if b := serviceBackup(nil); b != BackupFull {
		t.Errorf("serviceBackup of nothing = %q, want full", b)
	}
}

func TestWeekday(t *testing.T) {
	for in, want := range map[string]int{"sun": 0, "Mon": 1, "TUESDAY": 2, "wednesday": 3, "Sat": 6} {
		if got, ok := Weekday(in); !ok || got != want {
			t.Errorf("Weekday(%q) = %d, %v, want %d", in, got, ok, want)
		}
	}
	for _, in := range []string{"", "su", "mond", "funday", "7"} {
		if _, ok := Weekday(in); ok {
			t.Errorf("Weekday(%q) accepted", in)
		}
	}
}

func TestParseShow(t *testing.T) {
	m := parseShow([]byte("NextElapseUSecRealtime=Wed 2026-10-28 04:12:09 CET\nLastTriggerUSec=n/a\nExecMainStatus=0\nExecMainExitTimestamp=\n"))
	if m["NextElapseUSecRealtime"] != "Wed 2026-10-28 04:12:09 CET" || m["LastTriggerUSec"] != "" || m["ExecMainStatus"] != "0" || m["ExecMainExitTimestamp"] != "" {
		t.Errorf("parseShow = %v", m)
	}
}

func TestRequestedUndoesCronOffset(t *testing.T) {
	for _, sched := range []Scheduler{Cron, Systemd} {
		for _, c := range []struct{ day, at string }{{"", "04:00"}, {"", "23:50"}, {"Sun", "23:50"}, {"Sat", "23:59"}, {"Mon", "00:00"}} {
			a := AutoUpgrade{Scheduler: sched, Project: "fluxer", Domain: "chat.example.com", Dir: "/opt/fluxer", Day: c.day, At: c.at}
			day, at, err := a.Start()
			if err != nil {
				t.Fatal(err)
			}
			if sched == Systemd {
				day = c.day
			}
			gotDay, gotAt := a.Requested(day, at)
			if gotDay != c.day || gotAt != c.at {
				t.Errorf("%s %q %s started %q %s and reads back as %q %s", sched, c.day, c.at, day, at, gotDay, gotAt)
			}
		}
	}
}

func TestMonthlyRunPassedSurvivesReboot(t *testing.T) {
	stockholm, err := time.LoadLocation("Europe/Stockholm")
	if err != nil {
		t.Skip(err)
	}
	turnedOn := time.Date(2026, 10, 20, 12, 0, 0, 0, stockholm)
	for _, c := range []struct {
		now  time.Time
		want bool
	}{
		{time.Date(2026, 10, 30, 9, 0, 0, 0, stockholm), false},
		{time.Date(2026, 11, 1, 0, 29, 0, 0, stockholm), false},
		{time.Date(2026, 11, 1, 0, 30, 0, 0, stockholm), true},
		{time.Date(2026, 12, 15, 9, 0, 0, 0, stockholm), true},
	} {
		if got := monthlyRunPassed(turnedOn, c.now, "00:30"); got != c.want {
			t.Errorf("turned on %s, at %s the monthly backup has run = %v, want %v", turnedOn, c.now, got, c.want)
		}
	}
	if monthlyRunPassed(time.Time{}, time.Date(2026, 12, 15, 9, 0, 0, 0, stockholm), "00:30") || monthlyRunPassed(turnedOn, time.Date(2026, 12, 15, 9, 0, 0, 0, stockholm), "") {
		t.Error("a monthly backup with no known start or time reads as run")
	}
}

const fakeSystemctl = `#!/bin/sh
echo "$*" >> "$SYSTEMCTL_LOG"
case "$2" in
enable) echo 1 > "$SYSTEMCTL_STATE.enabled" ;;
restart) echo 1 > "$SYSTEMCTL_STATE.active" ;;
disable) : > "$SYSTEMCTL_STATE.enabled"; : > "$SYSTEMCTL_STATE.active" ;;
is-enabled) v=""; read -r v < "$SYSTEMCTL_STATE.enabled"; [ "$v" = 1 ] && echo enabled ;;
is-active) v=""; read -r v < "$SYSTEMCTL_STATE.active"; [ "$v" = 1 ] && echo active ;;
esac
exit 0
`

func TestAutoUpgradeInstallAndRemove(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("needs a shell to fake systemctl")
	}
	root := t.TempDir()
	bin := filepath.Join(root, "bin")
	if err := os.MkdirAll(bin, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(bin, "systemctl"), []byte(fakeSystemctl), 0o755); err != nil {
		t.Fatal(err)
	}
	state := filepath.Join(root, "unit")
	for _, s := range []string{".enabled", ".active"} {
		if err := os.WriteFile(state+s, nil, 0o600); err != nil {
			t.Fatal(err)
		}
	}
	log := filepath.Join(root, "systemctl.log")
	t.Setenv("PATH", bin)
	t.Setenv("XDG_CONFIG_HOME", filepath.Join(root, "config"))
	t.Setenv("SYSTEMCTL_LOG", log)
	t.Setenv("SYSTEMCTL_STATE", state)
	calls := func() []string {
		data, _ := os.ReadFile(log)
		_ = os.Remove(log)
		return strings.Fields(strings.ReplaceAll(strings.TrimSpace(string(data)), " ", "_"))
	}
	ctx := context.Background()
	a := AutoUpgrade{Scheduler: Systemd, Project: "fluxer", Dir: filepath.Join(root, "fluxer"), Binary: filepath.Join(bin, "rexctl"), Rootless: true, At: "04:00"}
	if err := InstallAutoUpgrade(ctx, a); err != nil {
		t.Fatal(err)
	}
	want := []string{"--user_daemon-reload", "--user_is-enabled_fluxer-upgrade.timer", "--user_enable_fluxer-upgrade.timer", "--user_restart_fluxer-upgrade.timer"}
	if got := calls(); strings.Join(got, " ") != strings.Join(want, " ") {
		t.Errorf("first install ran %v, want %v", got, want)
	}
	unitDir := filepath.Join(root, "config", "systemd", "user")
	for _, name := range []string{"fluxer-upgrade.service", "fluxer-upgrade.timer"} {
		if _, err := os.Stat(filepath.Join(unitDir, name)); err != nil {
			t.Errorf("%s wasn't written: %v", name, err)
		}
	}
	if err := InstallAutoUpgrade(ctx, a); err != nil {
		t.Fatal(err)
	}
	want = []string{"--user_is-enabled_fluxer-upgrade.timer", "--user_is-active_fluxer-upgrade.timer"}
	if got := calls(); strings.Join(got, " ") != strings.Join(want, " ") {
		t.Errorf("second install ran %v, want only %v", got, want)
	}
	a.At = "05:30"
	if err := InstallAutoUpgrade(ctx, a); err != nil {
		t.Fatal(err)
	}
	want = []string{"--user_daemon-reload", "--user_is-enabled_fluxer-upgrade.timer", "--user_restart_fluxer-upgrade.timer"}
	if got := calls(); strings.Join(got, " ") != strings.Join(want, " ") {
		t.Errorf("changing the time ran %v, want %v", got, want)
	}
	timer, err := os.ReadFile(filepath.Join(unitDir, "fluxer-upgrade.timer"))
	if _, at := timerSchedule(timer); err != nil || at != "05:30" {
		t.Errorf("the timer doesn't run at 05:30: %v", err)
	}
	removed, err := RemoveAutoUpgrade(ctx, a)
	if err != nil || !removed {
		t.Fatalf("RemoveAutoUpgrade = %v, %v", removed, err)
	}
	want = []string{"--user_is-enabled_fluxer-upgrade.timer", "--user_disable_--now_fluxer-upgrade.timer", "--user_daemon-reload"}
	if got := calls(); strings.Join(got, " ") != strings.Join(want, " ") {
		t.Errorf("remove ran %v, want %v", got, want)
	}
	entries, _ := os.ReadDir(unitDir)
	if len(entries) != 0 {
		t.Errorf("remove left %d files", len(entries))
	}
	removed, err = RemoveAutoUpgrade(ctx, a)
	if err != nil || removed {
		t.Errorf("second RemoveAutoUpgrade = %v, %v, want false, nil", removed, err)
	}
	if got := calls(); len(got) != 0 {
		t.Errorf("second remove ran %v", got)
	}
}

func TestFullBackupRunsHoursBeforeUpgrades(t *testing.T) {
	for _, tc := range []struct{ at, want string }{{"04:00", "01:00"}, {"02:45", "23:45"}, {"03:00", "00:00"}, {"03:30", "00:30"}, {"00:10", "21:10"}} {
		a := AutoUpgrade{Scheduler: Cron, Project: "fluxer", Domain: "chat.example.com", Dir: "/opt/fluxer", At: tc.at}
		got, err := a.BackupStart()
		if err != nil || got != tc.want {
			t.Errorf("--at %s backs up at %q (%v), want %s", tc.at, got, err, tc.want)
			continue
		}
		_, start, _ := a.Start()
		up, _ := time.Parse("15:04", start)
		back, _ := time.Parse("15:04", got)
		gap := up.Sub(back)
		if gap < 0 {
			gap += 24 * time.Hour
		}
		if gap < 3*time.Hour || gap+UpgradeWindow > 24*time.Hour-time.Hour {
			t.Errorf("--at %s upgrades at %s and backs up at %s, %v apart", tc.at, start, got, gap)
		}
	}
}

func TestFullBackupUnitsUseFullPaths(t *testing.T) {
	a := AutoUpgrade{Scheduler: Systemd, Project: "fluxer", Dir: "/opt/fluxer-missing", Binary: "/usr/local/bin/rexctl", Podman: true, At: "02:45", Log: "/opt/fluxer-missing/.rexctl/auto-upgrade.log"}
	files, err := a.backupFiles()
	if err != nil {
		t.Fatal(err)
	}
	wantService := `[Unit]
Description=Back up Fluxer in /opt/fluxer-missing
After=fluxer.service

[Service]
Type=oneshot
ExecStart=/usr/local/bin/rexctl backup --dir /opt/fluxer-missing
`
	wantTimer := `[Unit]
Description=Back up Fluxer in /opt/fluxer-missing once a month

[Timer]
OnCalendar=*-*-01 23:45:00
Persistent=true

[Install]
WantedBy=timers.target
`
	if len(files) != 2 || string(files[0]) != wantService || string(files[1]) != wantTimer {
		t.Errorf("systemd backup units:\n%s\n%s", files[0], files[1])
	}
	paths, err := a.BackupPaths()
	if err != nil || len(paths) != 2 || paths[0] != "/etc/systemd/system/fluxer-backup.service" || paths[1] != "/etc/systemd/system/fluxer-backup.timer" {
		t.Errorf("systemd backup paths = %v, %v", paths, err)
	}
	a.Scheduler = Cron
	files, err = a.backupFiles()
	if err != nil {
		t.Fatal(err)
	}
	wantCron := "SHELL=/bin/sh\nPATH=" + cronPath + "\n45 23 1 * * root '/usr/local/bin/rexctl' backup --dir '/opt/fluxer-missing' >'/opt/fluxer-missing/.rexctl/full-backup.log' 2>&1; echo \"rexctl exited with $?\" >>'/opt/fluxer-missing/.rexctl/full-backup.log'\n"
	if len(files) != 1 || string(files[0]) != wantCron {
		t.Errorf("cron backup file:\n%s\nwant:\n%s", files[0], wantCron)
	}
	if day, at := cronSchedule(files[0]); day != "" || at != "23:45" {
		t.Errorf("the cron backup file reads back as %q %q", day, at)
	}
	a.Domain = "chat.example.com"
	if _, start, _ := a.Start(); start == "02:45" {
		t.Fatal("the test needs a cron offset")
	}
	if files, _ := a.backupFiles(); !strings.Contains(string(files[0]), "\n45 23 1 * * ") {
		t.Errorf("the cron offset moved the backup:\n%s", files[0])
	}
	if paths, _ := a.BackupPaths(); len(paths) != 1 || paths[0] != "/etc/cron.d/fluxer-backup" {
		t.Errorf("cron backup paths = %v", paths)
	}
}

func fakeSystemd(t *testing.T) (string, func() []string) {
	t.Helper()
	if runtime.GOOS == "windows" {
		t.Skip("needs a shell to fake systemctl")
	}
	root := t.TempDir()
	bin := filepath.Join(root, "bin")
	if err := os.MkdirAll(bin, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(bin, "systemctl"), []byte(fakeSystemctl), 0o755); err != nil {
		t.Fatal(err)
	}
	state := filepath.Join(root, "unit")
	for _, s := range []string{".enabled", ".active"} {
		if err := os.WriteFile(state+s, nil, 0o600); err != nil {
			t.Fatal(err)
		}
	}
	log := filepath.Join(root, "systemctl.log")
	t.Setenv("PATH", bin)
	t.Setenv("XDG_CONFIG_HOME", filepath.Join(root, "config"))
	t.Setenv("SYSTEMCTL_LOG", log)
	t.Setenv("SYSTEMCTL_STATE", state)
	return root, func() []string {
		data, _ := os.ReadFile(log)
		_ = os.Remove(log)
		return strings.Fields(strings.ReplaceAll(strings.TrimSpace(string(data)), " ", "_"))
	}
}

func TestFullBackupTimerInstallAndRemove(t *testing.T) {
	root, calls := fakeSystemd(t)
	ctx := context.Background()
	a := AutoUpgrade{Scheduler: Systemd, Project: "fluxer", Dir: filepath.Join(root, "fluxer"), Binary: filepath.Join(root, "bin", "rexctl"), Rootless: true, At: "04:00", FullBackup: FullBackupMonthly, Log: filepath.Join(root, "fluxer", ".rexctl", "auto-upgrade.log")}
	if err := InstallAutoUpgrade(ctx, a); err != nil {
		t.Fatal(err)
	}
	got := strings.Join(calls(), " ")
	if !strings.Contains(got, "--user_restart_fluxer-backup.timer") || !strings.Contains(got, "--user_restart_fluxer-upgrade.timer") {
		t.Errorf("installing with a monthly full backup ran %s", got)
	}
	unitDir := filepath.Join(root, "config", "systemd", "user")
	timer, err := os.ReadFile(filepath.Join(unitDir, "fluxer-backup.timer"))
	if err != nil || !strings.Contains(string(timer), "OnCalendar=*-*-01 01:00:00\n") {
		t.Errorf("backup timer = %q, %v", timer, err)
	}
	s, err := CheckAutoUpgrade(ctx, a, time.Now())
	if err != nil || s.FullBackup != FullBackupMonthly || s.FullBackupAt != "01:00" || s.FullBackupRan {
		t.Errorf("status = %+v, %v, want a monthly full backup at 01:00 that hasn't run", s, err)
	}
	if !strings.Contains(string(timer), "Persistent=true\n") {
		t.Errorf("a monthly backup missed while the machine is off isn't made later, timer %q", timer)
	}
	installed := time.Now().AddDate(0, -2, 0)
	if err := os.Chtimes(filepath.Join(unitDir, "fluxer-backup.timer"), installed, installed); err != nil {
		t.Fatal(err)
	}
	if s, err := CheckAutoUpgrade(ctx, a, time.Now()); err != nil || !s.FullBackupRan {
		t.Errorf("status = %+v, %v, want a monthly full backup that was due since it was turned on", s, err)
	}
	a.FullBackup = ""
	if err := InstallAutoUpgrade(ctx, a); err != nil {
		t.Fatal(err)
	}
	if got := strings.Join(calls(), " "); !strings.Contains(got, "--user_disable_--now_fluxer-backup.timer") {
		t.Errorf("installing without a full backup ran %s", got)
	}
	for _, name := range []string{"fluxer-backup.service", "fluxer-backup.timer"} {
		if _, err := os.Stat(filepath.Join(unitDir, name)); !os.IsNotExist(err) {
			t.Errorf("%s is still there (%v)", name, err)
		}
	}
	if s, _ := CheckAutoUpgrade(ctx, a, time.Now()); s.FullBackup != "" {
		t.Errorf("status still shows a full backup %q", s.FullBackup)
	}
	a.FullBackup = FullBackupMonthly
	if err := InstallAutoUpgrade(ctx, a); err != nil {
		t.Fatal(err)
	}
	calls()
	removed, err := RemoveAutoUpgrade(ctx, a)
	if err != nil || !removed {
		t.Fatalf("RemoveAutoUpgrade = %v, %v", removed, err)
	}
	if got := strings.Join(calls(), " "); !strings.Contains(got, "fluxer-backup.timer") || !strings.Contains(got, "fluxer-upgrade.timer") {
		t.Errorf("remove ran %s", got)
	}
	if entries, _ := os.ReadDir(unitDir); len(entries) != 0 {
		t.Errorf("remove left %d files", len(entries))
	}
}

func TestDropBackupFlagKeepsTheSchedule(t *testing.T) {
	root, calls := fakeSystemd(t)
	ctx := context.Background()
	a := AutoUpgrade{Scheduler: Systemd, Project: "fluxer", Dir: filepath.Join(root, "fluxer"), Binary: filepath.Join(root, "bin", "rexctl"), Rootless: true, At: "02:45", Day: "Sun", Backup: "without-uploads"}
	if err := InstallAutoUpgrade(ctx, a); err != nil {
		t.Fatal(err)
	}
	unitDir := filepath.Join(root, "config", "systemd", "user")
	timerBefore, _ := os.ReadFile(filepath.Join(unitDir, "fluxer-upgrade.timer"))
	calls()
	changed, err := DropBackupFlag(ctx, a)
	if err != nil || !changed {
		t.Fatalf("DropBackupFlag = %v, %v", changed, err)
	}
	service, _ := os.ReadFile(filepath.Join(unitDir, "fluxer-upgrade.service"))
	full := a
	full.Backup = BackupFull
	if string(service) != full.service("") {
		t.Errorf("service after dropping --backup:\n%s\nwant:\n%s", service, full.service(""))
	}
	if timer, _ := os.ReadFile(filepath.Join(unitDir, "fluxer-upgrade.timer")); string(timer) != string(timerBefore) {
		t.Errorf("dropping --backup changed the timer to\n%s", timer)
	}
	if got := calls(); strings.Join(got, " ") != "--user_daemon-reload" {
		t.Errorf("dropping --backup ran %v", got)
	}
	if changed, err := DropBackupFlag(ctx, a); err != nil || changed {
		t.Errorf("second DropBackupFlag = %v, %v, want false, nil", changed, err)
	}

	cron := AutoUpgrade{Scheduler: Cron, Project: "fluxer", Domain: "chat.example.com", Dir: "/opt/fluxer", Binary: "/usr/local/bin/rexctl", At: "23:50", Day: "Sun", Backup: "without-uploads", Log: "/opt/fluxer/.rexctl/auto-upgrade.log"}
	day, hour, minute, err := cron.start()
	if err != nil {
		t.Fatal(err)
	}
	lean := cron.cron(day, hour, minute, "root")
	cron.Backup = BackupFull
	if got := strings.ReplaceAll(lean, " upgrade --yes --backup without-uploads ", " upgrade --yes "); got != cron.cron(day, hour, minute, "root") {
		t.Errorf("the cron line without --backup is\n%s\nwant\n%s", got, cron.cron(day, hour, minute, "root"))
	}
}
