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
	got := a.timer(hour, minute)
	if got != want {
		t.Errorf("timer:\n%s\nwant:\n%s", got, want)
	}
	if at := timerAt([]byte(got)); at != "23:05" {
		t.Errorf("timerAt = %q, want 23:05", at)
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
	got := a.cron(4, 7, "")
	if got != want {
		t.Errorf("cron:\n%s\nwant:\n%s", got, want)
	}
	if at := cronAt([]byte(got)); at != "04:07" {
		t.Errorf("cronAt = %q, want 04:07", at)
	}
	if !strings.Contains(a.cron(4, 7, "alice"), " * * * alice ") {
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
	start, err := a.Start()
	if err != nil {
		t.Fatal(err)
	}
	if again, _ := a.Start(); again != start {
		t.Errorf("Start() = %q then %q, want the same time every time", start, again)
	}
	if start < "04:00" || start >= "04:30" {
		t.Errorf("Start() = %q, want a time from 04:00 to 04:29", start)
	}
	files, err := a.files()
	if err != nil || cronAt(files[0]) != start {
		t.Errorf("cron file runs at %q, want %q (%v)", cronAt(files[0]), start, err)
	}
	spread := func(vary func(*AutoUpgrade, int)) int {
		starts := map[string]bool{}
		for i := range 8 {
			b := a
			vary(&b, i)
			s, _ := b.Start()
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
	if s, _ := late.Start(); s >= "00:29" && s < "23:59" {
		t.Errorf("Start() for 23:59 = %q, want it to wrap past midnight", s)
	}
	sd := a
	sd.Scheduler = Systemd
	if s, _ := sd.Start(); s != "04:00" {
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

func TestNextDaily(t *testing.T) {
	loc := time.FixedZone("CET", 3600)
	now := time.Date(2026, 10, 27, 5, 0, 0, 0, loc)
	if got := nextDaily(now, 4, 0); !got.Equal(time.Date(2026, 10, 28, 4, 0, 0, 0, loc)) {
		t.Errorf("after the time: %v", got)
	}
	if got := nextDaily(now, 6, 30); !got.Equal(time.Date(2026, 10, 27, 6, 30, 0, 0, loc)) {
		t.Errorf("before the time: %v", got)
	}
}

func TestParseShow(t *testing.T) {
	m := parseShow([]byte("NextElapseUSecRealtime=Wed 2026-10-28 04:12:09 CET\nLastTriggerUSec=n/a\nExecMainStatus=0\nExecMainExitTimestamp=\n"))
	if m["NextElapseUSecRealtime"] != "Wed 2026-10-28 04:12:09 CET" || m["LastTriggerUSec"] != "" || m["ExecMainStatus"] != "0" || m["ExecMainExitTimestamp"] != "" {
		t.Errorf("parseShow = %v", m)
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
	if err != nil || timerAt(timer) != "05:30" {
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
