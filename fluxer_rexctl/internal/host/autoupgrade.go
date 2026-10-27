// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"bufio"
	"bytes"
	"context"
	"fmt"
	"hash/fnv"
	"os"
	"os/exec"
	"os/user"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
	"time"
)

type Scheduler string

const (
	Systemd Scheduler = "systemd"
	Cron    Scheduler = "cron"
)

const (
	cronDir           = "/etc/cron.d"
	cronPath          = "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
	cronExitPrefix    = "rexctl exited with "
	systemdTimeLayout = "Mon 2006-01-02 15:04:05 MST"
)

const UpgradeWindow = 30 * time.Minute

func UpgradeScheduler() Scheduler {
	if BootUnitSupported() {
		return Systemd
	}
	if runtime.GOOS != "linux" {
		return ""
	}
	if info, err := os.Stat(cronDir); err == nil && info.IsDir() {
		return Cron
	}
	return ""
}

type AutoUpgrade struct {
	Scheduler Scheduler
	Project   string
	Domain    string
	Dir       string
	Binary    string
	Podman    bool
	Rootless  bool
	At        string
	Log       string
}

type AutoUpgradeState struct {
	On       bool
	At       string
	Next     string
	Last     string
	Ran      bool
	ExitCode int
	Running  bool
}

func (a AutoUpgrade) name() string { return a.Project + "-upgrade" }

func (a AutoUpgrade) ServiceName() string { return a.name() + ".service" }

func (a AutoUpgrade) TimerName() string { return a.name() + ".timer" }

func (a AutoUpgrade) Paths() ([]string, error) {
	if a.Scheduler == Cron {
		return []string{filepath.Join(cronDir, a.name())}, nil
	}
	dir, err := unitDir(a.Rootless)
	if err != nil {
		return nil, err
	}
	return []string{filepath.Join(dir, a.ServiceName()), filepath.Join(dir, a.TimerName())}, nil
}

func (a AutoUpgrade) runAs() string {
	if a.Rootless {
		if u, err := user.Current(); err == nil {
			return u.Username
		}
		return strconv.Itoa(os.Geteuid())
	}
	if a.Podman {
		return ""
	}
	return dirOwner(a.Dir)
}

func (a AutoUpgrade) clock() (int, int, error) {
	t, err := time.Parse("15:04", a.At)
	if err != nil {
		return 0, 0, fmt.Errorf("host: %q is not a time of day", a.At)
	}
	return t.Hour(), t.Minute(), nil
}

func (a AutoUpgrade) start() (int, int, error) {
	hour, minute, err := a.clock()
	if err != nil || a.Scheduler != Cron {
		return hour, minute, err
	}
	h := fnv.New32a()
	h.Write([]byte(a.Domain + "\x00" + a.Project + "\x00" + a.Dir))
	m := (hour*60 + minute + int(h.Sum32()%uint32(UpgradeWindow.Minutes()))) % (24 * 60)
	return m / 60, m % 60, nil
}

func (a AutoUpgrade) Start() (string, error) {
	hour, minute, err := a.start()
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%02d:%02d", hour, minute), nil
}

func (a AutoUpgrade) service(runAs string) string {
	after := []string{a.Project + ".service"}
	var b strings.Builder
	b.WriteString("[Unit]\n")
	fmt.Fprintf(&b, "Description=Upgrade Fluxer in %s\n", strings.ReplaceAll(a.Dir, "%", "%%"))
	if !a.Rootless {
		after = append([]string{"network-online.target"}, after...)
		b.WriteString("Wants=network-online.target\n")
	}
	fmt.Fprintf(&b, "After=%s\n", strings.Join(after, " "))
	b.WriteString("\n[Service]\n")
	b.WriteString("Type=oneshot\n")
	if runAs != "" {
		fmt.Fprintf(&b, "User=%s\n", runAs)
	}
	fmt.Fprintf(&b, "ExecStart=%s upgrade --yes --dir %s\n", systemdQuote(a.Binary), systemdQuote(a.Dir))
	if runAs != "" {
		fmt.Fprintf(&b, "ExecStopPost=-+/usr/bin/install -D -m 0755 %s %s\n", systemdQuote(a.Binary), LibexecBinary)
	}
	return b.String()
}

func (a AutoUpgrade) timer(hour, minute int) string {
	var b strings.Builder
	b.WriteString("[Unit]\n")
	fmt.Fprintf(&b, "Description=Upgrade Fluxer in %s every day\n", strings.ReplaceAll(a.Dir, "%", "%%"))
	b.WriteString("\n[Timer]\n")
	fmt.Fprintf(&b, "OnCalendar=*-*-* %02d:%02d:00\n", hour, minute)
	fmt.Fprintf(&b, "RandomizedDelaySec=%dmin\n", int(UpgradeWindow.Minutes()))
	b.WriteString("Persistent=true\n")
	b.WriteString("\n[Install]\n")
	b.WriteString("WantedBy=timers.target\n")
	return b.String()
}

func (a AutoUpgrade) cron(hour, minute int, runAs string) string {
	if runAs == "" {
		runAs = "root"
	}
	log := cronQuote(a.Log)
	var b strings.Builder
	b.WriteString("SHELL=/bin/sh\n")
	fmt.Fprintf(&b, "PATH=%s\n", cronPath)
	fmt.Fprintf(&b, "%d %d * * * %s %s upgrade --yes --dir %s >%s 2>&1; echo \"%s$?\" >>%s\n", minute, hour, runAs, cronQuote(a.Binary), cronQuote(a.Dir), log, cronExitPrefix, log)
	return b.String()
}

func cronQuote(s string) string {
	s = "'" + strings.ReplaceAll(s, "'", `'\''`) + "'"
	return strings.ReplaceAll(s, "%", `\%`)
}

func (a AutoUpgrade) files() ([][]byte, error) {
	if !filepath.IsAbs(a.Binary) || !filepath.IsAbs(a.Dir) {
		return nil, fmt.Errorf("host: automatic upgrades need absolute paths, got %q and %q", a.Binary, a.Dir)
	}
	hour, minute, err := a.start()
	if err != nil {
		return nil, err
	}
	runAs := a.runAs()
	switch a.Scheduler {
	case Systemd:
		if a.Rootless {
			runAs = ""
		}
		return [][]byte{[]byte(a.service(runAs)), []byte(a.timer(hour, minute))}, nil
	case Cron:
		if !filepath.IsAbs(a.Log) {
			return nil, fmt.Errorf("host: automatic upgrades need an absolute log path, got %q", a.Log)
		}
		return [][]byte{[]byte(a.cron(hour, minute, runAs))}, nil
	}
	return nil, fmt.Errorf("host: no scheduler for automatic upgrades")
}

func InstallAutoUpgrade(ctx context.Context, a AutoUpgrade) error {
	data, err := a.files()
	if err != nil {
		return err
	}
	paths, err := a.Paths()
	if err != nil {
		return err
	}
	userScope := a.Scheduler == Systemd && a.Rootless
	changed := false
	for i, p := range paths {
		c, err := writeHostFile(ctx, userScope, p, data[i])
		if err != nil {
			return err
		}
		changed = changed || c
	}
	if a.Scheduler != Systemd {
		return nil
	}
	if changed {
		if err := systemctlRun(ctx, a.Rootless, "daemon-reload"); err != nil {
			return err
		}
	}
	if !unitIs(ctx, a.Rootless, a.TimerName(), "enabled") {
		if err := systemctlRun(ctx, a.Rootless, "enable", a.TimerName()); err != nil {
			return err
		}
	}
	if changed || !unitIs(ctx, a.Rootless, a.TimerName(), "active") {
		if err := systemctlRun(ctx, a.Rootless, "restart", a.TimerName()); err != nil {
			return err
		}
	}
	if a.Rootless && !lingering(ctx) {
		return enableLinger(ctx)
	}
	return nil
}

func RemoveAutoUpgrade(ctx context.Context, a AutoUpgrade) (bool, error) {
	paths, err := a.Paths()
	if err != nil {
		return false, err
	}
	var found []string
	for _, p := range paths {
		if _, err := os.Stat(p); err == nil {
			found = append(found, p)
		}
	}
	if len(found) == 0 {
		return false, nil
	}
	if a.Scheduler == Cron {
		return true, removeHostFiles(ctx, false, found...)
	}
	if unitIs(ctx, a.Rootless, a.TimerName(), "enabled") || unitIs(ctx, a.Rootless, a.TimerName(), "active") {
		if err := systemctlRun(ctx, a.Rootless, "disable", "--now", a.TimerName()); err != nil {
			return false, err
		}
	}
	if err := removeHostFiles(ctx, a.Rootless, found...); err != nil {
		return false, err
	}
	return true, systemctlRun(ctx, a.Rootless, "daemon-reload")
}

func CheckAutoUpgrade(ctx context.Context, a AutoUpgrade, now time.Time) (AutoUpgradeState, error) {
	paths, err := a.Paths()
	if err != nil {
		return AutoUpgradeState{}, err
	}
	data, err := os.ReadFile(paths[len(paths)-1])
	if os.IsNotExist(err) {
		return AutoUpgradeState{}, nil
	}
	if err != nil {
		return AutoUpgradeState{}, err
	}
	s := AutoUpgradeState{On: true}
	if a.Scheduler == Cron {
		s.At = cronAt(data)
		if hour, minute, err := (AutoUpgrade{At: s.At}).clock(); err == nil {
			s.Next = nextDaily(now, hour, minute).Format(systemdTimeLayout)
		}
		if info, err := os.Stat(a.Log); err == nil {
			s.Last = info.ModTime().Format(systemdTimeLayout)
			s.Ran, s.ExitCode = cronExit(a.Log)
		}
		return s, nil
	}
	s.At = timerAt(data)
	t := systemctlShow(ctx, a.Rootless, a.TimerName(), "NextElapseUSecRealtime", "LastTriggerUSec")
	s.Next, s.Last = t["NextElapseUSecRealtime"], t["LastTriggerUSec"]
	svc := systemctlShow(ctx, a.Rootless, a.ServiceName(), "ActiveState", "ExecMainExitTimestamp", "ExecMainStatus")
	s.Running = svc["ActiveState"] == "activating"
	if svc["ExecMainExitTimestamp"] != "" {
		s.Ran = true
		s.ExitCode, _ = strconv.Atoi(svc["ExecMainStatus"])
	}
	return s, nil
}

func systemctlShow(ctx context.Context, rootless bool, unit string, props ...string) map[string]string {
	args := []string{"show", unit}
	if rootless {
		args = append([]string{"--user"}, args...)
	}
	for _, p := range props {
		args = append(args, "-p", p)
	}
	out, _ := exec.CommandContext(ctx, "systemctl", args...).Output()
	return parseShow(out)
}

func parseShow(out []byte) map[string]string {
	m := map[string]string{}
	s := bufio.NewScanner(bytes.NewReader(out))
	for s.Scan() {
		k, v, ok := strings.Cut(s.Text(), "=")
		if !ok {
			continue
		}
		v = strings.TrimSpace(v)
		if v == "n/a" || v == "0" && k != "ExecMainStatus" {
			v = ""
		}
		m[k] = v
	}
	return m
}

func timerAt(data []byte) string {
	for _, line := range strings.Split(string(data), "\n") {
		if v, ok := strings.CutPrefix(line, "OnCalendar=*-*-* "); ok && len(v) >= 5 {
			return v[:5]
		}
	}
	return ""
}

func cronAt(data []byte) string {
	for _, line := range strings.Split(string(data), "\n") {
		f := strings.Fields(line)
		if len(f) < 2 || strings.Contains(f[0], "=") {
			continue
		}
		minute, merr := strconv.Atoi(f[0])
		hour, herr := strconv.Atoi(f[1])
		if merr == nil && herr == nil {
			return fmt.Sprintf("%02d:%02d", hour, minute)
		}
	}
	return ""
}

func cronExit(log string) (bool, int) {
	data, err := os.ReadFile(log)
	if err != nil {
		return false, 0
	}
	lines := strings.Split(strings.TrimSpace(string(data)), "\n")
	v, ok := strings.CutPrefix(lines[len(lines)-1], cronExitPrefix)
	if !ok {
		return false, 0
	}
	code, err := strconv.Atoi(strings.TrimSpace(v))
	return err == nil, code
}

func nextDaily(now time.Time, hour, minute int) time.Time {
	next := time.Date(now.Year(), now.Month(), now.Day(), hour, minute, 0, 0, now.Location())
	if !next.After(now) {
		next = next.AddDate(0, 0, 1)
	}
	return next
}
