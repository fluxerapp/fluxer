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

const BackupFull = "full"

const FullBackupMonthly = "monthly"

const fullBackupLead = 3 * 60

const fullBackupLog = "full-backup.log"

const (
	minutesPerDay  = 24 * 60
	minutesPerWeek = 7 * minutesPerDay
)

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
	Scheduler  Scheduler
	Project    string
	Domain     string
	Dir        string
	Binary     string
	Podman     bool
	Rootless   bool
	At         string
	Day        string
	Backup     string
	FullBackup string
	Log        string
}

type AutoUpgradeState struct {
	On            bool
	At            string
	Day           string
	Backup        string
	FullBackup    string
	FullBackupRan bool
	FullBackupAt  string
	Next          string
	Last          string
	Ran           bool
	ExitCode      int
	Running       bool
}

func (a AutoUpgrade) name() string { return a.Project + "-upgrade" }

func (a AutoUpgrade) ServiceName() string { return a.name() + ".service" }

func (a AutoUpgrade) TimerName() string { return a.name() + ".timer" }

func (a AutoUpgrade) backupName() string { return a.Project + "-backup" }

func (a AutoUpgrade) BackupServiceName() string { return a.backupName() + ".service" }

func (a AutoUpgrade) BackupTimerName() string { return a.backupName() + ".timer" }

func (a AutoUpgrade) BackupLog() string { return filepath.Join(filepath.Dir(a.Log), fullBackupLog) }

func (a AutoUpgrade) BackupPaths() ([]string, error) {
	if a.Scheduler == Cron {
		return []string{filepath.Join(cronDir, a.backupName())}, nil
	}
	dir, err := unitDir(a.Rootless)
	if err != nil {
		return nil, err
	}
	return []string{filepath.Join(dir, a.BackupServiceName()), filepath.Join(dir, a.BackupTimerName())}, nil
}

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

func (a AutoUpgrade) weekday() (int, error) {
	if a.Day == "" {
		return -1, nil
	}
	if d, ok := Weekday(a.Day); ok {
		return d, nil
	}
	return 0, fmt.Errorf("host: %q is not a day of the week", a.Day)
}

func (a AutoUpgrade) start() (int, int, int, error) {
	hour, minute, err := a.clock()
	if err != nil {
		return 0, 0, 0, err
	}
	day, err := a.weekday()
	if err != nil || a.Scheduler != Cron {
		return day, hour, minute, err
	}
	offset := a.cronOffset()
	if day < 0 {
		m := (hour*60 + minute + offset) % minutesPerDay
		return day, m / 60, m % 60, nil
	}
	m := (day*minutesPerDay + hour*60 + minute + offset) % minutesPerWeek
	return m / minutesPerDay, m % minutesPerDay / 60, m % 60, nil
}

func (a AutoUpgrade) cronOffset() int {
	h := fnv.New32a()
	h.Write([]byte(a.Domain + "\x00" + a.Project + "\x00" + a.Dir))
	return int(h.Sum32() % uint32(UpgradeWindow.Minutes()))
}

func (a AutoUpgrade) Requested(day, at string) (string, string) {
	if a.Scheduler != Cron {
		return day, at
	}
	t, err := time.Parse("15:04", at)
	if err != nil {
		return day, at
	}
	m := t.Hour()*60 + t.Minute() - a.cronOffset()
	d, weekly := Weekday(day)
	if !weekly {
		m = (m + minutesPerDay) % minutesPerDay
		return "", fmt.Sprintf("%02d:%02d", m/60, m%60)
	}
	m = (d*minutesPerDay + m + minutesPerWeek) % minutesPerWeek
	return dayName(m / minutesPerDay), fmt.Sprintf("%02d:%02d", m%minutesPerDay/60, m%60)
}

func (a AutoUpgrade) Start() (string, string, error) {
	day, hour, minute, err := a.start()
	if err != nil {
		return "", "", err
	}
	return dayName(day), fmt.Sprintf("%02d:%02d", hour, minute), nil
}

func Weekday(s string) (int, bool) {
	s = strings.ToLower(strings.TrimSpace(s))
	for d := time.Sunday; d <= time.Saturday; d++ {
		full := strings.ToLower(d.String())
		if s != "" && (s == full || s == full[:3]) {
			return int(d), true
		}
	}
	return 0, false
}

func dayName(day int) string {
	if day < 0 || day > 6 {
		return ""
	}
	return time.Weekday(day).String()[:3]
}

func DayLong(day string) string {
	d, ok := Weekday(day)
	if !ok {
		return ""
	}
	return time.Weekday(d).String()
}

func every(day int) string {
	if day < 0 {
		return "every day"
	}
	return "every " + time.Weekday(day).String()
}

func (a AutoUpgrade) upgradeArgs() string {
	if a.Backup == "" || a.Backup == BackupFull {
		return "upgrade --yes"
	}
	return "upgrade --yes --backup " + a.Backup
}

func (a AutoUpgrade) service(runAs string) string {
	after := []string{a.Project + ".service"}
	if a.FullBackup != "" {
		after = append(after, a.BackupServiceName())
	}
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
	fmt.Fprintf(&b, "ExecStart=%s %s --dir %s\n", systemdQuote(a.Binary), a.upgradeArgs(), systemdQuote(a.Dir))
	if runAs != "" {
		fmt.Fprintf(&b, "ExecStopPost=-+/usr/bin/install -D -m 0755 %s %s\n", systemdQuote(a.Binary), LibexecBinary)
	}
	return b.String()
}

func (a AutoUpgrade) timer(day, hour, minute int) string {
	var b strings.Builder
	b.WriteString("[Unit]\n")
	fmt.Fprintf(&b, "Description=Upgrade Fluxer in %s %s\n", strings.ReplaceAll(a.Dir, "%", "%%"), every(day))
	b.WriteString("\n[Timer]\n")
	b.WriteString("OnCalendar=")
	if day >= 0 {
		b.WriteString(dayName(day) + " ")
	}
	fmt.Fprintf(&b, "*-*-* %02d:%02d:00\n", hour, minute)
	fmt.Fprintf(&b, "RandomizedDelaySec=%dmin\n", int(UpgradeWindow.Minutes()))
	b.WriteString("Persistent=true\n")
	b.WriteString("\n[Install]\n")
	b.WriteString("WantedBy=timers.target\n")
	return b.String()
}

func (a AutoUpgrade) cron(day, hour, minute int, runAs string) string {
	if runAs == "" {
		runAs = "root"
	}
	dow := "*"
	if day >= 0 {
		dow = strconv.Itoa(day)
	}
	log := cronQuote(a.Log)
	var b strings.Builder
	b.WriteString("SHELL=/bin/sh\n")
	fmt.Fprintf(&b, "PATH=%s\n", cronPath)
	fmt.Fprintf(&b, "%d %d * * %s %s %s %s --dir %s >%s 2>&1; echo \"%s$?\" >>%s\n", minute, hour, dow, runAs, cronQuote(a.Binary), a.upgradeArgs(), cronQuote(a.Dir), log, cronExitPrefix, log)
	return b.String()
}

func (a AutoUpgrade) BackupStart() (string, error) {
	hour, minute, err := a.clock()
	if err != nil {
		return "", err
	}
	m := (hour*60 + minute + minutesPerDay - fullBackupLead) % minutesPerDay
	return fmt.Sprintf("%02d:%02d", m/60, m%60), nil
}

func (a AutoUpgrade) backupService(runAs string) string {
	var b strings.Builder
	b.WriteString("[Unit]\n")
	fmt.Fprintf(&b, "Description=Back up Fluxer in %s\n", strings.ReplaceAll(a.Dir, "%", "%%"))
	fmt.Fprintf(&b, "After=%s.service\n", a.Project)
	b.WriteString("\n[Service]\n")
	b.WriteString("Type=oneshot\n")
	if runAs != "" {
		fmt.Fprintf(&b, "User=%s\n", runAs)
	}
	fmt.Fprintf(&b, "ExecStart=%s backup --dir %s\n", systemdQuote(a.Binary), systemdQuote(a.Dir))
	return b.String()
}

func (a AutoUpgrade) backupTimer(at string) string {
	var b strings.Builder
	b.WriteString("[Unit]\n")
	fmt.Fprintf(&b, "Description=Back up Fluxer in %s once a month\n", strings.ReplaceAll(a.Dir, "%", "%%"))
	b.WriteString("\n[Timer]\n")
	fmt.Fprintf(&b, "OnCalendar=*-*-01 %s:00\n", at)
	b.WriteString("Persistent=true\n")
	b.WriteString("\n[Install]\n")
	b.WriteString("WantedBy=timers.target\n")
	return b.String()
}

func (a AutoUpgrade) backupCron(at, runAs string) string {
	if runAs == "" {
		runAs = "root"
	}
	hour, minute, _ := AutoUpgrade{At: at}.clock()
	log := cronQuote(a.BackupLog())
	var b strings.Builder
	b.WriteString("SHELL=/bin/sh\n")
	fmt.Fprintf(&b, "PATH=%s\n", cronPath)
	fmt.Fprintf(&b, "%d %d 1 * * %s %s backup --dir %s >%s 2>&1; echo \"%s$?\" >>%s\n", minute, hour, runAs, cronQuote(a.Binary), cronQuote(a.Dir), log, cronExitPrefix, log)
	return b.String()
}

func (a AutoUpgrade) backupFiles() ([][]byte, error) {
	at, err := a.BackupStart()
	if err != nil {
		return nil, err
	}
	runAs := a.runAs()
	switch a.Scheduler {
	case Systemd:
		if a.Rootless {
			runAs = ""
		}
		return [][]byte{[]byte(a.backupService(runAs)), []byte(a.backupTimer(at))}, nil
	case Cron:
		return [][]byte{[]byte(a.backupCron(at, runAs))}, nil
	}
	return nil, fmt.Errorf("host: no scheduler for automatic upgrades")
}

func cronQuote(s string) string {
	s = "'" + strings.ReplaceAll(s, "'", `'\''`) + "'"
	return strings.ReplaceAll(s, "%", `\%`)
}

func (a AutoUpgrade) files() ([][]byte, error) {
	if !filepath.IsAbs(a.Binary) || !filepath.IsAbs(a.Dir) {
		return nil, fmt.Errorf("host: automatic upgrades need absolute paths, got %q and %q", a.Binary, a.Dir)
	}
	day, hour, minute, err := a.start()
	if err != nil {
		return nil, err
	}
	runAs := a.runAs()
	switch a.Scheduler {
	case Systemd:
		if a.Rootless {
			runAs = ""
		}
		return [][]byte{[]byte(a.service(runAs)), []byte(a.timer(day, hour, minute))}, nil
	case Cron:
		if !filepath.IsAbs(a.Log) {
			return nil, fmt.Errorf("host: automatic upgrades need an absolute log path, got %q", a.Log)
		}
		return [][]byte{[]byte(a.cron(day, hour, minute, runAs))}, nil
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
	if err := installUnits(ctx, a, paths, data, a.TimerName()); err != nil {
		return err
	}
	backupPaths, err := a.BackupPaths()
	if err != nil {
		return err
	}
	if a.FullBackup == "" {
		if _, err := removeUnits(ctx, a, backupPaths, a.BackupTimerName()); err != nil {
			return err
		}
	} else {
		backupData, err := a.backupFiles()
		if err != nil {
			return err
		}
		if err := installUnits(ctx, a, backupPaths, backupData, a.BackupTimerName()); err != nil {
			return err
		}
	}
	if a.Scheduler == Systemd && a.Rootless && !lingering(ctx) {
		return enableLinger(ctx)
	}
	return nil
}

func installUnits(ctx context.Context, a AutoUpgrade, paths []string, data [][]byte, timer string) error {
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
	if !unitIs(ctx, a.Rootless, timer, "enabled") {
		if err := systemctlRun(ctx, a.Rootless, "enable", timer); err != nil {
			return err
		}
	}
	if changed || !unitIs(ctx, a.Rootless, timer, "active") {
		if err := systemctlRun(ctx, a.Rootless, "restart", timer); err != nil {
			return err
		}
	}
	return nil
}

func RemoveAutoUpgrade(ctx context.Context, a AutoUpgrade) (bool, error) {
	paths, err := a.Paths()
	if err != nil {
		return false, err
	}
	backupPaths, err := a.BackupPaths()
	if err != nil {
		return false, err
	}
	removed, err := removeUnits(ctx, a, paths, a.TimerName())
	if err != nil {
		return false, err
	}
	removedBackup, err := removeUnits(ctx, a, backupPaths, a.BackupTimerName())
	return removed || removedBackup, err
}

func removeUnits(ctx context.Context, a AutoUpgrade, paths []string, timer string) (bool, error) {
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
	if unitIs(ctx, a.Rootless, timer, "enabled") || unitIs(ctx, a.Rootless, timer, "active") {
		if err := systemctlRun(ctx, a.Rootless, "disable", "--now", timer); err != nil {
			return false, err
		}
	}
	if err := removeHostFiles(ctx, a.Rootless, found...); err != nil {
		return false, err
	}
	return true, systemctlRun(ctx, a.Rootless, "daemon-reload")
}

func DropBackupFlag(ctx context.Context, a AutoUpgrade) (bool, error) {
	paths, err := a.Paths()
	if err != nil {
		return false, err
	}
	data, err := os.ReadFile(paths[0])
	if os.IsNotExist(err) {
		return false, nil
	}
	if err != nil {
		return false, err
	}
	backup := serviceBackup(data)
	if backup == BackupFull {
		return false, nil
	}
	full := bytes.ReplaceAll(data, []byte(" upgrade --yes --backup "+backup+" "), []byte(" upgrade --yes "))
	if bytes.Equal(full, data) {
		return false, fmt.Errorf("host: couldn't find --backup %s in %s", backup, paths[0])
	}
	userScope := a.Scheduler == Systemd && a.Rootless
	if _, err := writeHostFile(ctx, userScope, paths[0], full); err != nil {
		return false, err
	}
	if a.Scheduler == Systemd {
		return true, systemctlRun(ctx, a.Rootless, "daemon-reload")
	}
	return true, nil
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
	var backupSince time.Time
	if backupPaths, err := a.BackupPaths(); err == nil {
		if data, err := os.ReadFile(backupPaths[len(backupPaths)-1]); err == nil {
			s.FullBackup = FullBackupMonthly
			if info, err := os.Stat(backupPaths[len(backupPaths)-1]); err == nil {
				backupSince = info.ModTime()
			}
			if a.Scheduler == Cron {
				_, s.FullBackupAt = cronSchedule(data)
			} else {
				s.FullBackupAt = backupTimerAt(data)
			}
		}
	}
	if a.Scheduler == Cron {
		s.Day, s.At = cronSchedule(data)
		s.Backup = serviceBackup(data)
		if hour, minute, err := (AutoUpgrade{At: s.At}).clock(); err == nil {
			s.Next = nextRun(now, s.Day, hour, minute).Format(systemdTimeLayout)
		}
		if info, err := os.Stat(a.Log); err == nil {
			s.Last = info.ModTime().Format(systemdTimeLayout)
			s.Ran, s.ExitCode = cronExit(a.Log)
		}
		if s.FullBackup != "" {
			_, err := os.Stat(a.BackupLog())
			s.FullBackupRan = err == nil
		}
		return s, nil
	}
	s.Day, s.At = timerSchedule(data)
	s.Backup = BackupFull
	if service, err := os.ReadFile(paths[0]); err == nil {
		s.Backup = serviceBackup(service)
	}
	t := systemctlShow(ctx, a.Rootless, a.TimerName(), "NextElapseUSecRealtime", "LastTriggerUSec")
	s.Next, s.Last = t["NextElapseUSecRealtime"], t["LastTriggerUSec"]
	svc := systemctlShow(ctx, a.Rootless, a.ServiceName(), "ActiveState", "ExecMainExitTimestamp", "ExecMainStatus")
	s.Running = svc["ActiveState"] == "activating"
	if svc["ExecMainExitTimestamp"] != "" {
		s.Ran = true
		s.ExitCode, _ = strconv.Atoi(svc["ExecMainStatus"])
	}
	if s.FullBackup != "" {
		service := systemctlShow(ctx, a.Rootless, a.BackupServiceName(), "ExecMainExitTimestamp")
		s.FullBackupRan = service["ExecMainExitTimestamp"] != "" || monthlyRunPassed(backupSince, now, s.FullBackupAt)
	}
	return s, nil
}

func monthlyRunPassed(since, now time.Time, at string) bool {
	t, err := time.Parse("15:04", at)
	if err != nil || since.IsZero() {
		return false
	}
	last := time.Date(now.Year(), now.Month(), 1, t.Hour(), t.Minute(), 0, 0, now.Location())
	if last.After(now) {
		last = last.AddDate(0, -1, 0)
	}
	return last.After(since)
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

func timerSchedule(data []byte) (string, string) {
	for _, line := range strings.Split(string(data), "\n") {
		v, ok := strings.CutPrefix(line, "OnCalendar=")
		if !ok {
			continue
		}
		f := strings.Fields(v)
		day := ""
		if len(f) == 3 {
			d, ok := Weekday(f[0])
			if !ok {
				continue
			}
			day, f = dayName(d), f[1:]
		}
		if len(f) == 2 && f[0] == "*-*-*" && len(f[1]) >= 5 {
			return day, f[1][:5]
		}
	}
	return "", ""
}

func backupTimerAt(data []byte) string {
	for _, line := range strings.Split(string(data), "\n") {
		v, ok := strings.CutPrefix(line, "OnCalendar=*-*-01 ")
		if ok && len(v) >= 5 {
			return v[:5]
		}
	}
	return ""
}

func cronSchedule(data []byte) (string, string) {
	for _, line := range strings.Split(string(data), "\n") {
		f := strings.Fields(line)
		if len(f) < 5 || strings.Contains(f[0], "=") {
			continue
		}
		minute, merr := strconv.Atoi(f[0])
		hour, herr := strconv.Atoi(f[1])
		if merr != nil || herr != nil {
			continue
		}
		day := ""
		if f[4] != "*" {
			d, err := strconv.Atoi(f[4])
			if err != nil || d < 0 || d > 7 {
				continue
			}
			day = dayName(d % 7)
		}
		return day, fmt.Sprintf("%02d:%02d", hour, minute)
	}
	return "", ""
}

func serviceBackup(data []byte) string {
	for _, line := range strings.Split(string(data), "\n") {
		f := strings.Fields(line)
		for i := 0; i+1 < len(f); i++ {
			if f[i] == "--backup" {
				return f[i+1]
			}
		}
	}
	return BackupFull
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

func nextRun(now time.Time, day string, hour, minute int) time.Time {
	want, weekly := Weekday(day)
	for i := 0; ; i++ {
		next := time.Date(now.Year(), now.Month(), now.Day()+i, hour, minute, 0, 0, now.Location())
		if next.After(now) && (!weekly || int(next.Weekday()) == want) {
			return next
		}
	}
}
