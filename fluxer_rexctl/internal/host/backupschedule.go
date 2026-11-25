// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

const (
	ScheduleOff    = "off"
	ScheduleDaily  = "daily"
	ScheduleWeekly = "weekly"
)

const scheduledBackupDay = "Sun"

var (
	ErrNoScheduler  = errors.New("host: this machine has neither systemd nor cron")
	ErrNotLingering = errors.New("host: user services stop when the account signs out")
)

type ScheduledBackup struct {
	Scheduler Scheduler
	Project   string
	Dir       string
	Binary    string
	For       string
	Podman    bool
	Rootless  bool
}

func (b ScheduledBackup) name() string { return b.Project + "-scheduled-backup" }

func (b ScheduledBackup) TimerName() string { return b.name() + ".timer" }

func (b ScheduledBackup) units() AutoUpgrade {
	return AutoUpgrade{Scheduler: b.Scheduler, Project: b.Project, Dir: b.Dir, Binary: b.Binary, Podman: b.Podman, Rootless: b.Rootless}
}

func (b ScheduledBackup) Paths() ([]string, error) {
	switch b.Scheduler {
	case Cron:
		return []string{filepath.Join(cronDir, b.name())}, nil
	case Systemd:
		dir, err := unitDir(b.Rootless)
		if err != nil {
			return nil, err
		}
		return []string{filepath.Join(dir, b.name()+".service"), filepath.Join(dir, b.TimerName())}, nil
	}
	return nil, ErrNoScheduler
}

func (b ScheduledBackup) args(quote func(string) string) string {
	out := quote(b.Binary) + " backup --scheduled --dir " + quote(b.Dir)
	if b.For != "" {
		out += " --for " + quote(b.For)
	}
	return out
}

func (b ScheduledBackup) files(every string) ([][]byte, error) {
	if every != ScheduleDaily && every != ScheduleWeekly {
		return nil, fmt.Errorf("host: %q is not a backup schedule", every)
	}
	if !filepath.IsAbs(b.Binary) || !filepath.IsAbs(b.Dir) {
		return nil, fmt.Errorf("host: scheduled backups need absolute paths, got %q and %q", b.Binary, b.Dir)
	}
	runAs := b.units().runAs()
	dir := strings.ReplaceAll(b.Dir, "%", "%%")
	switch b.Scheduler {
	case Cron:
		if runAs == "" {
			runAs = "root"
		}
		dow := "*"
		if every == ScheduleWeekly {
			dow = "0"
		}
		line := fmt.Sprintf("SHELL=/bin/sh\nPATH=%s\n0 3 * * %s %s %s >/dev/null 2>&1\n", cronPath, dow, runAs, b.args(cronQuote))
		return [][]byte{[]byte(line)}, nil
	case Systemd:
		if b.Rootless {
			runAs = ""
		}
		var svc strings.Builder
		fmt.Fprintf(&svc, "[Unit]\nDescription=Back up Fluxer in %s\nAfter=%s.service\n\n[Service]\nType=oneshot\n", dir, b.Project)
		if runAs != "" {
			fmt.Fprintf(&svc, "User=%s\n", runAs)
		}
		fmt.Fprintf(&svc, "ExecStart=%s\n", b.args(systemdQuote))
		cal, what := "*-*-* 03:00:00", "every day"
		if every == ScheduleWeekly {
			cal, what = scheduledBackupDay+" "+cal, "every week"
		}
		timer := fmt.Sprintf("[Unit]\nDescription=Back up Fluxer in %s %s\n\n[Timer]\nOnCalendar=%s\nRandomizedDelaySec=30min\nPersistent=true\n\n[Install]\nWantedBy=timers.target\n", dir, what, cal)
		return [][]byte{[]byte(svc.String()), []byte(timer)}, nil
	}
	return nil, ErrNoScheduler
}

func (b ScheduledBackup) Current() string {
	paths, err := b.Paths()
	if err != nil {
		return ScheduleOff
	}
	data, err := os.ReadFile(paths[len(paths)-1])
	if err != nil {
		return ScheduleOff
	}
	for _, line := range strings.Split(string(data), "\n") {
		f := strings.Fields(line)
		switch {
		case strings.HasPrefix(line, "OnCalendar="+scheduledBackupDay+" "):
			return ScheduleWeekly
		case strings.HasPrefix(line, "OnCalendar="):
			return ScheduleDaily
		case len(f) > 5 && f[0] == "0" && f[1] == "3":
			if f[4] == "*" {
				return ScheduleDaily
			}
			return ScheduleWeekly
		}
	}
	return ScheduleOff
}

func (b ScheduledBackup) Remove(ctx context.Context) (bool, error) {
	paths, err := b.Paths()
	if err != nil {
		return false, err
	}
	return removeUnits(ctx, b.units(), paths, b.TimerName())
}

func (b ScheduledBackup) Set(ctx context.Context, every string) error {
	paths, err := b.Paths()
	if err != nil {
		return err
	}
	if every == ScheduleOff {
		_, err := b.Remove(ctx)
		return err
	}
	data, err := b.files(every)
	if err != nil {
		return err
	}
	if err := installUnits(ctx, b.units(), paths, data, b.TimerName()); err != nil {
		return err
	}
	if b.Scheduler == Systemd && b.Rootless && !lingering(ctx) {
		if step, ok := lingerStep(); ok {
			if err := step.Run(ctx); err != nil {
				return fmt.Errorf("%w: %v", ErrNotLingering, err)
			}
		}
	}
	return nil
}
