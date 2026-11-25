// SPDX-License-Identifier: AGPL-3.0-or-later

package main

import (
	"context"
	"errors"
	"fmt"
	"strconv"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/logs"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/supervise"
)

const (
	defaultBackupKeep  = 7
	defaultBackupHour  = 3
	scheduleMargin     = 12 * time.Hour
	scheduleFirstCheck = 5 * time.Minute
	scheduleRetry      = time.Minute
)

type schedule struct {
	every time.Duration
	keep  int
	hour  int
}

func parseSchedule(vars spec.Env) (schedule, error) {
	s := schedule{keep: defaultBackupKeep, hour: defaultBackupHour}
	switch v := strings.ToLower(strings.TrimSpace(vars["FLUXER_BACKUP_SCHEDULE"])); v {
	case "", "off":
	case "daily":
		s.every = 24 * time.Hour
	case "weekly":
		s.every = 7 * 24 * time.Hour
	default:
		return s, fmt.Errorf("FLUXER_BACKUP_SCHEDULE must be daily, weekly or off, not %q", v)
	}
	if v := strings.TrimSpace(vars["FLUXER_BACKUP_KEEP"]); v != "" {
		n, err := strconv.Atoi(v)
		if err != nil || n < 1 {
			return s, fmt.Errorf("FLUXER_BACKUP_KEEP must be a whole number from 1 up, not %q", v)
		}
		s.keep = n
	}
	if v := strings.TrimSpace(vars["FLUXER_BACKUP_HOUR"]); v != "" {
		n, err := strconv.Atoi(v)
		if err != nil || n < 0 || n > 23 {
			return s, fmt.Errorf("FLUXER_BACKUP_HOUR must be an hour of the day from 0 to 23, not %q", v)
		}
		s.hour = n
	}
	return s, nil
}

func backupDue(s schedule, archives []backup.Archive, now time.Time) bool {
	if s.every == 0 || now.Hour() != s.hour {
		return false
	}
	return len(archives) == 0 || now.Sub(archives[0].Manifest.CreatedAt)+scheduleMargin >= s.every
}

func runSchedule(ctx context.Context, cf *configFlags, root string, sup *supervise.Supervisor, out *logs.Output) {
	scheduleLoop(ctx, wallClock{}, func(now time.Time) bool {
		retry, err := scheduledBackup(ctx, cf, root, sup, out, now)
		if err != nil && ctx.Err() == nil {
			scheduleFailed(out, err)
		}
		return retry
	})
}

func scheduleFailed(out *logs.Output, err error) {
	out.Logf(logs.PrioErr, "scheduled backup failed: %v", err)
}

type scheduleClock interface {
	Now() time.Time
	After(d time.Duration) <-chan time.Time
}

type wallClock struct{}

func (wallClock) Now() time.Time                         { return time.Now() }
func (wallClock) After(d time.Duration) <-chan time.Time { return time.After(d) }

func scheduleLoop(ctx context.Context, clk scheduleClock, check func(now time.Time) bool) {
	next := nextScheduleCheck(clk.Now(), scheduleFirstCheck)
	for {
		select {
		case <-ctx.Done():
			return
		case <-clk.After(next.Sub(clk.Now())):
		}
		soon := time.Duration(0)
		if check(clk.Now()) {
			soon = scheduleRetry
		}
		next = nextScheduleCheck(clk.Now(), soon)
	}
}

func nextScheduleCheck(now time.Time, soon time.Duration) time.Time {
	top := time.Date(now.Year(), now.Month(), now.Day(), now.Hour(), 0, 0, 0, now.Location()).Add(time.Hour)
	if soon > 0 {
		if t := now.Add(soon); t.Before(top) {
			return t
		}
		if last := top.Add(-scheduleRetry); last.After(now) {
			return last
		}
	}
	return top
}

func scheduledBackup(ctx context.Context, cf *configFlags, root string, sup *supervise.Supervisor, out *logs.Output, now time.Time) (bool, error) {
	cfg, err := cf.loadWithSecrets()
	if err != nil {
		return false, err
	}
	s, err := parseSchedule(cfg.Env)
	if err != nil {
		return false, err
	}
	dir := cfg.Layout.BackupsDir()
	if !backupDue(s, backup.List(dir, backup.ReasonScheduled), now) {
		return false, nil
	}
	if !sup.Health().Ready {
		return true, nil
	}
	out.Logf(logs.PrioInfo, "scheduled backup: starting. Fluxer shows a maintenance page while it copies the uploads and the search index")
	t, err := backupTarget(cfg)
	if err != nil {
		return false, err
	}
	path, err := backup.Create(ctx, t, client(cfg.Layout.Runtime), backup.CreateOptions{
		Dir:      dir,
		EnvFile:  cfg.EnvFile,
		Release:  dataVersion(cfg.Layout, root),
		Reason:   backup.ReasonScheduled,
		Progress: func(stage string) { out.Logf(logs.PrioInfo, "scheduled backup: %s", stage) },
	})
	var space *backup.SpaceError
	if errors.As(err, &space) {
		return false, fmt.Errorf("%w. Free up space or lower FLUXER_BACKUP_KEEP", err)
	}
	if err != nil {
		return false, err
	}
	out.Logf(logs.PrioInfo, "scheduled backup written to %s", path)
	return false, backup.Prune(dir, backup.ReasonScheduled, s.keep)
}
