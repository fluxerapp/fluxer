// SPDX-License-Identifier: AGPL-3.0-or-later

package main

import (
	"context"
	"fmt"
	"os"
	"os/signal"
	"strings"
	"syscall"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/config"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/ctl"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/lock"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/logs"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/notify"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/pg"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/stamp"
)

const (
	keepPreUpgrade = 3
	newerDataPoll  = 2 * time.Second
	extendEvery    = time.Minute
)

type upgradeBackup func(ctx context.Context, from string) error

func settleVersion(ctx context.Context, data, release string, before upgradeBackup) error {
	stamped, err := stamp.Read(data)
	if err != nil {
		return err
	}
	switch stamp.Decide(stamped, release) {
	case stamp.Adopt:
		if before != nil {
			if err := before(ctx, ""); err != nil {
				return fmt.Errorf("the backup before Fluxer %s first ran on this data failed, so Fluxer did not start: %w", release, err)
			}
		}
		return stamp.Write(data, release)
	case stamp.Upgrade:
		if before != nil {
			if err := before(ctx, stamped); err != nil {
				return fmt.Errorf("the backup before the upgrade from Fluxer %s to %s failed, so Fluxer did not start: %w", stamped, release, err)
			}
		}
		return stamp.Write(data, release)
	case stamp.Refuse:
		return fmt.Errorf("this data was upgraded to Fluxer %s, which is newer than this Fluxer %s", stamped, release)
	}
	return nil
}

func dataVersion(l spec.Layout, root string) string {
	if v, err := stamp.Read(l.Data); err == nil && stamp.Valid(v) {
		return v
	}
	return releaseVersion(root)
}

func backupOnUpgrade(vars spec.Env) bool {
	switch strings.ToLower(strings.TrimSpace(vars["FLUXER_BACKUP_ON_UPGRADE"])) {
	case "false", "0", "no", "off":
		return false
	}
	return true
}

func preUpgradeBackup(cfg *config.Config, out *logs.Output, status func(string)) upgradeBackup {
	if !backupOnUpgrade(cfg.Env) {
		return nil
	}
	return func(ctx context.Context, from string) error {
		if cfg.Env["FLUXER_POSTGRES_URL"] != "" {
			out.Logf(logs.PrioWarning, "the database is external, so back it up yourself before an upgrade")
			return nil
		}
		if _, ok, err := pg.DataMajor(cfg.Layout.PostgresDir()); err != nil || !ok {
			return err
		}
		t, err := backupTarget(cfg)
		if err != nil {
			return err
		}
		status("backing up the database before the upgrade")
		ctx, stop := signal.NotifyContext(ctx, syscall.SIGTERM, syscall.SIGINT)
		defer stop()
		done := make(chan struct{})
		defer close(done)
		go func() {
			tick := time.NewTicker(extendEvery)
			defer tick.Stop()
			for {
				notify.Extend(5 * extendEvery)
				select {
				case <-done:
					return
				case <-tick.C:
				}
			}
		}()
		path, err := backup.Create(ctx, t, nil, backup.CreateOptions{
			Scope:    backup.ScopeDatabase,
			Locked:   true,
			Release:  from,
			Reason:   backup.ReasonPreUpgrade,
			Progress: func(stage string) { out.Logf(logs.PrioInfo, "backup before the upgrade: %s", stage) },
		})
		if err != nil {
			return err
		}
		if from == "" {
			out.Logf(logs.PrioInfo, "backed up the database of the earlier Fluxer to %s", path)
		} else {
			out.Logf(logs.PrioInfo, "backed up the database of Fluxer %s to %s", from, path)
		}
		return backup.Prune(cfg.Layout.BackupsDir(), backup.ReasonPreUpgrade, keepPreUpgrade)
	}
}

func newerDataMessage(l spec.Layout, stamped, release string) string {
	msg := fmt.Sprintf("this data was upgraded to Fluxer %s, and this Fluxer %s can't run on it. Start Fluxer %s or newer", stamped, release, stamped)
	for _, a := range backup.List(l.BackupsDir(), backup.ReasonPreUpgrade) {
		if c, ok := stamp.Compare(a.Manifest.Release, release); ok && c <= 0 {
			return msg + ", or go back to the backup from before the upgrade with fluxerd restore " + a.Path
		}
	}
	return msg + ", or restore a backup made with Fluxer " + release + " or older"
}

func waitWhileDataIsNewer(l spec.Layout, release string, out *logs.Output) (bool, error) {
	stamped, err := stamp.Read(l.Data)
	if err != nil || stamp.Decide(stamped, release) != stamp.Refuse {
		return false, err
	}
	lk, err := lock.Acquire(l.LockFile())
	if err != nil {
		return false, err
	}
	lk.Release()
	sigs := make(chan os.Signal, 4)
	signal.Notify(sigs, syscall.SIGHUP, syscall.SIGTERM, syscall.SIGINT)
	defer signal.Stop(sigs)
	msg := newerDataMessage(l, stamped, release)
	out.Logf(logs.PrioErr, "%s", msg)
	ln, err := ctl.Listen(l.ControlSocket())
	if err != nil {
		return false, err
	}
	srv := controlServer(ctl.Waiting(msg))
	go srv.Serve(ln)
	defer srv.Close()
	notify.Status(msg)
	notify.Ready()
	tick := time.NewTicker(newerDataPoll)
	defer tick.Stop()
	for {
		select {
		case sig := <-sigs:
			if sig == syscall.SIGHUP {
				continue
			}
			out.Logf(logs.PrioInfo, "%s: shutting down", sig)
			notify.Stopping()
			return true, nil
		case <-tick.C:
			stamped, err := stamp.Read(l.Data)
			if err != nil {
				return false, err
			}
			if stamp.Decide(stamped, release) != stamp.Refuse {
				out.Logf(logs.PrioInfo, "the data now fits Fluxer %s, starting", release)
				notify.Status("starting")
				return false, nil
			}
		}
	}
}
