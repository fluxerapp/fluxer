// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"os"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime/upgrade"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/setup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type composeUpgrader struct {
	app     *App
	inst    *instance.Instance
	f       *upgradeFlags
	pins    []string
	search  string
	waiting []string
}

func (c *composeUpgrader) engine(ctx context.Context) (*engine.Engine, error) {
	return c.app.Engine(ctx, c.inst)
}

func (c *composeUpgrader) names() (string, string) {
	pend := c.inst.State.Upgrade
	if pend == nil {
		return c.inst.State.Release, c.inst.State.Release
	}
	return pend.From, pend.To
}

func (c *composeUpgrader) Current(context.Context) (string, error) {
	return c.inst.State.Release, nil
}

func (c *composeUpgrader) Check(context.Context, *release.Release) error {
	return legacyRefusal()
}

func (c *composeUpgrader) Prepare(context.Context, *ui.Step, *release.Release) error {
	return legacyRefusal()
}

func (c *composeUpgrader) Backup(ctx context.Context, s *ui.Step, _ string) (string, error) {
	inst, pend := c.inst, c.inst.State.Upgrade
	e, err := c.engine(ctx)
	if err != nil {
		return "", err
	}
	info, err := createBackup(ctx, inst, e.Named(inst.Project(), inst.Dir), backup.CreateOptions{Reason: "upgrade", Scope: upgradeScope(c.f), OnEvent: c.app.progress(s, "Starting"), Progress: func(stage string) {
		if t := rt.BackupStageTitle(stage); t != "" {
			s.Title(t)
		}
	}})
	if errors.Is(err, backup.ErrNeverStarted) {
		s.Done("There was nothing to back up yet.")
		return "", nil
	}
	if err != nil {
		fix := "Run rexctl backup to see why, then run rexctl upgrade again."
		if logPath := writeBackupLog(inst, upgrade.LogName(pend.To), err); logPath != "" {
			fix = fmt.Sprintf("Read %s to see why, then run rexctl upgrade again.", logPath)
		}
		report := ui.Wrap(err, "Couldn't back up your instance.", fix)
		var be *ui.ExitError
		if errors.As(backupCreateErr(inst, err), &be) && be.Err == nil {
			report.Msg, report.Fix = be.Msg, be.Fix
		}
		return "", report
	}
	if info.WithoutUploads() {
		s.Title("Checking the backup")
		if _, err := backup.Verify(info.Path); err != nil {
			os.Remove(info.Path)
			return "", ui.Wrap(err, "The backup without uploads didn't pass its check.", "Run rexctl backup --verbose to see why, then run rexctl upgrade again.")
		}
		s.Done("Backed up your instance without uploads.")
		return info.Path, nil
	}
	s.Done("Backed up your instance.")
	return info.Path, nil
}

func (c *composeUpgrader) Switch(ctx context.Context, s *ui.Step, rel *release.Release) error {
	inst := c.inst
	_, toShort := c.names()
	e, err := c.engine(ctx)
	if err != nil {
		return err
	}
	c.search = runningImage(ctx, e.Named(inst.Project(), inst.Dir), searchService)
	next, err := release.SnapshotState(inst, rel.Version)
	if err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't read the settings for Fluxer %s.", toShort), "Run rexctl doctor to see what's wrong.")
	}
	set, err := release.LoadSet(inst, rel.Version)
	if err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't read the files for Fluxer %s.", toShort), "Run rexctl doctor to see what's wrong.")
	}
	r, err := inst.Render(set, stack.OverlaysFor(next.Settings), next.Images.Refs)
	if err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't write the files for Fluxer %s.", toShort), "Run rexctl upgrade --verbose to see why.")
	}
	st := inst.State
	st.Images, st.Settings, st.Secrets, st.Rendered = next.Images, next.Settings, next.Secrets, r.Hashes
	return nil
}

func (c *composeUpgrader) StartVerify(ctx context.Context, s *ui.Step) error {
	inst := c.inst
	e, err := c.engine(ctx)
	if err != nil {
		return err
	}
	if pend := inst.State.Upgrade; pend != nil && pend.Phase == release.PhaseGoingBack {
		return startRestored(ctx, c.app, inst, e, c.search, inst.State.Release, s)
	}
	short := inst.State.Release
	if err := e.Named(inst.Project(), inst.Dir).RemoveReplacements(ctx); err != nil {
		return rt.EngineError("", err)
	}
	p, err := deploy.Load(ctx, inst, e)
	if err != nil {
		return rt.EngineError("", err)
	}
	c.waiting, err = startRelease(ctx, c.app, inst, e, p, s, c.search, inst.State.Release)
	if err != nil {
		if len(c.waiting) == 0 && ctx.Err() == nil {
			c.waiting = notRunning(context.WithoutCancel(ctx), p)
		}
		if len(c.waiting) > 0 {
			return ui.Wrap(err, fmt.Sprintf("Fluxer %s didn't start because %s", short, checks.Services(c.waiting, "isn't ready.", "aren't ready.")), "")
		}
		return err
	}
	if err := inst.DropRootCopies(ctx, true); err != nil {
		c.app.UI.Detail("%v", err)
	}
	return nil
}

func (c *composeUpgrader) GoBack(ctx context.Context, s *ui.Step, from string) error {
	inst := c.inst
	e, err := c.engine(ctx)
	if err != nil {
		return err
	}
	c.search = runningImage(ctx, e.Named(inst.Project(), inst.Dir), searchService)
	if pend := inst.State.Upgrade; pend != nil {
		if err := inst.PutBackSites(pend.Sites); err != nil {
			c.app.UI.Detail("%v", err)
		}
	}
	if !release.HasSnapshot(inst, from) {
		return ui.Failed(fmt.Sprintf("The files for %s aren't on this machine anymore.", fluxerName(from)), "Run rexctl upgrade to start the newer release again instead.")
	}
	if err := release.Restore(inst, from); err != nil {
		return err
	}
	if err := inst.Save(); err != nil {
		return err
	}
	return matchSites(c.app, inst)
}

func (c *composeUpgrader) CheckBack(ctx context.Context, to string) error {
	snap, err := release.SnapshotState(c.inst, to)
	if err != nil {
		return nil
	}
	if errors.Is(release.CheckPostgres(postgresRef(c.inst.State), postgresRef(snap)), release.ErrPostgresMajor) {
		cur, prev := c.inst.State.Release, to
		return ui.Refused(fmt.Sprintf("Fluxer %s uses a different database version than %s.", prev, cur), c.inst.State.RestoreDatabaseFix())
	}
	return nil
}

func (c *composeUpgrader) Prune(ctx context.Context, keep []string) error {
	e, err := c.engine(ctx)
	if err != nil {
		return err
	}
	return release.PruneOlder(ctx, c.inst, e)
}

func (c *composeUpgrader) Abandon(u *instance.Upgrade) {
	if err := c.inst.PutBackSites(u.Sites); err != nil {
		c.app.UI.Detail("%v", err)
	}
}

func (c *composeUpgrader) FailureLog(ctx context.Context, name string, cause error) string {
	e, err := c.engine(ctx)
	if err != nil {
		return ""
	}
	return writeFailureLog(ctx, c.inst, e.Named(c.inst.Project(), c.inst.Dir), name, cause, c.waiting)
}

func (c *composeUpgrader) Sizes(ctx context.Context, to *release.Release, policy string) (download, saved int64) {
	download, saved = -1, -1
	e, err := c.engine(ctx)
	if err != nil {
		return download, saved
	}
	download = int64(upgradePull(ctx, c.inst, e))
	if policy == settings.BackupOff || policy == "" {
		return download, 0
	}
	scope := backup.ScopeFull
	if policy == settings.BackupWithoutUploads {
		scope = backup.ScopeWithoutUploads
	}
	if n, err := estimateBackup(ctx, c.inst, e.Named(c.inst.Project(), c.inst.Dir), scope); err == nil {
		saved = int64(n)
	}
	return download, saved
}

func (c *composeUpgrader) Finished(ctx context.Context, res upgrade.Result) {
	inst := c.inst
	e, err := c.engine(ctx)
	if err != nil {
		c.app.UI.Detail("%v", err)
		return
	}
	p := e.Named(inst.Project(), inst.Dir)
	forward := !upgrade.Rollback(&instance.Upgrade{From: res.From, To: res.To})
	if forward {
		c.app.importAdmins(ctx, inst, c.app.adminClient(inst, p))
		if done, err := setup.Done(ctx, c.app.adminClient(inst, p)); err == nil {
			inst.State.SetupDone = done
		}
		if ai := inst.State.AdminImport; ai != nil && ai.SentAt != "" {
			inst.State.AdminImport = nil
		}
		inst.State.MovedSites = nil
		if err := inst.Save(); err != nil {
			c.app.UI.Detail("%v", err)
		}
	}
	if loaded, err := deploy.Load(ctx, inst, e); err == nil {
		if err := refreshBootUnit(ctx, c.app, inst, e, loaded); err != nil {
			c.app.UI.Detail("%v", err)
			c.app.UI.Warn("rexctl couldn't update the service that starts Fluxer with this machine.", "Run rexctl doctor --fix to update it.")
		}
	}
	for _, pin := range c.pins {
		c.app.UI.Info("rexctl kept your own %s image.", pin)
	}
	if forward {
		upgradeLocalVoiceServers(ctx, c.app, inst, e)
	}
}
