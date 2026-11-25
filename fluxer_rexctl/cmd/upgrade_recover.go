// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime/upgrade"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func recoverPending(ctx context.Context, app *App, inst *instance.Instance, eng *upgrade.Engine) (upgrade.Result, error) {
	var res upgrade.Result
	err := app.Locked(ctx, inst, func() error {
		if err := adoptOldUpgrade(inst); err != nil {
			return err
		}
		var err error
		res, err = eng.Recover(ctx)
		return err
	})
	return res, err
}

func adoptOldUpgrade(inst *instance.Instance) error {
	u := inst.State.Upgrade
	if u == nil || u.From == u.To || inst.State.Release != u.From || phaseIndex(u.Phase) < phaseIndex(release.PhaseRecreated) || u.Phase == release.PhaseRestoring {
		return nil
	}
	next, err := release.SnapshotState(inst, u.To)
	if err != nil {
		return nil
	}
	st := inst.State
	st.Release, st.Images, st.Settings, st.Secrets = u.To, next.Images, next.Settings, next.Secrets
	return saveInstance(inst)
}

func pendingWhat(u *instance.Upgrade) string {
	toShort := u.To
	switch {
	case u.From == u.To:
		return "restore on Fluxer " + toShort
	case upgrade.Rollback(u):
		return "move back to Fluxer " + toShort
	}
	return "upgrade to Fluxer " + toShort
}

func recoveryPlan(app *App, eng *upgrade.Engine, u *instance.Upgrade) string {
	fromShort, toShort := u.From, u.To
	what := pendingWhat(u)
	switch u.Phase {
	case release.PhaseVerified:
		app.UI.Info("The %s finished, and rexctl records it first.", what)
		return u.To
	case release.PhaseRecreated:
		app.UI.Info("The %s didn't finish. rexctl checks that Fluxer %s runs first, and goes back to %s if it doesn't.", what, toShort, fluxerName(fromShort))
		return u.To
	case release.PhaseRestoring:
		app.UI.Info("The %s didn't finish. rexctl restores the backup again from the start.", what)
		return u.To
	case release.PhaseGoingBack:
		if u.BackFailed && !eng.Has(u.From) {
			app.UI.Info("The %s didn't finish, and %s isn't on this machine anymore. rexctl starts Fluxer %s again first.", what, fluxerName(fromShort), toShort)
			return u.To
		}
		if eng.NoWayBack(u) {
			app.UI.Info("The %s didn't finish, and Fluxer %s already updated the data with no backup from before. rexctl starts Fluxer %s again first.", what, toShort, toShort)
			return u.To
		}
		app.UI.Info("The %s didn't finish. rexctl goes back to %s first.", what, fluxerName(fromShort))
		return u.From
	case release.PhaseSwitching, release.PhaseMigrated:
		app.UI.Info("The %s didn't finish. rexctl goes back to %s first.", what, fluxerName(fromShort))
		return u.From
	}
	app.UI.Info("The %s didn't finish. rexctl stops it, and %s keeps running.", what, fluxerName(fromShort))
	return u.From
}

func reportRecovery(app *App, inst *instance.Instance, res upgrade.Result) {
	fromShort, toShort := res.From, res.To
	switch res.Outcome {
	case upgrade.Cleared:
		what := pendingWhat(&instance.Upgrade{From: res.From, To: res.To})
		app.UI.Info("The %s stopped before it changed anything, so %s keeps running.", what, fluxerName(fromShort))
	case upgrade.Upgraded:
		if res.From == res.To {
			app.UI.OK("Finished restoring the backup on Fluxer %s.", toShort)
			return
		}
		if upgrade.Rollback(&instance.Upgrade{From: res.From, To: res.To}) {
			app.outcome = fmt.Sprintf("Went back to Fluxer %s.", toShort)
			app.UI.OK("Went back to Fluxer %s.", toShort)
			return
		}
		pruneBackups(app, inst)
		app.outcome = fmt.Sprintf("Upgraded to Fluxer %s.", toShort)
		app.UI.OK("Finished the upgrade to Fluxer %s.", toShort)
	case upgrade.WentBack:
		app.UI.OK("Went back to %s.", fluxerName(fromShort))
	}
}

func startRestored(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine, search, version string, s *ui.Step) error {
	pull := false
	for _, ref := range inst.State.Images.Refs {
		if _, found, err := e.LocalImage(ctx, ref); err != nil || !found {
			pull = true
			break
		}
	}
	if err := e.Named(inst.Project(), inst.Dir).RemoveReplacements(ctx); err != nil {
		return err
	}
	_, err := deploy.Apply(ctx, inst, e, deploy.ApplyOptions{Pull: pull})
	if err != nil && engine.ClassOf(err) != engine.ClassDependencyFailed {
		return err
	}
	p, lerr := deploy.Load(ctx, inst, e)
	if lerr != nil {
		return lerr
	}
	if serr := keepSearchReadable(ctx, app, inst, p, search, version); serr != nil {
		return serr
	}
	if err != nil {
		if err := startEach(ctx, p, false); err != nil {
			return err
		}
	}
	started := map[string]bool{}
	show := rt.ShowReadiness(s, "Starting "+ui.Capitalise(fluxerName(version)))
	return p.WaitReady(ctx, readyTimeout, func(r engine.Readiness) {
		show(r)
		var stopped []string
		for _, svc := range r.Failed {
			if !stack.OneShot[svc] && !started[svc] {
				started[svc] = true
				stopped = append(stopped, svc)
			}
		}
		if len(stopped) > 0 {
			_ = p.Up(ctx, engine.UpOptions{Services: stopped, NoDeps: true})
		}
	})
}
