// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"
	"os"
	goruntime "runtime"
	"strings"
	"sync"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime/upgrade"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	legacyMoveFix     = "Run rexctl migrate first."
	cleanUpReminder   = 14 * 24 * time.Hour
	releasesPage      = "https://github.com/fluxerapp/fluxer/releases"
	legacyLookupLimit = 10 * time.Second
)

var legacyRefusedCommands = []string{"voice add", "voice join"}

func legacyCompose(inst *instance.Instance) bool {
	return inst != nil && inst.State != nil && !inst.State.IsVoice() && rt.KindOf(inst) == rt.Compose
}

func legacyCommandRefused(path string) bool {
	path = strings.TrimSpace(strings.TrimPrefix(path, "rexctl"))
	for _, c := range legacyRefusedCommands {
		if path == c || strings.HasPrefix(path, c+" ") {
			return true
		}
	}
	return false
}

func legacyRefusal() error {
	return ui.Refused("This instance uses the older layout with one container per service, and rexctl no longer changes it this way.", legacyMoveFix)
}

func lastComposeRelease(ctx context.Context) string {
	ctx, cancel := context.WithTimeout(ctx, legacyLookupLimit)
	defer cancel()
	rel, err := release.Resolve(ctx, "")
	if err != nil || !release.Valid(rel.ComposeLast) {
		return ""
	}
	return rel.ComposeLast
}

func takeoverFix(ctx context.Context) string {
	if last := lastComposeRelease(ctx); last != "" {
		return fmt.Sprintf("Install rexctl %s from %s/tag/fluxer-release@%s and run rexctl takeover with it, then run rexctl upgrade, which offers the move.", last, releasesPage, last)
	}
	return takeoverFixOffline
}

const takeoverFixOffline = "Install the last rexctl that has rexctl takeover from " + releasesPage + " and run rexctl takeover with it, then run rexctl upgrade, which offers the move."

func preRexctlRefusal(ctx context.Context, dir string) error {
	return ui.Refused(fmt.Sprintf("The Fluxer install in %s predates rexctl, and this rexctl doesn't take installs over.", dir), takeoverFix(ctx))
}

func legacyNeedsMoveLine(cur, to string) string {
	return fmt.Sprintf("Fluxer %s is the last release for this layout. Fluxer %s needs a move to one container or to systemd, which rexctl makes only when you ask.", cur, to)
}

type moveChoice struct {
	to    string
	label string
}

func moveChoices(inst *instance.Instance) []moveChoice {
	kind := engine.Kind(inst.State.Engine.Kind)
	if kind == "" {
		kind = engine.Docker
	}
	out := []moveChoice{{to: string(kind), label: "In " + kind.Name() + ", one container"}}
	if goruntime.GOOS == "linux" && euid() == 0 && host.BootUnitSupported() && nativeOS(detectHost(context.Background())) {
		out = append(out, moveChoice{to: runtimeSystemd, label: "On this machine with systemd"})
	}
	return out
}

func clearSelfUpdate(app *App, inst *instance.Instance) {
	u := inst.State.Upgrade
	if u == nil || (u.Phase != release.PhaseResolved && u.Phase != release.PhaseSelfUpdated) {
		return
	}
	inst.State.Upgrade = nil
	if err := inst.Save(); err != nil {
		app.UI.Detail("clearing the upgrade the older rexctl started: %v", err)
	}
}

func runLegacyUpgrade(ctx context.Context, app *App, inst *instance.Instance, f *upgradeFlags, selfUpdated bool) error {
	f.declined = true
	if inst.State.Upgrade != nil && !f.DryRun {
		clearSelfUpdate(app, inst)
	}
	if inst.State.Upgrade != nil {
		return finishComposeUpgrade(ctx, app, inst, f)
	}
	rel, err := resolveRelease(ctx, f.To)
	if err != nil {
		return err
	}
	cur, latest := inst.State.Release, rel.Version
	if release.Compare(latest, cur) <= 0 {
		app.UI.OK("Fluxer %s is the newest release for this layout.", cur)
		app.UI.Info("Run rexctl migrate to move it to one container or to systemd, which get new releases.")
		return nil
	}
	to := moveRelease(latest)
	newer := !selfUpdated && release.Valid(app.Version) && release.Compare(latest, app.Version) > 0
	newer = newer && (f.DryRun || f.Prefetch || app.UI.Interactive() || app.UI.Yes())
	if newer && f.DryRun {
		app.UI.Info("rexctl updates itself to %s first.", latest)
		to = latest
	}
	if newer && !f.DryRun {
		return legacySelfUpdate(ctx, app, inst, rel)
	}
	if f.To != "" && f.To != to {
		return ui.Refused(fmt.Sprintf("The move to the new layout installs Fluxer %s, the release of this rexctl.", to), "Run rexctl upgrade without --to, and run rexctl upgrade --to "+f.To+" after the move.")
	}
	if selfUpdated {
		app.UI.Info("rexctl updated itself. Fluxer %s needs a move to the new layout first.", to)
	}
	if f.DryRun || f.Prefetch || !app.UI.Interactive() || app.UI.Yes() {
		app.UI.Info("%s", legacyNeedsMoveLine(cur, to))
		app.UI.Info("Nothing changed. Run rexctl migrate when you're ready.")
		if !f.DryRun && inst.State.MoveNotice != to {
			app.moveNotice = to
		}
		return nil
	}
	moveCard(app, inst, to)
	if release.Compare(latest, to) > 0 {
		app.UI.Info("After the move, rexctl upgrade takes it on to Fluxer %s.", latest)
	}
	choices := moveChoices(inst)
	pick := choices[0].to
	if len(choices) > 1 {
		items := make([]ui.PickItem, len(choices))
		for i, c := range choices {
			items[i] = ui.PickItem{Value: c.to, Label: c.label}
		}
		if pick, err = app.UI.Pick("Where should Fluxer run?", items, choices[0].to); err != nil {
			return err
		}
	}
	ok, err := app.UI.Confirm("Move and upgrade?", false)
	if err != nil {
		return err
	}
	if !ok {
		app.UI.Info("Nothing changed. Fluxer %s keeps running. Run rexctl migrate when you're ready.", cur)
		return nil
	}
	return runMigrate(ctx, app, &migrateFlags{To: pick, confirmed: true})
}

func legacySelfUpdate(ctx context.Context, app *App, inst *instance.Instance, rel *release.Release) error {
	next := rel.Version
	asset, err := rel.AssetFor(goruntime.GOOS, goruntime.GOARCH)
	if err != nil {
		return ui.Failed(fmt.Sprintf("There's no rexctl %s for this machine.", next), "")
	}
	err = app.UI.Step(ctx, "Updating rexctl", func(ctx context.Context, s *ui.Step) error {
		if err := selfUpdate(ctx, asset); err != nil {
			return err
		}
		s.Done("Updated rexctl to %s.", next)
		return nil
	})
	if err != nil {
		return selfUpdateError(err, inst)
	}
	if err := reexec(resumeArgs(os.Args[1:], release.PhaseSelfUpdated, app.pickedDir(), false)); err != nil {
		return ui.Wrap(err, "Updated rexctl but couldn't start it again.", "Run rexctl upgrade again.")
	}
	return nil
}

func moveRelease(latest string) string {
	if to := stack.EmbeddedRelease(); release.Valid(to) {
		return to
	}
	return latest
}

func finishComposeUpgrade(ctx context.Context, app *App, inst *instance.Instance, f *upgradeFlags) error {
	eng, err := upgradeEngine(ctx, app, inst, f)
	if err != nil {
		return err
	}
	if f.DryRun {
		recoveryPlan(app, eng, inst.State.Upgrade)
		return nil
	}
	res, err := recoverPending(ctx, app, inst, eng)
	if err != nil {
		return err
	}
	if res.Outcome == upgrade.WentBack {
		return reportUpgrade(app, inst, res)
	}
	reportRecovery(app, inst, res)
	app.UI.Info("Run rexctl migrate to move it to one container or to systemd, which get new releases.")
	return nil
}

func moveCard(app *App, inst *instance.Instance, to string) {
	stays := "address, ports, settings, admins, users, messages, uploads"
	changes := "new TLS certificates, PostgreSQL 16 to 17 by dump and restore"
	database, uploads := backup.ExternalData(inst)
	if database {
		stays += ", your own PostgreSQL"
		changes = "new TLS certificates"
	}
	if uploads {
		stays += ", your own object store"
	}
	rows := []ui.KV{
		{Key: "Now on", Value: "Fluxer " + inst.State.Release + ", one container per service"},
		{Key: "After", Value: "Fluxer " + to + " in one container, or under systemd"},
		{Key: "Backup", Value: "everything, into " + inst.BackupDir() + ", before anything stops"},
		{Key: "Downtime", Value: "while the backup restores, longer with many uploads"},
		{Key: "Stays", Value: stays},
		{Key: "Changes", Value: changes},
		{Key: "Undo", Value: "rexctl migrate --undo starts the old containers again until rexctl migrate --clean-up removes them. Changes to Valkey or SeaweedFS stay"},
	}
	if oneWayMove(inst, to) {
		rows[len(rows)-1].Value = "not possible, because Fluxer " + to + " updates your own PostgreSQL database and the old containers can't run on it"
	}
	if free, err := host.FreeDisk(inst.Dir); err == nil {
		rows = append(rows, ui.KV{Key: "Free space", Value: ui.Bytes(int64(free))})
	}
	app.UI.Card(ui.Card{Title: "Move to the new layout", Rows: rows})
}

func legacyStatusLines(app *App) {
	app.UI.Info("This instance uses the older layout with one container per service.")
	app.UI.Info("It keeps running but gets no new releases, and no security fixes either. The move is the fix.")
	app.UI.Info("Run rexctl migrate to move it to one container or to systemd.")
}

func cleanUpDue(inst *instance.Instance, now time.Time) bool {
	m := inst.State.MovedFrom
	return m != nil && !m.At.IsZero() && now.Sub(m.At) >= cleanUpReminder
}

func cleanUpLine(app *App, inst *instance.Instance) {
	if !cleanUpDue(inst, time.Now()) {
		return
	}
	app.UI.Info("The old containers from %s, before the move on %s, are still on this machine.", inst.State.MovedFrom.Dir, inst.State.MovedFrom.At.Format("2 January"))
	app.UI.Info("Run rexctl migrate --clean-up to remove them once you're happy with the move.")
}

var fetchFailed sync.Map

func init() {
	instance.FetchSet = func(version, dir string) error {
		if err, ok := fetchFailed.Load(version); ok {
			return err.(error)
		}
		ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
		defer cancel()
		err := release.FetchComposeBundle(ctx, version, dir)
		if err != nil {
			fetchFailed.Store(version, err)
		}
		return err
	}
}
