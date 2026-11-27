// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"maps"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/migrate"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const undoMove = "move"

var errContainerHost = errors.New("migrate: one container takes no further changes in this move")

func moveTargetDir(inst *instance.Instance, to string) string {
	if to == runtimeSystemd {
		return defaultNativeDir(inst)
	}
	base := inst.Dir + "-container"
	if filepath.Clean(inst.Dir) == "/opt/fluxer" {
		return "/opt/fluxer-container"
	}
	return base
}

func newMoveTarget(app *App, inst *instance.Instance, to, dir string, dryRun bool) migrate.MoveTarget {
	if dir == "" {
		dir = moveTargetDir(inst, to)
	}
	if to == runtimeSystemd {
		return newNativeTarget(app, inst, dir, dryRun)
	}
	return newContainerTarget(app, inst, dir, engine.Kind(to), dryRun)
}

func minimalModes(inst *instance.Instance) []migrate.Option {
	done := migrate.Done(migrate.Compose, migrateValues(inst))
	_, uploads := backup.ExternalData(inst)
	var out []migrate.Option
	for _, o := range []migrate.Option{migrate.OptKV, migrate.OptStorage} {
		if o == migrate.OptStorage && uploads {
			continue
		}
		if !slices.Contains(done, o) {
			out = append(out, o)
		}
	}
	return out
}

func movePlan(inst *instance.Instance, to, dir string, modes []migrate.Option, oneWay bool) []ui.PlanItem {
	where := "Fluxer in one container"
	if to == runtimeSystemd {
		where = "the systemd service"
	}
	items := []ui.PlanItem{{Verb: ui.PlanCreate, Text: "Download Fluxer " + stack.EmbeddedRelease()}}
	for _, o := range modes {
		item := ui.PlanItem{Verb: ui.PlanChange, Text: o.Title(), Detail: "on the running instance first, and it stays if you undo the move"}
		if oneWay {
			item.Detail = "on the running instance first"
		}
		if o == migrate.OptStorage {
			item.Detail = "needs free space for a second copy of the uploads"
		}
		items = append(items, item)
	}
	items = append(items,
		ui.PlanItem{Verb: ui.PlanCreate, Text: "Back up your instance", Detail: inst.BackupDir()},
		ui.PlanItem{Verb: ui.PlanChange, Text: "Stop Fluxer's containers", Detail: "they and their volumes stay"},
		ui.PlanItem{Verb: ui.PlanCreate, Text: "Create " + dir + " with " + where},
	)
	if strings.TrimSpace(inst.State.Settings[settings.RustProcess]) != settings.RustSingle {
		items = append(items, ui.PlanItem{Verb: ui.PlanChange, Text: migrate.OptRust.Title(), Detail: "the new instance runs the Rust services in one process"})
	}
	check := ui.PlanItem{Verb: ui.PlanRun, Text: "Check that it works", Detail: "if it doesn't, rexctl removes the new instance and starts the old containers"}
	if oneWay {
		check.Detail = "Fluxer " + stack.EmbeddedRelease() + " updates your own PostgreSQL database when it starts, so the move can't be undone after that"
	}
	return append(items, ui.PlanItem{Verb: ui.PlanRun, Text: "Restore the backup and start Fluxer at " + inst.Origin()}, check)
}

func pickMoveTarget(app *App, inst *instance.Instance) (string, error) {
	choices := moveChoices(inst)
	if len(choices) == 1 || !app.UI.Interactive() {
		if !app.UI.Interactive() {
			return "", ui.Usage("Pass --to docker, podman or systemd to say where Fluxer should run.")
		}
		return choices[0].to, nil
	}
	items := make([]ui.PickItem, len(choices))
	for i, c := range choices {
		items[i] = ui.PickItem{Value: c.to, Label: c.label}
	}
	return app.UI.Pick("Where should Fluxer run?", items, choices[0].to)
}

func runMove(ctx context.Context, app *App, inst *instance.Instance, f *migrateFlags) error {
	to := f.To
	switch to {
	case runtimeSystemd, string(engine.Docker), string(engine.Podman):
	default:
		return ui.Usage("Pass --to as docker, podman or systemd.")
	}
	if !legacyCompose(inst) {
		return ui.Refused("This instance already runs in the new layout, which rexctl migrate --to moves to.", "Run rexctl status to check it.")
	}
	st, err := migrate.LoadState(inst.Dir)
	if err != nil {
		return err
	}
	if moved, err := recoverDockerMove(ctx, app, inst, f.DryRun); err != nil || moved {
		return err
	}
	if err := managed(inst); err != nil {
		return err
	}
	if to == runtimeSystemd {
		if err := dockerMoveAllowed(app, inst, f.NewDir, f.DryRun); err != nil {
			return err
		}
	}
	target := newMoveTarget(app, inst, to, f.NewDir, f.DryRun)
	modes := minimalModes(inst)
	items := movePlan(inst, to, target.Dir(), modes, target.OneWay())
	if f.DryRun && st.Docker != nil {
		app.UI.Plan("Then it would do this", items...)
		app.UI.Info("rexctl checks the move once the half-made instance is removed.")
		app.UI.Info("Dry run. Nothing changed.")
		return nil
	}
	if err := movePreflight(ctx, inst, target); err != nil {
		return err
	}
	if f.DryRun {
		app.UI.Plan("rexctl would do this", items...)
		app.UI.Info("Dry run. Nothing changed.")
		return nil
	}
	if !f.confirmed {
		app.UI.Plan("rexctl will do this", items...)
		ok, err := app.UI.Confirm("Make these changes?", false)
		if err != nil {
			return err
		}
		if !ok {
			app.UI.Info("Nothing changed. Fluxer %s keeps running.", inst.State.Release)
			return nil
		}
	}
	if to == runtimeSystemd {
		defer os.RemoveAll(downloadDir(target.Dir()))
	}
	if err := target.Prepare(ctx); err != nil {
		return unchanged(err)
	}
	if len(modes) > 0 {
		if err := moveModes(ctx, app, inst, modes); err != nil {
			return err
		}
		fresh, err := instance.Find(inst.Dir)
		if err != nil {
			return err
		}
		inst = fresh
		target = newMoveTarget(app, inst, to, f.NewDir, false)
	}
	recorded, err := loadMigrations(inst)
	if err != nil {
		return err
	}
	folders := oldFoldersOf(ctx, app, inst)
	var r *migrate.Run
	err = app.Locked(ctx, inst, func() error {
		var err error
		r, _, err = executeMigrate(ctx, app, inst, []migrate.Step{migrate.NewDockerStep(target, recorded)}, []migrate.Option{migrate.OptDocker})
		return err
	})
	if err != nil {
		return err
	}
	moved, err := instance.Find(r.Host.Dir())
	if err != nil {
		return err
	}
	if moved.State.MovedFrom != nil {
		moved.State.MovedFrom.AutoUpgrade = carrySchedules(ctx, app, inst, moved)
		if err := saveInstance(moved); err != nil {
			return err
		}
	}
	app.UI.OK("Fluxer %s runs from %s. The backup from before the move is %s.", moved.State.Release, moved.Dir, r.Backup)
	if len(folders) > 0 {
		app.UI.Info("The old containers kept their data in %s on this machine. rexctl leaves these folders as they are, also after rexctl migrate --clean-up, so delete them yourself once you no longer need them.", ui.JoinAnd(folders))
	}
	if oneWayMove(inst, moved.State.Release) {
		app.UI.Info("The old containers are stopped and stay until rexctl migrate --clean-up. Fluxer %s has updated your own PostgreSQL database, so the move can't be undone.", moved.State.Release)
		return nil
	}
	app.UI.Info("The old containers are stopped and stay until rexctl migrate --clean-up. To go back, run rexctl migrate --undo. If you undo, anything posted after the move is lost.")
	return nil
}

func unchanged(err error) error {
	var ee *ui.ExitError
	if !errors.As(err, &ee) || ee.Code != ui.ExitFailed {
		return err
	}
	refused := *ee
	refused.Code = ui.ExitRefused
	return &refused
}

func movePreflight(ctx context.Context, inst *instance.Instance, target migrate.MoveTarget) error {
	c, err := migrate.ForNative(maps.Clone(inst.State.Settings))
	if err == nil {
		err = target.Preflight(ctx, c.Settings)
	}
	var refused *ui.ExitError
	switch {
	case err == nil:
		return nil
	case errors.As(err, &refused):
		return refused
	}
	return &ui.ExitError{Code: ui.ExitRefused, Msg: "rexctl didn't change anything.", Fix: migrateReason(err), Err: err}
}

func moveModes(ctx context.Context, app *App, inst *instance.Instance, modes []migrate.Option) error {
	var steps []migrate.Step
	for _, o := range modes {
		if o == migrate.OptKV {
			steps = append(steps, migrate.NewKVStep())
		} else {
			steps = append(steps, migrate.NewStorageStep())
		}
	}
	return app.Locked(ctx, inst, func() error {
		r, st, err := executeMigrate(ctx, app, inst, steps, modes)
		if err != nil {
			return err
		}
		for _, n := range r.Notes {
			app.UI.Info("%s", n)
		}
		if st.Storage != nil && !st.Storage.Open() {
			return openWrites(ctx, app, r.Host, &st)
		}
		return nil
	})
}

type schedules struct {
	auto       bool
	day, at    string
	fullBackup string
	backups    string
}

func readSchedules(ctx context.Context, inst *instance.Instance) schedules {
	var out schedules
	if !host.BootUnitSupported() || host.UpgradeScheduler() == "" {
		return out
	}
	if a, err := autoUpgradeFor(inst, nil); err == nil && a.Scheduler != "" {
		if st, err := host.CheckAutoUpgrade(ctx, a, time.Now()); err == nil && st.On {
			out.auto = true
			out.day, out.at = a.Requested(st.Day, st.At)
			out.fullBackup = st.FullBackup
		}
	}
	if r, err := rt.Open(ctx, inst, nil); err == nil {
		if s, err := backupScheduleFor(r, inst); err == nil {
			if every := s.Current(); every != host.ScheduleOff {
				out.backups = every
			}
		}
	}
	return out
}

func dropSchedules(ctx context.Context, inst *instance.Instance, s schedules) error {
	var errs []error
	if a, err := autoUpgradeFor(inst, nil); s.auto && err == nil {
		_, err = host.RemoveAutoUpgrade(ctx, a)
		errs = append(errs, err)
	}
	if r, err := rt.Open(ctx, inst, nil); s.backups != "" && err == nil {
		if b, err := backupScheduleFor(r, inst); err == nil {
			_, err = b.Remove(ctx)
			errs = append(errs, err)
		}
	}
	return errors.Join(errs...)
}

func applySchedules(ctx context.Context, app *App, inst *instance.Instance, s schedules) {
	if s.auto {
		a, err := autoUpgradeFor(inst, nil)
		if err == nil {
			a.Day, a.At, a.FullBackup = s.day, s.at, s.fullBackup
			err = host.InstallAutoUpgrade(ctx, a)
		}
		if err != nil {
			app.UI.Detail("%v", err)
			app.UI.Warn("Couldn't turn on automatic upgrades for "+inst.Dir+".", "Run rexctl auto-upgrade on --dir "+shellWord(inst.Dir)+".")
		}
	}
	if s.backups != "" {
		r, err := rt.Open(ctx, inst, nil)
		if err == nil {
			var b host.ScheduledBackup
			if b, err = backupScheduleFor(r, inst); err == nil {
				err = b.Set(ctx, s.backups)
			}
		}
		if err != nil {
			app.UI.Detail("%v", err)
			app.UI.Warn("Couldn't turn on scheduled backups for "+inst.Dir+".", "Run rexctl backup schedule "+s.backups+" --dir "+shellWord(inst.Dir)+".")
		}
	}
}

func carrySchedules(ctx context.Context, app *App, from, to *instance.Instance) bool {
	s := readSchedules(ctx, from)
	if err := dropSchedules(ctx, from, s); err != nil {
		app.UI.Detail("removing the schedules of %s: %v", from.Dir, err)
	}
	applySchedules(ctx, app, to, s)
	return s.auto
}

func movedPair(inst *instance.Instance) (*instance.Instance, *instance.Instance, error) {
	moved := inst
	if inst.State.MovedTo != "" {
		next, err := instance.Find(inst.State.MovedTo)
		if err != nil {
			return nil, nil, ui.Refused("There's no move to undo.", "Run rexctl status to check this instance.")
		}
		moved = next
	}
	if moved.State.MovedFrom == nil {
		return nil, nil, ui.Refused("This instance didn't move here with rexctl migrate, so there's nothing to undo or clean up.", "Run rexctl status to check it.")
	}
	old, err := instance.Find(moved.State.MovedFrom.Dir)
	if err != nil {
		return moved, nil, nil
	}
	return moved, old, nil
}

func runUndoMove(ctx context.Context, app *App, inst *instance.Instance, confirm string) error {
	if inst.State.MovedTo != "" {
		if _, err := instance.Find(inst.State.MovedTo); errors.Is(err, instance.ErrNotFound) {
			return finishGoneUndo(ctx, app, inst)
		}
	}
	moved, old, err := movedPair(inst)
	if err != nil {
		return err
	}
	if old == nil || instance.RealPath(old.State.MovedTo) != instance.RealPath(moved.Dir) {
		return ui.Refused(fmt.Sprintf("The old instance in %s is gone, so there's nothing to go back to.", moved.State.MovedFrom.Dir), "Run rexctl status to check this one.")
	}
	if euid() != 0 && (rt.KindOf(moved) == rt.Native || rootOwned(old)) {
		return ui.Refused("Going back needs root.", "Run "+sudoAgain(moved.Dir)+".")
	}
	if oneWayMove(old, moved.State.Release) {
		return ui.Refused(fmt.Sprintf("Fluxer %s has already updated your own PostgreSQL database, and the old containers on Fluxer %s can't run on it, so rexctl can't go back.", moved.State.Release, old.State.Release), "Keep the new instance and run rexctl status to check it. The backup from before the move is in "+old.BackupDir()+".")
	}
	what := fmt.Sprintf("This removes Fluxer from %s and starts the old containers in %s again, with the data from before the move. rexctl backs up the new instance first, but anything posted after the move is gone from the old one. Any change rexctl made to Valkey or SeaweedFS before the move stays.", moved.Dir, old.Dir)
	if err := app.UI.ConfirmDestructive(what, moved.Domain(), confirm); err != nil {
		return err
	}
	if err := os.Chdir(old.Dir); err != nil {
		return ui.Wrap(err, "Couldn't open "+old.Dir+", so rexctl didn't go back.", "Check that "+old.Dir+" is there and readable, then run "+migrateCommand+" --undo again.")
	}
	if err := oldImagesHere(ctx, app, old); err != nil {
		return err
	}
	runner := &moveBase{app: app, inst: old}
	if err := runner.self(ctx, migrateOutput(app), nil, "backup", "--dir", moved.Dir); err != nil {
		return ui.Wrap(err, "Couldn't back up the new instance, so rexctl didn't go back.", "Run rexctl backup --dir "+shellWord(moved.Dir)+" to see why.")
	}
	s := readSchedules(ctx, moved)
	if err := dropSchedules(ctx, moved, s); err != nil {
		app.UI.Detail("removing the schedules of %s: %v", moved.Dir, err)
	}
	rules := moved.State.MovedFrom.Firewall
	if err := keepRules(moved, rules); err != nil {
		return err
	}
	if err := runner.selfQuiet(ctx, "uninstall", "--dir", moved.Dir, "--confirm", moved.Domain()); err != nil {
		return ui.Wrap(err, "Couldn't remove the new instance in "+moved.Dir+".", "Run rexctl uninstall --dir "+shellWord(moved.Dir)+", then run "+migrateCommand+" --undo again.")
	}
	return finishUndo(ctx, app, old, rules, s)
}

func oldImagesHere(ctx context.Context, app *App, old *instance.Instance) error {
	e, err := app.Engine(ctx, old)
	if err != nil {
		return err
	}
	err = app.UI.Step(ctx, "Checking the images of the old containers", func(ctx context.Context, s *ui.Step) error {
		p, err := deploy.Load(ctx, old, e)
		if err != nil {
			return err
		}
		if err := p.Pull(ctx, app.progress(s, "Downloading")); err != nil {
			return err
		}
		s.Done("The images of the old containers are on this machine.")
		return nil
	})
	return rt.ContainerError(err, "Couldn't get the images of the old containers, so rexctl didn't go back.")
}

func finishGoneUndo(ctx context.Context, app *App, old *instance.Instance) error {
	if euid() != 0 && rootOwned(old) {
		return ui.Refused("Going back needs root.", "Run "+sudoAgain(old.Dir)+".")
	}
	app.UI.Warn(fmt.Sprintf("The new instance in %s is already gone.", old.State.MovedTo), "rexctl starts the old containers in "+old.Dir+" again.")
	if err := finishUndo(ctx, app, old, nil, schedules{}); err != nil {
		return err
	}
	app.UI.Info("Automatic upgrades, backup schedules and firewall rules from the new instance didn't come back. Check them with %s.", sudoRun(old.Dir, "status"))
	return nil
}

func finishUndo(ctx context.Context, app *App, old *instance.Instance, rules []instance.OpenedPort, s schedules) error {
	return app.Locked(ctx, old, func() error {
		old.State.MovedTo = ""
		for _, p := range rules {
			if !slices.Contains(old.State.Firewall, p) {
				old.State.Firewall = append(old.State.Firewall, p)
			}
		}
		if err := saveInstance(old); err != nil {
			return err
		}
		app.remember(old.Dir)
		if old.State.BootService != instance.BootServiceOff {
			if e, err := app.Engine(ctx, old); err == nil {
				if err := refreshBootUnit(ctx, app, old, e, nil); err != nil {
					app.UI.Detail("%v", err)
					app.UI.Warn("Couldn't put back the service that starts Fluxer with this machine.", "Run rexctl doctor --fix --dir "+shellWord(old.Dir)+".")
				}
			}
		}
		if err := migrateStart(ctx, app, old); err != nil {
			return err
		}
		applySchedules(ctx, app, old, s)
		app.UI.OK("Fluxer %s runs from the old containers in %s again.", old.State.Release, old.Dir)
		return nil
	})
}

func runCleanUpMove(ctx context.Context, app *App, inst *instance.Instance, confirm string) error {
	moved, old, err := movedPair(inst)
	if err != nil {
		return err
	}
	if from, finish := unrecordedMove(moved); finish != "" {
		if moveUnderWay(from) {
			return ui.Refused(fmt.Sprintf("Fluxer is moving here from %s.", from), "Run "+migrateCommand+" --clean-up again when the move is done.")
		}
		return ui.Refused(fmt.Sprintf("The move from %s stopped before rexctl recorded it.", from), "Run "+finish+" first, then run "+migrateCommand+" --clean-up again.")
	}
	var folders []string
	if old != nil {
		e, err := app.Engine(ctx, old)
		if err != nil {
			return err
		}
		folders = oldFoldersOf(ctx, app, old)
		what := fmt.Sprintf("This deletes the old containers and volumes in %s, which hold the data from before the move. Backups in %s stay, and you can't undo the move afterwards.", old.Dir, old.BackupDir())
		if len(folders) > 0 {
			what += fmt.Sprintf(" rexctl doesn't delete %s, the %s the old containers kept data in.", ui.JoinAnd(folders), oneOrMany(len(folders), "folder", "folders"))
		}
		if err := app.UI.ConfirmDestructive(what, moved.Domain(), confirm); err != nil {
			return err
		}
		left := ""
		err = app.UI.Step(ctx, "Removing the old containers", func(ctx context.Context, s *ui.Step) error {
			p, err := deploy.Load(ctx, old, e)
			if err != nil {
				return err
			}
			if err := p.Down(ctx, true); err != nil {
				return err
			}
			if err := rt.RemoveProjectVolumes(ctx, e, p.Name); err != nil {
				return err
			}
			left = rt.NetworksLeft(ctx, e, p.Name)
			s.Done("Removed the old containers and their volumes.")
			return nil
		})
		if err != nil {
			return ui.Wrap(err, "Couldn't remove the old containers.", "Run "+migrateCommand+" --clean-up --verbose to see why.")
		}
		if left != "" {
			app.UI.Warn(left, "")
		}
	}
	moved.State.MovedFrom = nil
	if err := saveInstance(moved); err != nil {
		return err
	}
	app.UI.OK("The move to %s is complete.", moved.Dir)
	if len(folders) > 0 {
		app.UI.Info("%s still %s the data from before the move. Delete %s yourself once you no longer need %s.", ui.JoinAnd(folders), oneOrMany(len(folders), "holds", "hold"), oneOrMany(len(folders), "it", "them"), oneOrMany(len(folders), "it", "them"))
	}
	return nil
}

type containerHost struct {
	app  *App
	inst *instance.Instance
}

func (h *containerHost) Runtime() migrate.Runtime        { return migrate.Runtime(instance.RuntimeContainer) }
func (h *containerHost) Dir() string                     { return h.inst.Dir }
func (h *containerHost) Settings() map[string]string     { return maps.Clone(h.inst.State.Settings) }
func (h *containerHost) AppProxyURL() string             { return "" }
func (h *containerHost) ObjectsRoot() string             { return "" }
func (h *containerHost) Importer() migrate.Importer      { return nil }
func (h *containerHost) Start(ctx context.Context) error { return migrateStart(ctx, h.app, h.inst) }
func (h *containerHost) Backup(context.Context) (string, error) {
	return "", errContainerHost
}
func (h *containerHost) Stop(context.Context, []string) error      { return errContainerHost }
func (h *containerHost) StartOnly(context.Context, []string) error { return errContainerHost }
func (h *containerHost) Remove(context.Context, []string) error    { return errContainerHost }
func (h *containerHost) Services(context.Context) ([]migrate.Service, error) {
	return nil, errContainerHost
}
func (h *containerHost) Apply(context.Context, map[string]string) error { return errContainerHost }
func (h *containerHost) Env(map[string]string) (map[string]string, error) {
	return nil, errContainerHost
}
func (h *containerHost) CheckTarget(context.Context, map[string]string) error {
	return errContainerHost
}
func (h *containerHost) FreePorts(context.Context, []int) error { return errContainerHost }
func (h *containerHost) CheckObjects(context.Context) error     { return errContainerHost }
func (h *containerHost) PrepareObjects(context.Context) error   { return errContainerHost }
func (h *containerHost) RetireValkey(context.Context, string) (string, error) {
	return "", errContainerHost
}
func (h *containerHost) ReviveValkey(context.Context, string) error { return errContainerHost }
func (h *containerHost) HoldsData(context.Context, string) (bool, error) {
	return false, errContainerHost
}
func (h *containerHost) DropData(context.Context, string) error { return errContainerHost }

func (h *containerHost) Halt(ctx context.Context) error {
	r, err := h.app.Runtime(ctx, h.inst)
	if err != nil {
		return err
	}
	return h.app.UI.Step(ctx, "Stopping Fluxer", func(ctx context.Context, s *ui.Step) error { return r.Stop(ctx, s) })
}

func (h *containerHost) Check(ctx context.Context) error {
	r, err := h.app.Runtime(ctx, h.inst)
	if err != nil {
		return err
	}
	return h.app.UI.Check(ctx, "Checking Fluxer", func(ctx context.Context, s *ui.Step) error {
		st, err := r.Status(ctx)
		if err != nil {
			return err
		}
		if !st.Running {
			return fmt.Errorf("fluxer isn't running in the container")
		}
		for _, p := range st.Parts {
			if !p.Optional && (p.State == rt.PartFailed || p.State == rt.PartStopped) {
				return fmt.Errorf("%s is %s", p.Name, p.State)
			}
		}
		s.Done("Fluxer runs in its container.")
		return nil
	})
}
