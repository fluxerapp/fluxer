// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"io"
	"maps"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/migrate"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	nativeModesFile    = "modes.env"
	nativeAppProxyPort = "18084"
	composeAppProxyURL = "http://app-proxy:8080"
	composeVolumeLabel = "com.docker.compose.volume"
)

const migrateCommand = "rexctl migrate"

type migrateFlags struct {
	OldFlags   []string
	To         string
	Migrate    string
	DryRun     bool
	Details    bool
	NewDir     string
	OpenWrites bool
	Undo       string
	CleanUp    bool
	Confirm    string
	confirmed  bool
}

func newMigrateCmd(app *App) *cobra.Command {
	var f migrateFlags
	c := &cobra.Command{
		Use:   "migrate",
		Short: "Move an older instance to the current layout",
		Long: "Move your instance to the new layout, off Valkey or SeaweedFS, or onto one Rust process. " +
			"With --to, an instance in the older layout moves to one container on Docker or Podman, or to the systemd service. " +
			"rexctl shows what each change saves and removes, backs up your instance first, moves the data, checks it and goes back if anything goes wrong.\n\n" +
			"A move to one container runs as the account that runs the instance now. " +
			"The move to the systemd service runs as root, and so does every command for the new instance. " +
			"After the move off SeaweedFS, uploads stay read-only until you open them with --open-writes.\n\n" +
			"The old data stays until you remove it with --clean-up after a newer backup.",
		Args: noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			f.OldFlags = changedFlags(c, "experimental", "experimental-native")
			return runMigrate(c.Context(), app, &f)
		},
	}
	fl := c.Flags()
	fl.StringVar(&f.Migrate, "migrate", "", "the changes to make as a comma-separated list of docker, kv, rust or storage")
	fl.StringVar(&f.To, "to", "", "move an instance in the older layout, or the voice servers of a voice machine, to docker, podman or systemd")
	fl.BoolVar(&f.Details, "details", false, "show the machine, versions, date and method behind the typical figures")
	fl.StringVar(&f.NewDir, "new-dir", "", "the folder for the moved instance, with --to")
	fl.StringVar(&f.NewDir, "native-dir", "", "")
	_ = fl.MarkHidden("native-dir")
	fl.Bool("experimental", false, "")
	fl.Bool("experimental-native", false, "")
	_ = fl.MarkHidden("experimental")
	_ = fl.MarkHidden("experimental-native")
	fl.BoolVar(&f.DryRun, "dry-run", false, "show what each change saves and removes without changing anything")
	fl.BoolVar(&f.OpenWrites, "open-writes", false, "let people upload again after the move off SeaweedFS. rexctl can't move the files back after this")
	fl.StringVar(&f.Undo, "undo", "", "go back from a change rexctl migrate made. On its own it undoes the move to the new layout. Pass kv, or storage until you open writes")
	fl.Lookup("undo").NoOptDefVal = undoMove
	fl.BoolVar(&f.CleanUp, "clean-up", false, "remove the old containers and volumes after a move, or the Valkey or SeaweedFS data a change left behind once a newer backup holds what replaced it")
	fl.StringVar(&f.Confirm, "confirm", "", "your domain, to confirm --clean-up without a prompt")
	c.MarkFlagsMutuallyExclusive("migrate", "open-writes", "undo", "clean-up")
	return c
}

type migrateChoice struct {
	opt  migrate.Option
	diff migrate.Diff
	step migrate.Step
}

func runtimeOf(inst *instance.Instance) migrate.Runtime {
	if rt.KindOf(inst) == rt.Native {
		return migrate.Native
	}
	return migrate.Compose
}

func migrateValues(inst *instance.Instance) map[string]string {
	out := map[string]string{}
	if rt.KindOf(inst) == rt.Native {
		if f, err := envfile.Load(filepath.Join(native.New(inst).Root(), nativeModesFile)); err == nil {
			maps.Copy(out, f.Map())
		}
	}
	maps.Copy(out, inst.State.Settings)
	return out
}

func runMigrate(ctx context.Context, app *App, f *migrateFlags) error {
	for _, name := range f.OldFlags {
		app.UI.Info("rexctl migrate no longer needs --%s.", name)
	}
	picked, err := migrate.Parse(f.Migrate)
	if err != nil {
		return ui.Usage(sentence(err.Error()))
	}
	if f.CleanUp && f.DryRun {
		return ui.Usage("--clean-up has no dry run. Run it without --confirm to see what it deletes before you agree.")
	}
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	if inst.State.IsVoice() {
		if f.To == "" {
			return ui.Usage("On a voice machine, rexctl migrate only moves the voice servers. Run rexctl migrate --to systemd, docker or podman.")
		}
		return runVoiceMigrate(ctx, app, inst, f.To, f.DryRun)
	}
	if slices.Contains(picked, migrate.OptDocker) && legacyCompose(inst) {
		app.UI.Info("--migrate docker is now --to systemd.")
		f.To = runtimeSystemd
	}
	switch {
	case f.Undo == undoMove:
		return runUndoMove(ctx, app, inst, f.Confirm)
	case f.CleanUp && (inst.State.MovedFrom != nil || movedNotCleaned(inst)):
		return runCleanUpMove(ctx, app, inst, f.Confirm)
	case f.To == "" && legacyCompose(inst) && f.Migrate == "" && f.Undo == "" && !f.CleanUp && !f.OpenWrites:
		if st, err := migrate.LoadState(inst.Dir); err == nil && st.Docker != nil {
			moved, err := recoverDockerMove(ctx, app, inst, f.DryRun)
			if err == nil && f.DryRun && !moved {
				app.UI.Info("Dry run. Nothing changed.")
			}
			return err
		}
		if f.To, err = pickMoveTarget(app, inst); err != nil {
			return err
		}
	}
	if f.To != "" {
		return runMove(ctx, app, inst, f)
	}
	if rt.KindOf(inst) == rt.Container {
		return ui.Refused("This instance already runs in one container, the layout rexctl migrate moves to.", "Run rexctl status to check it.")
	}
	if moved, err := recoverDockerMove(ctx, app, inst, f.DryRun); err != nil || moved {
		return err
	}
	if err := managed(inst); err != nil {
		return err
	}
	switch {
	case f.OpenWrites:
		return runOpenWrites(ctx, app, inst)
	case f.Undo != "":
		return runUndo(ctx, app, inst, migrate.Option(f.Undo))
	case f.CleanUp:
		return runCleanUp(ctx, app, inst, f.Confirm)
	}
	m, err := migrate.LoadMeasurements()
	if err != nil {
		return err
	}
	choices, err := migrateChoices(ctx, app, inst, m)
	if err != nil {
		return err
	}
	if len(choices) == 0 {
		app.UI.OK("Every change is already made on this instance.")
		remindMigrations(ctx, app, inst)
		return nil
	}
	for _, o := range picked {
		if !slices.ContainsFunc(choices, func(c migrateChoice) bool { return c.opt == o }) {
			return ui.Refused(fmt.Sprintf("%s is already done on this instance, or doesn't apply to it.", o.Title()), "Run "+migrateCommand+" --dry-run to see the changes it can make.")
		}
	}
	paint := migrate.Paints(app.UI)
	if f.DryRun || (len(picked) == 0 && !app.UI.Interactive()) {
		showChoices(ctx, app, inst, m, choices, f.Details)
		if !f.DryRun {
			app.UI.Info("")
			app.UI.Info("Pass --migrate with the changes you want, such as --migrate %s.", choices[0].opt)
		}
		return nil
	}
	if len(picked) == 0 {
		migrate.Show(app.UI, migrate.SummaryLines(paint, diffsOf(choices), nil))
		var items []ui.PickItem
		for _, c := range choices {
			lines := c.diff.Lines(paint)
			migrate.Show(app.UI, lines)
			app.UI.Info("")
			if c.step == nil {
				continue
			}
			items = append(items, ui.PickItem{Value: string(c.opt), Label: c.opt.Title(), Hint: c.opt.Summary(), Detail: func(int) string { return strings.Join(lines, "\n") }})
		}
		if len(items) > 0 {
			vals, err := app.UI.PickMany("Which changes do you want to make?", items, nil)
			if err != nil {
				return err
			}
			for _, v := range vals {
				picked = append(picked, migrate.Option(v))
			}
		}
		if f.Details {
			migrate.Show(app.UI, migrate.BasisLines(paint, m))
		}
		if len(picked) == 0 {
			app.UI.Info("Nothing changed.")
			return nil
		}
	}
	picked = migrate.Order(picked)
	if len(picked) > 1 {
		if together, err := combinedDiff(ctx, app, inst, m, picked); err == nil {
			migrate.Show(app.UI, together.Lines(paint))
			app.UI.Info("")
		}
	}
	var steps []migrate.Step
	for _, o := range picked {
		i := slices.IndexFunc(choices, func(c migrateChoice) bool { return c.opt == o })
		steps = append(steps, choices[i].step)
	}
	app.UI.Plan("rexctl will do this", migratePlan(inst, steps)...)
	ok, err := app.UI.Confirm("Make these changes?", true)
	if err != nil {
		return err
	}
	if !ok {
		app.UI.Info("Nothing changed.")
		return nil
	}
	var r *migrate.Run
	var st migrate.State
	err = app.Locked(ctx, inst, func() error {
		var err error
		r, st, err = executeMigrate(ctx, app, inst, steps, picked)
		return err
	})
	if err != nil {
		return err
	}
	for _, n := range r.Notes {
		app.UI.Info("%s", n)
	}
	app.UI.OK("Made the changes. The backup from before them is %s.", r.Backup)
	if r.State.Storage == nil {
		return nil
	}
	opened, err := offerOpenWrites(ctx, app, inst, r.Host, &st)
	if err == nil && !opened {
		noticeReadOnly(ctx, app, inst)
	}
	return err
}

func movedNotCleaned(inst *instance.Instance) bool {
	moved := inst.Moved()
	return moved != nil && !moved.CleanedUp
}

func migratePlan(inst *instance.Instance, steps []migrate.Step) []ui.PlanItem {
	items := []ui.PlanItem{{Verb: ui.PlanCreate, Text: "Back up your instance", Detail: inst.BackupDir()}}
	for _, s := range steps {
		item := ui.PlanItem{Verb: ui.PlanChange, Text: s.Option().Title()}
		switch w := s.Writers(); {
		case s.Option() == migrate.OptDocker:
			item.Detail = "stops Fluxer's containers for the whole move"
		case len(w) > 0:
			item.Detail = "stops " + ui.JoinAnd(w) + " while the data moves"
		}
		items = append(items, item)
	}
	return append(items, ui.PlanItem{Verb: ui.PlanRun, Text: "Start Fluxer and check that it works", Detail: "if it doesn't, rexctl puts everything back"})
}

func keepMigrateBackup(inst *instance.Instance, path string) error {
	if inst.State.MigrateBackup != "" && fileExists(inst.State.MigrateBackup) && migrateUndoable(inst) {
		return nil
	}
	inst.State.MigrateBackup = path
	return saveInstance(inst)
}

func migrateUndoable(inst *instance.Instance) bool {
	if legacyCompose(inst) {
		return true
	}
	st, err := migrate.LoadState(inst.Dir)
	return err != nil || st.Docker != nil || st.Storage.Pending() || st.KV.Pending()
}

func clearDockerMarker(inst *instance.Instance) error {
	st, err := migrate.LoadState(inst.Dir)
	if err != nil || st.Docker == nil {
		return err
	}
	st.Docker = nil
	return st.Save(inst.Dir)
}

func executeMigrate(ctx context.Context, app *App, inst *instance.Instance, steps []migrate.Step, picked []migrate.Option) (*migrate.Run, migrate.State, error) {
	host, err := migrateHost(ctx, app, inst)
	if err != nil {
		return nil, migrate.State{}, err
	}
	journal, err := migrate.FileJournal(inst.Dir)
	if err != nil {
		return nil, migrate.State{}, ui.Wrap(err, "Couldn't read what rexctl migrate changed before.", "Check that you can read "+inst.Dir+", then try again.")
	}
	if ch, ok := host.(*composeHost); ok {
		if err := checkMountDrift(ctx, ch.p); err != nil {
			return nil, migrate.State{}, err
		}
	}
	r := &migrate.Run{Host: host, Journal: journal, Log: migrateOutput(app), OnPhase: migrateProgress(app, len(steps)), OnBackup: func(path string) error { return keepMigrateBackup(inst, path) }}
	err = migrate.Execute(ctx, r, steps)
	if ch, ok := host.(*composeHost); ok {
		ch.dropImporterImage(ctx)
	}
	if err == nil && slices.Contains(picked, migrate.OptDocker) {
		markMoved(app, inst, r.Host.Dir())
	}
	giveFilesBack(app, inst)
	var fail *migrate.Failure
	var refused *ui.ExitError
	if err == nil || errors.As(err, &fail) && fail.Rollback == nil {
		if cerr := clearDockerMarker(inst); cerr != nil {
			app.UI.Detail("%v", cerr)
		}
	}
	switch {
	case errors.As(err, &fail) && errors.Is(fail.Rollback, migrate.ErrOneWay):
		return r, migrate.State{}, oneWayError(err, fail, inst.Dir, r.Host.Dir(), ctx.Err() != nil)
	case errors.As(err, &fail) && fail.Rollback == nil && r.Backup != "" && (ctx.Err() != nil || errors.Is(err, context.Canceled)):
		return r, migrate.State{}, &ui.ExitError{Code: ui.ExitInterrupted, Msg: "Stopped the change before it finished. rexctl put your instance back as it was.", Fix: "The backup from before the change is " + r.Backup + ".", Err: err}
	case errors.As(err, &fail) && fail.Rollback == nil && r.Backup != "":
		return r, migrate.State{}, ui.Wrap(err, "The change didn't work, and rexctl put your instance back as it was. "+migrateReason(fail.Err), "The backup from before the change is "+r.Backup+".")
	case errors.As(err, &fail) && r.Backup != "":
		msg := "The change didn't work, and rexctl couldn't put everything back. " + migrateReason(fail.Err)
		fix := "Run " + migrateCommand + " again to finish going back, or restore the backup from before the change with rexctl restore " + r.Backup + "."
		var om *oldMountsError
		if errors.As(fail.Rollback, &om) {
			msg += " " + om.Msg
			fix = om.Fix
		}
		return r, migrate.State{}, ui.Wrap(err, msg, fix)
	case errors.As(err, &fail) && fail.Phase == migrate.PhasePreflight && errors.As(fail.Err, &refused):
		return r, migrate.State{}, refused
	case errors.As(err, &fail) && fail.Phase == migrate.PhasePreflight:
		return r, migrate.State{}, &ui.ExitError{Code: ui.ExitRefused, Msg: "rexctl didn't change anything.", Fix: migrateReason(fail.Err), Err: err}
	case errors.As(err, &fail):
		return r, migrate.State{}, ui.Wrap(err, "rexctl didn't change anything.", migrateReason(fail.Err))
	case err != nil:
		return r, migrate.State{}, ui.Wrap(err, "rexctl didn't change anything.", migrateReason(err))
	}
	r.State.Docker = nil
	st, err := migrate.LoadState(r.Host.Dir())
	if err != nil {
		return r, st, err
	}
	st = st.Merge(r.State)
	if err := st.Save(r.Host.Dir()); err != nil {
		return r, st, ui.Wrap(err, "Made the changes, but couldn't record them.", "Check that you can write to "+r.Host.Dir()+".")
	}
	return r, st, nil
}

func oneWayError(err error, fail *migrate.Failure, oldDir, dir string, interrupted bool) error {
	left := fmt.Sprintf("The old containers can't run on that database, so rexctl left the new instance in %s stopped and the old containers stopped.", dir)
	finish := migrateCommand + " --dir " + shellWord(oldDir)
	if moved, ferr := instance.Find(dir); ferr == nil {
		finish = finishMove(oldDir, moved)
	}
	if interrupted || errors.Is(fail.Err, context.Canceled) {
		return &ui.ExitError{
			Code: ui.ExitInterrupted,
			Msg:  fmt.Sprintf("Stopped the change after Fluxer %s started on your own PostgreSQL database. %s", stack.EmbeddedRelease(), left),
			Fix:  "Run " + finish + " to keep the new instance, then " + runThere(dir, "start") + ".",
			Err:  err,
		}
	}
	return ui.Wrap(err, fmt.Sprintf("The change didn't work after Fluxer %s started on your own PostgreSQL database. %s %s", stack.EmbeddedRelease(), left, migrateReason(fail.Err)), "Run "+finish+" to keep the new instance, then "+runThere(dir, "start")+" once the cause is fixed.")
}

func recoverDockerMove(ctx context.Context, app *App, inst *instance.Instance, dryRun bool) (bool, error) {
	st, err := migrate.LoadState(inst.Dir)
	if err != nil || st.Docker == nil {
		return false, err
	}
	m := st.Docker
	switched := m.SwitchedAt != nil && m.Dir != "" && instance.RealPath(m.Dir) != instance.RealPath(inst.Dir)
	if switched {
		if _, err := instance.Find(m.Dir); err != nil {
			switched = false
		}
	}
	if dryRun {
		if switched {
			app.UI.Info("The move to the new layout stopped after Fluxer started from %s.", m.Dir)
			app.UI.Plan("rexctl would do this", finishMovePlan(inst, m.Dir)...)
			app.UI.Info("Dry run. Nothing changed.")
			return true, nil
		}
		app.UI.Info("The move to the new layout stopped before it finished.")
		app.UI.Plan("rexctl would do this",
			ui.PlanItem{Verb: ui.PlanRemove, Text: "Remove the half-made instance in " + m.Dir},
			ui.PlanItem{Verb: ui.PlanRun, Text: "Start the old containers in " + inst.Dir + " again"},
		)
		return false, nil
	}
	if euid() != 0 && (m.Runtime == "" || m.Runtime == instance.RuntimeNative || rootOwned(inst)) {
		return false, ui.Refused("The move to the new layout stopped before it finished, and cleaning up after it needs root.", "Run "+sudoAgain(inst.Dir)+".")
	}
	var moved bool
	err = app.Locked(ctx, inst, func() error {
		if switched {
			moved = true
			return finishDockerMove(ctx, app, inst, st)
		}
		if err := guardOldMounts(ctx, app, inst); err != nil {
			return err
		}
		app.UI.Warn("The move to the new layout stopped before it finished.", "rexctl removes the half-made instance in "+m.Dir+" and starts the old containers again.")
		if m.Dir != "" && instance.RealPath(m.Dir) != instance.RealPath(inst.Dir) {
			t := &moveBase{app: app, inst: inst, dir: m.Dir, runtime: m.Runtime, units: m.Units}
			if err := t.Discard(ctx); err != nil {
				return ui.Wrap(err, "Couldn't remove the half-made instance in "+m.Dir+".", "Run "+migrateCommand+" --verbose to see why.")
			}
		}
		st.Docker = nil
		if err := st.Save(inst.Dir); err != nil {
			return ui.Wrap(err, "Couldn't record that the move to the new layout is undone.", "Check that you can write to "+inst.Dir+", then try again.")
		}
		return migrateStart(ctx, app, inst)
	})
	return moved, err
}

func finishMovePlan(inst *instance.Instance, dir string) []ui.PlanItem {
	return []ui.PlanItem{
		{Verb: ui.PlanKeep, Text: "Keep the new instance in " + dir, Detail: "it may already hold new messages"},
		{Verb: ui.PlanChange, Text: "Record the move in " + dir + " and " + inst.Dir},
		{Verb: ui.PlanChange, Text: "Move automatic upgrades and backup schedules to " + dir},
		{Verb: ui.PlanKeep, Text: "Keep the old containers in " + inst.Dir + " stopped", Detail: "they stay until rexctl migrate --clean-up"},
	}
}

func finishDockerMove(ctx context.Context, app *App, inst *instance.Instance, st migrate.State) error {
	dir := st.Docker.Dir
	app.UI.Warn(fmt.Sprintf("The move to the new layout stopped after Fluxer started from %s.", dir), "rexctl keeps that instance, because it may already hold new messages, and records the move.")
	carried := st
	carried.Docker = nil
	there, err := migrate.LoadState(dir)
	if err == nil {
		err = there.Merge(carried).Save(dir)
	}
	if err != nil {
		return ui.Wrap(err, "Couldn't record the move to the new layout in "+dir+".", "Check that you can write to "+dir+", then run "+migrateCommand+" again.")
	}
	markMoved(app, inst, dir)
	if moved, err := instance.Find(dir); err == nil && moved.State.MovedFrom != nil {
		moved.State.MovedFrom.AutoUpgrade = carrySchedules(ctx, app, inst, moved)
		if err := saveInstance(moved); err != nil {
			return ui.Wrap(err, "Couldn't record the move to the new layout in "+dir+".", "Check that you can write to "+dir+", then run "+migrateCommand+" again.")
		}
	}
	st.Docker = nil
	if err := st.Save(inst.Dir); err != nil {
		return ui.Wrap(err, "Couldn't record the move to the new layout in "+inst.Dir+".", "Check that you can write to "+inst.Dir+", then run "+migrateCommand+" again.")
	}
	app.UI.OK("Fluxer runs from %s in the new layout. The old containers in %s stay stopped.", dir, inst.Dir)
	app.UI.Info("To check the new instance, run %s.", runThere(dir, "status"))
	return nil
}

func migrateStart(ctx context.Context, app *App, inst *instance.Instance) error {
	if legacyCompose(inst) {
		if err := guardOldMounts(ctx, app, inst); err != nil {
			return err
		}
	}
	r, err := app.Runtime(ctx, inst)
	if err != nil {
		return err
	}
	if inst.State.Stopped {
		inst.State.Stopped = false
		if err := saveInstance(inst); err != nil {
			return err
		}
	}
	return app.UI.Step(ctx, "Starting Fluxer", func(ctx context.Context, s *ui.Step) error {
		if err := r.Start(ctx, s, rt.StartOptions{}); err != nil {
			return err
		}
		s.Done("Started Fluxer at %s.", inst.Origin())
		return nil
	})
}

func noticeReadOnly(ctx context.Context, app *App, inst *instance.Instance) {
	if inst.State.NotifyURL == "" {
		return
	}
	n := upgradeNotice{Instance: inst.Domain(), Status: noticeReadOnlyStatus, Release: inst.State.Release, Text: "Uploads are read-only after the move off SeaweedFS. Run " + migrateCommand + " --open-writes once images and files load."}
	n.fill()
	if err := postNotice(ctx, inst.State.NotifyURL, n); err != nil {
		app.UI.Detail("sending the read-only notice to %s: %v", maskURL(inst.State.NotifyURL), err)
	}
}

func showChoices(ctx context.Context, app *App, inst *instance.Instance, m migrate.Measurements, choices []migrateChoice, details bool) {
	paint := migrate.Paints(app.UI)
	var together *migrate.Diff
	if len(choices) > 1 {
		opts := make([]migrate.Option, len(choices))
		for i, c := range choices {
			opts[i] = c.opt
		}
		if d, err := combinedDiff(ctx, app, inst, m, opts); err == nil {
			together = &d
		}
	}
	migrate.Show(app.UI, migrate.SummaryLines(paint, diffsOf(choices), together))
	for _, c := range choices {
		migrate.Show(app.UI, c.diff.Lines(paint))
		if c.step == nil {
			app.UI.Info("  This release of rexctl can't make this change yet.")
		}
	}
	if together != nil {
		migrate.Show(app.UI, together.Lines(paint))
	}
	if details {
		migrate.Show(app.UI, migrate.BasisLines(paint, m))
	}
}

func dockerMoveArgs(newDir string) []string {
	args := []string{"migrate", "--to", runtimeSystemd}
	if newDir != "" {
		args = append(args, "--new-dir", newDir)
	}
	return args
}

func engineName(inst *instance.Instance) string {
	return engine.Kind(inst.State.Engine.Kind).Name()
}

func toOneContainer(inst *instance.Instance) string {
	return migrateCommand + " --to " + cmpDir(inst.State.Engine.Kind, string(engine.Docker))
}

func dockerBlocked(inst *instance.Instance) string {
	if !nativeSupported() {
		return "The move out of " + engineName(inst) + " works only on Linux with systemd."
	}
	if kind, name, ok := rootlessEngine(inst); ok {
		return fmt.Sprintf("The move out of %s needs %s running as root, and this instance runs on it without root under the account %s.", kind, kind, name)
	}
	return ""
}

func dockerMoveAllowed(app *App, inst *instance.Instance, newDir string, dryRun bool) error {
	if reason := dockerBlocked(inst); reason != "" {
		return ui.Refused(reason, "Move it to one container with "+toOneContainer(inst)+" instead.")
	}
	if dryRun || euid() == 0 {
		return nil
	}
	again := sudoRun(inst.Dir, dockerMoveArgs(newDir)...)
	if app.command == "rexctl migrate" {
		again = sudoAgain(inst.Dir)
	}
	return ui.Refused("The move to systemd runs as root.", "Run "+again+".")
}

func markMoved(app *App, inst *instance.Instance, to string) {
	if err := instance.Forget(instance.ScopeMachine, inst.Dir); err != nil {
		app.UI.Detail("%v", err)
	}
	moved, err := instance.Find(inst.Dir)
	if err == nil {
		handOverRules(moved, to)
		moved.State.MovedTo = to
		err = moved.Save()
	}
	if err != nil {
		app.UI.Detail("recording in %s that Fluxer moved to %s: %v", inst.Dir, to, err)
	}
}

func handOverRules(moved *instance.Instance, to string) {
	target, err := instance.Find(to)
	if err != nil {
		return
	}
	moved.State.Firewall = slices.DeleteFunc(moved.State.Firewall, func(p instance.OpenedPort) bool {
		return slices.Contains(target.State.Firewall, p)
	})
}

func migrateProgress(app *App, total int) func(migrate.Phase, migrate.Option) {
	var cur migrate.Option
	n := 0
	checked := false
	arrow := app.UI.Glyphs().Arrow
	finish := func() {
		if cur != "" {
			app.UI.OK("%s is done.", cur.Title())
			cur = ""
		}
	}
	return func(p migrate.Phase, o migrate.Option) {
		switch p {
		case migrate.PhaseMigrate, migrate.PhaseVerify, migrate.PhaseSwitch:
			if o != cur {
				finish()
				cur = o
				n++
				app.UI.Section(fmt.Sprintf("%s, change %d of %d", o.Title(), n, total))
			}
			app.UI.Info("  %s %s", arrow, phaseWords(p))
		case migrate.PhasePreflight:
			if !checked {
				checked = true
				app.UI.Info("%s", sentence(phaseWords(p)))
			}
		case migrate.PhaseRollback:
			cur = ""
			app.UI.Warn(fmt.Sprintf("Going back from %s.", o.Title()), "")
		case migrate.PhaseCheck:
			finish()
			if o == "" {
				app.UI.Info("%s", sentence(phaseWords(p)))
			}
		default:
			finish()
			app.UI.Info("%s", sentence(phaseWords(p)))
		}
	}
}

func phaseWords(p migrate.Phase) string {
	switch p {
	case migrate.PhasePreflight:
		return "checking first"
	case migrate.PhaseBackup:
		return "backing up"
	case migrate.PhaseStop:
		return "stopping writes"
	case migrate.PhaseMigrate:
		return "moving the data"
	case migrate.PhaseVerify:
		return "checking the data"
	case migrate.PhaseSwitch:
		return "switching over"
	case migrate.PhaseStart:
		return "starting"
	case migrate.PhaseCheck:
		return "checking that it works"
	case migrate.PhaseRollback:
		return "going back"
	}
	return string(p)
}

var errorPrefix = regexp.MustCompile(`^([a-z]+: )+`)

func migrateReason(err error) string {
	if err == nil {
		return ""
	}
	reason := strings.TrimPrefix(err.Error(), migrate.ErrNoNativeEquivalent.Error()+": ")
	return sentence(errorPrefix.ReplaceAllString(reason, ""))
}

func diffsOf(choices []migrateChoice) []migrate.Diff {
	out := make([]migrate.Diff, len(choices))
	for i, c := range choices {
		out[i] = c.diff
	}
	return out
}

const writesWarning = "Once uploads open, new files exist only as plain files on disk, and rexctl can no longer move this instance back to SeaweedFS."

func offerOpenWrites(ctx context.Context, app *App, inst *instance.Instance, h migrate.Host, st *migrate.State) (bool, error) {
	app.UI.Info("")
	app.UI.Info("Uploads are read-only for now. Open a few channels and check that images and files load.")
	app.UI.Info("%s", writesWarning)
	if !app.UI.Interactive() {
		app.UI.Info("When the files look right, run %s --open-writes. To go back to SeaweedFS instead, run %s --undo storage.", migrateCommand, migrateCommand)
		return false, nil
	}
	ok, err := app.UI.Confirm("Do images and files load? Open uploads now?", false)
	if err != nil || !ok {
		app.UI.Info("Run %s --open-writes when the files look right, or %s --undo storage to go back.", migrateCommand, migrateCommand)
		return false, err
	}
	return true, app.Locked(ctx, inst, func() error { return openWrites(ctx, app, h, st) })
}

func openWrites(ctx context.Context, app *App, h migrate.Host, st *migrate.State) error {
	err := migrate.OpenWrites(ctx, h, st, time.Now())
	if st.Storage.Open() {
		if serr := st.Save(h.Dir()); serr != nil {
			return errors.Join(err, serr)
		}
	}
	if err != nil {
		return migrateRefusal(err)
	}
	app.UI.OK("Uploads are open. SeaweedFS's data stays until you run %s --clean-up after a newer backup.", migrateCommand)
	return nil
}

func loadMigrations(inst *instance.Instance) (migrate.State, error) {
	st, err := migrate.LoadState(inst.Dir)
	if err != nil {
		return st, err
	}
	return st.Current(migrate.Done(runtimeOf(inst), migrateValues(inst))), nil
}

func forgetRestoredMigrations(inst *instance.Instance, made time.Time) error {
	st, err := migrate.LoadState(inst.Dir)
	if err != nil {
		return err
	}
	next := st.Restored(made)
	if next != st {
		if err := next.Save(inst.Dir); err != nil {
			return err
		}
	}
	if rt.KindOf(inst) != rt.Native {
		return nil
	}
	f, err := envfile.Load(filepath.Join(native.New(inst).Root(), nativeModesFile))
	if err != nil {
		return err
	}
	modes := f.Map()
	if inst.State.Settings == nil {
		inst.State.Settings = map[string]string{}
	}
	for _, m := range migrate.Modes {
		if v := strings.TrimSpace(modes[m.Env]); v != "" {
			inst.State.Settings[m.Key] = v
			delete(inst.State.Settings, m.Env)
		}
	}
	switch {
	case inst.State.Settings[settings.StorageBackend] != settings.StorageFilesystem:
		delete(inst.State.Settings, migrate.StorageReadOnlyVar)
	case next.Storage != nil && !next.Storage.Open():
		inst.State.Settings[migrate.StorageReadOnlyVar] = "true"
	}
	return saveInstance(inst)
}

func runOpenWrites(ctx context.Context, app *App, inst *instance.Instance) error {
	st, err := loadMigrations(inst)
	if err != nil {
		return err
	}
	switch {
	case st.Storage == nil:
		return migrateRefusal(migrate.ErrNotSwitched)
	case st.Storage.Open():
		return migrateRefusal(migrate.ErrAlreadyOpen)
	}
	app.UI.Info("%s", writesWarning)
	if ok, err := app.UI.Confirm("Open uploads now?", false); err != nil || !ok {
		return err
	}
	return app.Locked(ctx, inst, func() error {
		h, err := migrateHost(ctx, app, inst)
		if err != nil {
			return err
		}
		return openWrites(ctx, app, h, &st)
	})
}

func runUndo(ctx context.Context, app *App, inst *instance.Instance, o migrate.Option) error {
	if o != migrate.OptKV && o != migrate.OptStorage {
		return ui.Usage("--undo takes kv or storage. To leave the native runtime or one Rust process, restore the backup from before the change with rexctl restore.")
	}
	st, err := loadMigrations(inst)
	if err != nil {
		return err
	}
	what := map[migrate.Option]string{
		migrate.OptKV:      "This copies the data Fluxer keeps in Postgres back into Valkey and runs Fluxer on Valkey again.",
		migrate.OptStorage: "This deletes the plain files and runs Fluxer on SeaweedFS again, as it was before the change.",
	}[o]
	app.UI.Info("%s", what)
	if ok, err := app.UI.Confirm("Go back?", false); err != nil || !ok {
		return err
	}
	err = app.Locked(ctx, inst, func() error {
		h, err := migrateHost(ctx, app, inst)
		if err != nil {
			return err
		}
		if o == migrate.OptStorage {
			err = migrate.UndoStorage(ctx, h, &st)
		} else {
			err = migrate.UndoKV(ctx, h, &st, &migrate.Run{Log: migrateOutput(app)})
		}
		if serr := st.Save(inst.Dir); serr != nil {
			return errors.Join(err, serr)
		}
		return err
	})
	if err != nil {
		return migrateRefusal(err)
	}
	app.UI.OK("Went back to %s.", map[migrate.Option]string{migrate.OptKV: "Valkey", migrate.OptStorage: "SeaweedFS"}[o])
	return nil
}

func runCleanUp(ctx context.Context, app *App, inst *instance.Instance, confirm string) error {
	st, err := loadMigrations(inst)
	if err != nil {
		return err
	}
	list, err := backup.List(inst.BackupDir())
	if err != nil {
		return err
	}
	var newest time.Time
	for _, b := range list {
		if b.CreatedAt.After(newest) && !b.WithoutUploads() {
			newest = b.CreatedAt
		}
	}
	old := migrate.OldDataOf(st, newest)
	if len(old) == 0 {
		app.UI.OK("No old data is left from rexctl migrate.")
		return nil
	}
	return app.Locked(ctx, inst, func() error { return cleanUpOld(ctx, app, inst, &st, old, newest, confirm) })
}

func cleanUpOld(ctx context.Context, app *App, inst *instance.Instance, st *migrate.State, old []migrate.OldData, newest time.Time, confirm string) error {
	h, err := migrateHost(ctx, app, inst)
	if err != nil {
		return err
	}
	deleted := 0
	for _, d := range old {
		if !d.Ready {
			app.UI.Warn(fmt.Sprintf("rexctl keeps %s for now.", d.What), d.Wait)
			continue
		}
		if folder := keptFolder(h, d.What); folder != "" {
			if err := migrate.CleanUp(ctx, h, st, d, time.Now()); err != nil {
				return err
			}
			if err := st.Save(inst.Dir); err != nil {
				return err
			}
			app.UI.Info("%s is the folder %s on this machine, and rexctl doesn't delete folders you mounted. Delete it yourself once you no longer need it.", d.What, folder)
			deleted++
			continue
		}
		if err := app.UI.ConfirmDestructive(fmt.Sprintf("This deletes %s, which %s no longer uses. The backup from %s still has it.", d.What, d.Option.Title(), backupTime(newest)), inst.Domain(), confirm); err != nil {
			return err
		}
		if err := migrate.CleanUp(ctx, h, st, d, time.Now()); err != nil {
			return err
		}
		if err := st.Save(inst.Dir); err != nil {
			return err
		}
		app.UI.OK("Deleted %s.", d.What)
		deleted++
	}
	if deleted == 0 {
		return ui.Refused("rexctl kept all the old data, so nothing changed.", "Do what the lines above say, then run "+migrateCommand+" --clean-up again.")
	}
	if inst.State.MigrateBackup != "" && st.Docker == nil && !st.Storage.Pending() && !st.KV.Pending() {
		inst.State.MigrateBackup = ""
		return saveInstance(inst)
	}
	return nil
}

func keptFolder(h migrate.Host, what string) string {
	if ch, ok := h.(*composeHost); ok {
		return ch.dataFolder(what)
	}
	return ""
}

func remindMigrations(ctx context.Context, app *App, inst *instance.Instance) {
	st, err := loadMigrations(inst)
	if err != nil {
		return
	}
	var h migrate.Host
	if legacyCompose(inst) && len(migrate.OldDataOf(st, time.Time{})) > 0 {
		h, _ = migrateHost(ctx, app, inst)
	}
	if st.Storage != nil && !st.Storage.Open() {
		app.UI.Info("Uploads are still read-only after the move off SeaweedFS. Run %s --open-writes when the files look right.", migrateCommand)
	}
	for _, d := range migrate.OldDataOf(st, time.Time{}) {
		if folder := keptFolder(h, d.What); folder != "" {
			app.UI.Info("%s is still in %s on this machine. Once a newer backup exists, run %s --clean-up, then delete the folder yourself. rexctl doesn't delete folders you mounted.", d.What, folder, migrateCommand)
			continue
		}
		app.UI.Info("%s is still on disk. %s --clean-up removes it once a newer backup exists.", d.What, migrateCommand)
	}
}

func migrateRefusal(err error) error {
	switch {
	case errors.Is(err, migrate.ErrWritesOpen):
		return ui.Refused("Uploads opened after the move off SeaweedFS, so new files exist only as plain files.", "rexctl can't move them back. Restore the backup from before the change if you must go back, and accept losing newer uploads.")
	case errors.Is(err, migrate.ErrNotSwitched):
		return ui.Refused("rexctl migrate didn't make that change on this instance.", "Run "+migrateCommand+" --dry-run to see what's possible.")
	case errors.Is(err, migrate.ErrAlreadyOpen):
		return ui.Refused("Uploads are already open.", "")
	case errors.Is(err, migrate.ErrCleanedUp):
		return ui.Refused("The old Valkey data is already deleted.", "Restore the backup from before the change to go back.")
	case errors.Is(err, migrate.ErrUndoElsewhere):
		return ui.Refused("The move off SeaweedFS was made while Fluxer still ran in the old containers, and SeaweedFS's data stayed with them.", "Open uploads with "+migrateCommand+" --open-writes, or restore the backup from before the change to go back.")
	case errors.Is(err, migrate.ErrNoSeaweedFS):
		return ui.Refused("SeaweedFS's data is no longer on this machine, so rexctl keeps the plain files.", "Open uploads with "+migrateCommand+" --open-writes, or restore the backup from before the change to go back.")
	}
	return err
}

func migrateChoices(ctx context.Context, app *App, inst *instance.Instance, m migrate.Measurements) ([]migrateChoice, error) {
	runtime := runtimeOf(inst)
	done := migrate.Done(runtime, migrateValues(inst))
	before, err := migrateInventory(ctx, app, inst, runtime, done, inst.State.Settings)
	if err != nil {
		return nil, err
	}
	var out []migrateChoice
	for _, o := range migrate.All {
		if slices.Contains(done, o) || o == migrate.OptDocker {
			continue
		}
		to, next := migrate.Target(runtime, done, o)
		after, err := migrateInventory(ctx, app, inst, to, next, withOptions(inst.State.Settings, o))
		if err != nil {
			return nil, err
		}
		c := migrateChoice{opt: o, diff: migrate.BuildDiff(m, runtime, done, []migrate.Option{o}, before, after)}
		switch o {
		case migrate.OptKV:
			c.step = migrate.NewKVStep()
		case migrate.OptRust:
			c.step = migrate.NewRustStep()
		case migrate.OptStorage:
			c.step = migrate.NewStorageStep()
		}
		out = append(out, c)
	}
	return out, nil
}

func defaultNativeDir(inst *instance.Instance) string {
	if uid, ok := inst.OwnerUID(); ok && uid == 0 && instance.RootPath(inst.Dir+"-native", true) == "" {
		return inst.Dir + "-native"
	}
	if filepath.Clean(inst.Dir) == "/opt/fluxer" {
		return "/opt/fluxer-native"
	}
	return "/opt/fluxer"
}

func combinedDiff(ctx context.Context, app *App, inst *instance.Instance, m migrate.Measurements, opts []migrate.Option) (migrate.Diff, error) {
	runtime := runtimeOf(inst)
	done := migrate.Done(runtime, migrateValues(inst))
	before, err := migrateInventory(ctx, app, inst, runtime, done, inst.State.Settings)
	if err != nil {
		return migrate.Diff{}, err
	}
	to, next := migrate.Target(runtime, done, opts...)
	after, err := migrateInventory(ctx, app, inst, to, next, withOptions(inst.State.Settings, opts...))
	if err != nil {
		return migrate.Diff{}, err
	}
	return migrate.BuildDiff(m, runtime, done, opts, before, after), nil
}

func withOptions(chosen map[string]string, opts ...migrate.Option) map[string]string {
	changes := map[string]string{}
	for _, m := range migrate.Modes {
		if slices.Contains(opts, m.Option) {
			changes[m.Key] = m.Value
		}
	}
	return merged(chosen, changes)
}

func migrateInventory(ctx context.Context, app *App, inst *instance.Instance, runtime migrate.Runtime, done []migrate.Option, chosen map[string]string) (migrate.Inventory, error) {
	if runtime == migrate.Native {
		if runtimeOf(inst) == migrate.Compose {
			c, _ := migrate.ForNative(chosen)
			chosen = c.Settings
		}
		return migrate.NativeInventory(migrate.NativeChildren(done, migrate.NativeVoice(chosen))), nil
	}
	e, err := app.Engine(ctx, inst)
	if err != nil {
		return migrate.Inventory{}, err
	}
	p, err := composeModel(ctx, e, inst, chosen)
	if err != nil {
		return migrate.Inventory{}, err
	}
	return migrate.ComposeInventory(p.Model), nil
}

func composeModel(ctx context.Context, e *engine.Engine, inst *instance.Instance, chosen map[string]string) (*engine.Project, error) {
	return loadComposeModel(ctx, e, inst, chosen, false)
}

func customComposeModel(ctx context.Context, e *engine.Engine, inst *instance.Instance, chosen map[string]string) (*engine.Project, error) {
	return loadComposeModel(ctx, e, inst, chosen, true)
}

func loadComposeModel(ctx context.Context, e *engine.Engine, inst *instance.Instance, chosen map[string]string, custom bool) (*engine.Project, error) {
	set, err := deploy.ReleaseSet(inst)
	if err != nil {
		return nil, err
	}
	env, err := deploy.Environment(set, inst.State, chosen)
	if err != nil {
		return nil, err
	}
	tmp, err := os.MkdirTemp("", "rexctl-migrate-")
	if err != nil {
		return nil, err
	}
	defer os.RemoveAll(tmp)
	overlays := stack.OverlaysFor(chosen)
	if _, err := stack.Render(set, overlays, inst.State.Images.Refs, inst.Project(), tmp); err != nil {
		return nil, err
	}
	files := stack.ComposeFiles(tmp, overlays)
	if path := inst.Path(instance.CustomCompose); custom && fileExists(path) {
		files = append(files, path)
	}
	return e.Load(ctx, engine.Spec{Name: inst.Project(), Dir: inst.Dir, Files: files, Env: env})
}

func migrateHost(ctx context.Context, app *App, inst *instance.Instance) (migrate.Host, error) {
	set, err := deploy.ReleaseSet(inst)
	if err != nil {
		return nil, err
	}
	if rt.KindOf(inst) == rt.Native {
		return &nativeHost{app: app, inst: inst, n: native.New(inst), set: set}, nil
	}
	e, err := app.Engine(ctx, inst)
	if err != nil {
		return nil, err
	}
	p, err := deploy.Load(ctx, inst, e)
	if err != nil {
		return nil, err
	}
	return &composeHost{app: app, inst: inst, e: e, p: p, set: set}, nil
}

func merged(current, changes map[string]string) map[string]string {
	out := maps.Clone(current)
	if out == nil {
		out = map[string]string{}
	}
	for k, v := range changes {
		if v == "" {
			delete(out, k)
		} else {
			out[k] = v
		}
	}
	return out
}

func migrateOutput(app *App) io.Writer {
	if app.UI.Quiet() {
		return io.Discard
	}
	return app.UI.Stderr()
}

func offerMigrations(ctx context.Context, app *App) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	if rt.KindOf(inst) == rt.Container || len(migrate.Done(runtimeOf(inst), migrateValues(inst))) == len(migrate.All) {
		return nil
	}
	app.UI.Info("Fluxer can now run with fewer moving parts. Each change below is optional.")
	return runMigrate(ctx, app, &migrateFlags{})
}
