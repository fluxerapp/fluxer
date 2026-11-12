// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"cmp"
	"context"
	"errors"
	"fmt"
	"io"
	"maps"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/migrate"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	nativeModesFile    = "modes.env"
	nativeAppProxyURL  = "http://127.0.0.1:18084"
	composeAppProxyURL = "http://app-proxy:8080"
	composeVolumeLabel = "com.docker.compose.volume"
)

const migrateCommand = "rexctl migrate --experimental-native"

type migrateFlags struct {
	Native     bool
	Migrate    string
	DryRun     bool
	Details    bool
	NativeDir  string
	OpenWrites bool
	Undo       string
	CleanUp    bool
	Confirm    string
}

func newMigrateCmd(app *App) *cobra.Command {
	var f migrateFlags
	c := &cobra.Command{
		Use:   "migrate",
		Short: "Move your instance out of Docker, off Valkey or SeaweedFS, or onto one Rust process",
		Long: "Move your instance out of Docker, off Valkey or SeaweedFS, or onto one Rust process. " +
			"This is experimental, so rexctl migrate runs only with --experimental-native. " +
			"rexctl shows what each change saves and removes, backs up your instance first, moves the data, checks it and goes back if anything fails. " +
			"The move out of Docker runs as root, and so does every command for the new instance. " +
			"After the move off SeaweedFS, uploads stay read-only until you open them with --open-writes. " +
			"The old data stays until you remove it with --clean-up after a newer backup.",
		Args: noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runMigrate(c.Context(), app, &f)
		},
	}
	fl := c.Flags()
	fl.StringVar(&f.Migrate, "migrate", "", "the changes to make, separated by commas: docker, kv, rust and storage")
	fl.BoolVar(&f.Details, "details", false, "show the machine, versions, date and method behind the typical figures")
	fl.StringVar(&f.NativeDir, "native-dir", "", "the folder for the instance that runs without Docker, with --migrate docker")
	fl.BoolVar(&f.Native, "experimental-native", false, "turn on rexctl migrate, which is experimental")
	fl.BoolVar(&f.DryRun, "dry-run", false, "show what each change saves and removes without changing anything")
	fl.BoolVar(&f.OpenWrites, "open-writes", false, "let people upload again after the move off SeaweedFS. rexctl can't move the files back after this")
	fl.StringVar(&f.Undo, "undo", "", "go back from a change rexctl migrate made: kv, or storage until you open writes")
	fl.BoolVar(&f.CleanUp, "clean-up", false, "remove the Valkey or SeaweedFS data a change left behind, once a newer backup holds what replaced it")
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
	if inst.State.IsNative() {
		return migrate.Native
	}
	return migrate.Compose
}

func migrateValues(inst *instance.Instance) map[string]string {
	out := map[string]string{}
	if inst.State.IsNative() {
		if f, err := envfile.Load(filepath.Join(native.New(inst).Root(), nativeModesFile)); err == nil {
			maps.Copy(out, f.Map())
		}
	}
	maps.Copy(out, inst.State.Settings)
	return out
}

func runMigrate(ctx context.Context, app *App, f *migrateFlags) error {
	if !f.Native {
		return ui.Usage("rexctl migrate is experimental. Run it with --experimental-native.")
	}
	picked, err := migrate.Parse(f.Migrate)
	if err != nil {
		return ui.Usage(sentence(err.Error()))
	}
	inst, err := app.Instance()
	if err != nil {
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
	if slices.Contains(picked, migrate.OptDocker) && runtimeOf(inst) == migrate.Compose && !f.DryRun {
		if err := dockerMoveAllowed(app, inst, f.NativeDir); err != nil {
			return err
		}
	}
	m, err := migrate.LoadMeasurements()
	if err != nil {
		return err
	}
	choices, err := migrateChoices(ctx, app, inst, f.NativeDir, m)
	if err != nil {
		return err
	}
	if len(choices) == 0 {
		app.UI.OK("Every change is already made on this instance.")
		remindMigrations(app, inst)
		return nil
	}
	for _, o := range picked {
		if !slices.ContainsFunc(choices, func(c migrateChoice) bool { return c.opt == o }) {
			return ui.Refused(fmt.Sprintf("%s is already done on this instance, or doesn't apply to it.", o.Title()), "Run "+migrateCommand+" --dry-run to see the changes it can make.")
		}
	}
	paint := migrate.Paints(app.UI)
	if f.DryRun || (len(picked) == 0 && !app.UI.Interactive()) {
		showChoices(ctx, app, inst, m, choices, f.Details, f.NativeDir)
		if !f.DryRun {
			app.UI.Info("")
			app.UI.Info("Pass --migrate with the changes you want, such as --migrate %s.", choices[0].opt)
		}
		return nil
	}
	skipped := ""
	if len(picked) == 0 {
		migrate.Show(app.UI, migrate.SummaryLines(paint, diffsOf(choices), nil))
		for _, c := range choices {
			migrate.Show(app.UI, c.diff.Lines(paint))
			app.UI.Info("")
			if c.opt == migrate.OptDocker {
				if skipped = dockerNote(inst, f.NativeDir); skipped != "" {
					app.UI.Info("%s", skipped)
					app.UI.Info("")
					continue
				}
			}
			ok, err := app.UI.Confirm("Make this change?", false)
			if err != nil {
				return err
			}
			if ok {
				picked = append(picked, c.opt)
			}
		}
		if f.Details {
			migrate.Show(app.UI, migrate.BasisLines(paint, m))
		}
		if len(picked) == 0 {
			app.UI.Info("Nothing changed.")
			if skipped != "" {
				app.UI.Info("%s", skipped)
			}
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
	if !app.UI.Interactive() && !app.UI.Yes() {
		return ui.MissingFlag("yes")
	}
	host, err := migrateHost(ctx, app, inst)
	if err != nil {
		return err
	}
	r := &migrate.Run{Host: host, Log: migrateOutput(app), OnPhase: func(p migrate.Phase, o migrate.Option) {
		if o == "" {
			app.UI.Info("%s", sentence(phaseWords(p)))
			return
		}
		app.UI.Info("%s: %s", o.Title(), phaseWords(p))
	}}
	err = migrate.Execute(ctx, r, steps)
	if err == nil && slices.Contains(picked, migrate.OptDocker) {
		markMoved(app, inst, r.Host.Dir())
	}
	giveFilesBack(app, inst)
	var fail *migrate.Failure
	var refused *ui.ExitError
	switch {
	case errors.As(err, &fail) && fail.Rollback == nil && r.Backup != "":
		return ui.Wrap(err, "The change didn't work, and rexctl put your instance back as it was. "+migrateReason(fail.Err), "The backup from before the change is "+r.Backup+".")
	case errors.As(err, &fail) && r.Backup != "":
		return ui.Wrap(err, "The change didn't work, and rexctl couldn't put everything back. "+migrateReason(fail.Err), "Restore the backup from before the change with rexctl restore "+r.Backup+".")
	case errors.As(err, &fail) && fail.Phase == migrate.PhasePreflight && errors.As(fail.Err, &refused):
		return refused
	case errors.As(err, &fail) && fail.Phase == migrate.PhasePreflight:
		return &ui.ExitError{Code: ui.ExitRefused, Msg: "rexctl didn't change anything.", Fix: migrateReason(fail.Err), Err: err}
	case errors.As(err, &fail):
		return ui.Wrap(err, "rexctl didn't change anything.", migrateReason(fail.Err))
	case err != nil:
		return ui.Wrap(err, "rexctl didn't change anything.", migrateReason(err))
	}
	st, err := migrate.LoadState(r.Host.Dir())
	if err != nil {
		return err
	}
	st = st.Merge(r.State)
	if err := st.Save(r.Host.Dir()); err != nil {
		return ui.Wrap(err, "Made the changes, but couldn't record them.", "Check that you can write to "+r.Host.Dir()+".")
	}
	for _, n := range r.Notes {
		app.UI.Info("%s", n)
	}
	app.UI.OK("Made the changes. The backup from before them is %s.", r.Backup)
	if slices.Contains(picked, migrate.OptDocker) {
		app.UI.Info("Fluxer now runs from %s without Docker, and root manages it. The Docker instance in %s is stopped and stays as it was, and rexctl won't start it again while the new instance is there.", r.Host.Dir(), inst.Dir)
		app.UI.Info("To check the new instance, run %s.", sudoRun(r.Host.Dir(), "status"))
	}
	if skipped != "" {
		app.UI.Info("%s", skipped)
	}
	if r.State.Storage != nil {
		return offerOpenWrites(ctx, app, r.Host, &st)
	}
	return nil
}

func showChoices(ctx context.Context, app *App, inst *instance.Instance, m migrate.Measurements, choices []migrateChoice, details bool, nativeDir string) {
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
		if c.opt != migrate.OptDocker {
			continue
		}
		if note := dockerNote(inst, nativeDir); note != "" {
			app.UI.Info("  %s", note)
		}
	}
	if together != nil {
		migrate.Show(app.UI, together.Lines(paint))
	}
	if details {
		migrate.Show(app.UI, migrate.BasisLines(paint, m))
	}
}

func dockerMoveArgs(nativeDir string) []string {
	args := []string{"migrate", "--experimental-native", "--migrate", "docker"}
	if nativeDir != "" {
		args = append(args, "--native-dir", nativeDir)
	}
	return args
}

func dockerBlocked(inst *instance.Instance) string {
	if !nativeSupported() {
		return "The move out of Docker works only on Linux with systemd."
	}
	if kind, name, ok := rootlessEngine(inst); ok {
		return fmt.Sprintf("The move out of Docker needs Docker or Podman running as root, and this instance runs on %s without root under the account %s.", kind, name)
	}
	return ""
}

func dockerNote(inst *instance.Instance, nativeDir string) string {
	if reason := dockerBlocked(inst); reason != "" {
		return reason
	}
	if euid() == 0 {
		return ""
	}
	return "The move out of Docker runs as root. To make it, run " + sudoRun(inst.Dir, dockerMoveArgs(nativeDir)...) + "."
}

func dockerMoveAllowed(app *App, inst *instance.Instance, nativeDir string) error {
	if reason := dockerBlocked(inst); reason != "" {
		return ui.Refused(reason, "The other changes still work. Run "+migrateCommand+" --dry-run to see them.")
	}
	if euid() == 0 {
		return nil
	}
	again := sudoRun(inst.Dir, dockerMoveArgs(nativeDir)...)
	if app.command == "rexctl migrate" {
		again = sudoAgain(inst.Dir)
	}
	return ui.Refused("The move out of Docker runs as root.", "Run "+again+".")
}

func markMoved(app *App, inst *instance.Instance, to string) {
	if err := instance.Forget(instance.ScopeMachine, inst.Dir); err != nil {
		app.UI.Detail("%v", err)
	}
	moved, err := instance.Find(inst.Dir)
	if err == nil {
		moved.State.MovedTo = to
		err = moved.Save()
	}
	if err != nil {
		app.UI.Detail("recording in %s that Fluxer moved to %s: %v", inst.Dir, to, err)
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

func offerOpenWrites(ctx context.Context, app *App, h migrate.Host, st *migrate.State) error {
	app.UI.Info("")
	app.UI.Info("Uploads are read-only for now. Open a few channels and check that images and files load.")
	app.UI.Info("%s", writesWarning)
	if !app.UI.Interactive() {
		app.UI.Info("When the files look right, run %s --open-writes. To go back to SeaweedFS instead, run %s --undo storage.", migrateCommand, migrateCommand)
		return nil
	}
	ok, err := app.UI.Confirm("Open uploads now?", false)
	if err != nil || !ok {
		app.UI.Info("Run %s --open-writes when the files look right, or %s --undo storage to go back.", migrateCommand, migrateCommand)
		return err
	}
	return openWrites(ctx, app, h, st)
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
	if !inst.State.IsNative() {
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
	return saveNative(inst)
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
	h, err := migrateHost(ctx, app, inst)
	if err != nil {
		return err
	}
	return openWrites(ctx, app, h, &st)
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
	h, err := migrateHost(ctx, app, inst)
	if err != nil {
		return err
	}
	for _, d := range old {
		if !d.Ready {
			app.UI.Warn(fmt.Sprintf("rexctl keeps %s for now.", d.What), d.Wait)
			continue
		}
		if err := app.UI.ConfirmDestructive(fmt.Sprintf("This deletes %s, which %s no longer uses. The backup from %s still has it.", d.What, d.Option.Title(), backupTime(newest)), inst.Domain(), confirm); err != nil {
			return err
		}
		if err := migrate.CleanUp(ctx, h, &st, d, time.Now()); err != nil {
			return err
		}
		if err := st.Save(inst.Dir); err != nil {
			return err
		}
		app.UI.OK("Deleted %s.", d.What)
	}
	return nil
}

func remindMigrations(app *App, inst *instance.Instance) {
	st, err := loadMigrations(inst)
	if err != nil {
		return
	}
	if st.Storage != nil && !st.Storage.Open() {
		app.UI.Info("Uploads are still read-only after the move off SeaweedFS. Run %s --open-writes when the files look right.", migrateCommand)
	}
	for _, d := range migrate.OldDataOf(st, time.Time{}) {
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
		return ui.Refused("The move off SeaweedFS was made while Fluxer still ran in Docker, and SeaweedFS's data stayed with the Docker instance.", "Open uploads with "+migrateCommand+" --open-writes, or restore the backup from before the change to go back.")
	case errors.Is(err, migrate.ErrNoSeaweedFS):
		return ui.Refused("SeaweedFS's data is no longer on this machine, so rexctl keeps the plain files.", "Open uploads with "+migrateCommand+" --open-writes, or restore the backup from before the change to go back.")
	}
	return err
}

func migrateChoices(ctx context.Context, app *App, inst *instance.Instance, nativeDir string, m migrate.Measurements) ([]migrateChoice, error) {
	runtime := runtimeOf(inst)
	done := migrate.Done(runtime, migrateValues(inst))
	before, err := migrateInventory(ctx, app, inst, runtime, done, inst.State.Settings)
	if err != nil {
		return nil, err
	}
	recorded, err := loadMigrations(inst)
	if err != nil {
		return nil, err
	}
	var out []migrateChoice
	for _, o := range migrate.All {
		if slices.Contains(done, o) || (o == migrate.OptDocker && runtime != migrate.Compose) {
			continue
		}
		to, next := migrate.Target(runtime, done, o)
		after, err := migrateInventory(ctx, app, inst, to, next, withOptions(inst.State.Settings, o))
		if err != nil {
			return nil, err
		}
		c := migrateChoice{opt: o, diff: migrate.BuildDiff(m, runtime, done, []migrate.Option{o}, before, after)}
		switch o {
		case migrate.OptDocker:
			c.step = migrate.NewDockerStep(&nativeTarget{app: app, inst: inst, dir: cmp.Or(nativeDir, defaultNativeDir(inst))}, recorded)
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
	return e.Load(ctx, engine.Spec{Name: inst.Project(), Dir: inst.Dir, Files: stack.ComposeFiles(tmp, overlays), Env: env})
}

func migrateHost(ctx context.Context, app *App, inst *instance.Instance) (migrate.Host, error) {
	set, err := deploy.ReleaseSet(inst)
	if err != nil {
		return nil, err
	}
	if inst.State.IsNative() {
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

func checkObjectsFS(path string) error {
	if r := checks.ObjectsFS(path); r.Status == checks.Fail {
		return errors.New(r.Message)
	}
	return nil
}

type composeHost struct {
	app     *App
	inst    *instance.Instance
	e       *engine.Engine
	p       *engine.Project
	set     *stack.Set
	objects *engine.Project
}

func (h *composeHost) Runtime() migrate.Runtime                             { return migrate.Compose }
func (h *composeHost) Dir() string                                          { return h.inst.Dir }
func (h *composeHost) Settings() map[string]string                          { return maps.Clone(h.inst.State.Settings) }
func (h *composeHost) AppProxyURL() string                                  { return composeAppProxyURL }
func (h *composeHost) ObjectsRoot() string                                  { return stack.ObjectsRoot }
func (h *composeHost) FreePorts(context.Context, []int) error               { return nil }
func (h *composeHost) RetireValkey(context.Context, string) (string, error) { return "", nil }
func (h *composeHost) ReviveValkey(context.Context, string) error           { return nil }

func (h *composeHost) Backup(ctx context.Context) (string, error) {
	info, err := backup.Create(ctx, h.inst, h.p, backup.CreateOptions{Reason: "migrate"})
	if err != nil {
		return "", err
	}
	return info.Path, nil
}

func (h *composeHost) present(services []string) []string {
	have := h.p.Services()
	return slices.DeleteFunc(slices.Clone(services), func(s string) bool { return !slices.Contains(have, s) })
}

func (h *composeHost) Stop(ctx context.Context, services []string) error {
	if s := h.present(services); len(s) > 0 {
		return h.p.Stop(ctx, s...)
	}
	return nil
}

func (h *composeHost) Halt(ctx context.Context) error {
	if err := h.e.Named(h.inst.Project(), h.inst.Dir).Stop(ctx); err != nil {
		return err
	}
	h.inst.State.Stopped = true
	return h.inst.Save()
}

func (h *composeHost) Start(ctx context.Context) error { return runStart(ctx, h.app) }

func (h *composeHost) StartOnly(ctx context.Context, services []string) error {
	return h.p.Up(ctx, engine.UpOptions{Services: h.present(services), NoDeps: true, Wait: true})
}

func (h *composeHost) Remove(ctx context.Context, services []string) error {
	if s := h.present(services); len(s) > 0 {
		return h.p.Remove(ctx, s...)
	}
	return nil
}

func (h *composeHost) Check(ctx context.Context) error {
	return h.p.WaitReady(ctx, engine.DefaultReadyTimeout, nil)
}

func (h *composeHost) Services(ctx context.Context) ([]migrate.Service, error) {
	containers, err := h.p.PS(ctx)
	if err != nil {
		return nil, err
	}
	out := make([]migrate.Service, 0, len(containers))
	for _, c := range containers {
		out = append(out, migrate.Service{Name: c.Service, Running: c.State == "running", Health: c.Health})
	}
	return out, nil
}

func (h *composeHost) Apply(ctx context.Context, changes map[string]string) error {
	st := h.inst.State
	st.Settings = merged(st.Settings, changes)
	r, err := h.inst.Render(h.set, stack.OverlaysFor(st.Settings), st.Images.Refs)
	if err != nil {
		return err
	}
	st.Rendered = r.Hashes
	if err := h.inst.Save(); err != nil {
		return err
	}
	p, err := deploy.Load(ctx, h.inst, h.e)
	if err != nil {
		return err
	}
	h.p, h.objects = p, nil
	return nil
}

func (h *composeHost) Env(changes map[string]string) (map[string]string, error) {
	return deploy.Environment(h.set, h.inst.State, merged(h.inst.State.Settings, changes))
}

func (h *composeHost) CheckTarget(ctx context.Context, changes map[string]string) error {
	_, err := composeModel(ctx, h.e, h.inst, merged(h.inst.State.Settings, changes))
	return err
}

func (h *composeHost) objectsProject(ctx context.Context) (*engine.Project, error) {
	if h.objects != nil {
		return h.objects, nil
	}
	p, err := composeModel(ctx, h.e, h.inst, withOptions(h.inst.State.Settings, migrate.OptStorage))
	if err != nil {
		return nil, err
	}
	h.objects = p
	return p, nil
}

func (h *composeHost) CheckObjects(ctx context.Context) error {
	if err := checkObjectsFS(engine.Stores(h.inst.Dir, h.e, "").Data); err != nil {
		return err
	}
	vols, err := h.e.Volumes(ctx, h.inst.Project())
	if err != nil {
		return err
	}
	for _, v := range vols {
		if v.Labels[composeVolumeLabel] == stack.SeaweedFSVolume && v.Mountpoint != "" {
			if err := migrate.CheckSpace(v.Mountpoint, v.Mountpoint); err != nil && !errors.Is(err, os.ErrPermission) {
				return err
			}
		}
	}
	return nil
}

func (h *composeHost) PrepareObjects(ctx context.Context) error {
	p, err := h.objectsProject(ctx)
	if err != nil {
		return err
	}
	svc, ok := p.Model.Services[stack.ObjectsInitService]
	if !ok {
		return fmt.Errorf("the release has no %s service", stack.ObjectsInitService)
	}
	o := p.HelperFor(stack.ObjectsInitService)
	o.Entrypoint = svc.Entrypoint
	o.User = svc.User
	o.Mounts = append(o.Mounts, engine.Mount{Volume: stack.ObjectsVolume, Target: stack.ObjectsRoot})
	o.Stderr = migrateOutput(h.app)
	return p.Helper(ctx, o)
}

func (h *composeHost) dataVolume(ctx context.Context, what string) (string, error) {
	vols, err := h.e.Volumes(ctx, h.inst.Project())
	if err != nil {
		return "", err
	}
	for _, v := range vols {
		if v.Labels[composeVolumeLabel] == what || v.Name == h.inst.Project()+"_"+what {
			return v.Name, nil
		}
	}
	return "", nil
}

func (h *composeHost) HoldsData(ctx context.Context, what string) (bool, error) {
	name, err := h.dataVolume(ctx, what)
	return name != "", err
}

func (h *composeHost) DropData(ctx context.Context, what string) error {
	name, err := h.dataVolume(ctx, what)
	if err != nil || name == "" {
		return err
	}
	return h.e.RemoveVolume(ctx, name)
}

func (h *composeHost) Importer() migrate.Importer { return composeImporter{h} }

type composeImporter struct{ h *composeHost }

func (c composeImporter) Run(ctx context.Context, j migrate.Job) error {
	imp := migrate.ComposeImporter{Project: c.h.p}
	if j.Objects {
		p := c.h.p
		if !stack.FilesystemStorage(c.h.inst.State.Settings) {
			var err error
			if p, err = c.h.objectsProject(ctx); err != nil {
				return err
			}
		}
		imp.Objects = p
	}
	return imp.Run(ctx, j)
}

type nativeHost struct {
	app  *App
	inst *instance.Instance
	n    *native.Stack
	set  *stack.Set
}

func (h *nativeHost) Runtime() migrate.Runtime             { return migrate.Native }
func (h *nativeHost) Dir() string                          { return h.inst.Dir }
func (h *nativeHost) Settings() map[string]string          { return maps.Clone(h.inst.State.Settings) }
func (h *nativeHost) AppProxyURL() string                  { return nativeAppProxyURL }
func (h *nativeHost) ObjectsRoot() string                  { return filepath.Join(h.n.Root(), "objects") }
func (h *nativeHost) PrepareObjects(context.Context) error { return nil }
func (h *nativeHost) Importer() migrate.Importer {
	return migrate.NativeImporter{Stack: h.n, Set: h.set}
}

func (h *nativeHost) Backup(ctx context.Context) (string, error) {
	return nativeBackup(ctx, h.app, h.inst, h.n, h.set, "Backing up your instance", "migrate")
}

func (h *nativeHost) running(ctx context.Context) bool {
	return h.n.OwnsUnit(ctx) && h.n.ActiveState(ctx) == "active"
}

func (h *nativeHost) live(ctx context.Context) []string {
	if !h.running(ctx) {
		return nil
	}
	entries, err := h.n.Entries(ctx)
	if err != nil {
		return nil
	}
	var out []string
	for _, e := range entries {
		out = append(out, e.Name)
	}
	return out
}

func (h *nativeHost) Stop(ctx context.Context, services []string) error {
	live := h.live(ctx)
	single := h.n.RustSingle()
	var stop []string
	for _, s := range services {
		if name := stack.RunsIn(s, single); slices.Contains(live, name) && !slices.Contains(stop, name) {
			stop = append(stop, name)
		}
	}
	if len(stop) == 0 {
		return nil
	}
	return h.n.Stop(ctx, stop...)
}

func (h *nativeHost) Remove(ctx context.Context, services []string) error {
	return h.Stop(ctx, services)
}

func (h *nativeHost) Halt(ctx context.Context) error { return h.n.Stop(ctx) }

func (h *nativeHost) Start(ctx context.Context) error { return runNativeStart(ctx, h.app, h.inst) }

func (h *nativeHost) StartOnly(ctx context.Context, _ []string) error { return h.Start(ctx) }

func (h *nativeHost) Check(ctx context.Context) error {
	return h.app.UI.Step(ctx, "Checking Fluxer", func(ctx context.Context, s *ui.Step) error {
		return waitForNative(ctx, h.n, s, "Starting")
	})
}

func (h *nativeHost) Services(ctx context.Context) ([]migrate.Service, error) {
	if !h.running(ctx) {
		return nil, nil
	}
	entries, err := h.n.Entries(ctx)
	if err != nil {
		return nil, err
	}
	var out []migrate.Service
	for _, e := range entries {
		running := e.State == "running" || e.State == "starting"
		out = append(out, migrate.Service{Name: e.Name, Running: running, Health: e.Health})
		for _, w := range e.Watch {
			out = append(out, migrate.Service{Name: e.Name + "/" + w.Name, Running: running, Health: w.Health})
		}
	}
	return out, nil
}

func (h *nativeHost) Apply(_ context.Context, changes map[string]string) error {
	h.inst.State.Settings = merged(h.inst.State.Settings, changes)
	return saveNative(h.inst)
}

func (h *nativeHost) Env(changes map[string]string) (map[string]string, error) {
	return deploy.Environment(h.set, h.inst.State, merged(h.inst.State.Settings, changes))
}

func (h *nativeHost) CheckTarget(_ context.Context, changes map[string]string) error {
	if changes[settings.RustProcess] == settings.RustSingle {
		bin := filepath.Join(h.n.Current(), "bin", "fluxer-server")
		if _, err := os.Stat(bin); err != nil {
			return fmt.Errorf("this Fluxer release has no %s: %w", bin, err)
		}
	}
	return nil
}

func (h *nativeHost) FreePorts(_ context.Context, ports []int) error {
	var busy []string
	for _, p := range ports {
		l, err := net.Listen("tcp", net.JoinHostPort("127.0.0.1", strconv.Itoa(p)))
		if err != nil {
			busy = append(busy, strconv.Itoa(p))
			continue
		}
		l.Close()
	}
	if len(busy) > 0 {
		return fmt.Errorf("ports %s are still in use", strings.Join(busy, ", "))
	}
	return nil
}

func (h *nativeHost) CheckObjects(context.Context) error {
	root := h.ObjectsRoot()
	if err := checkObjectsFS(root); err != nil {
		return err
	}
	if err := migrate.CheckObjectsDir(root); err != nil {
		return err
	}
	dir, _ := h.n.DataDir(stack.SeaweedFSVolume)
	return migrate.CheckSpace(dir, root)
}

func (h *nativeHost) RetireValkey(ctx context.Context, suffix string) (string, error) {
	dir, _ := h.n.DataDir(stack.ValkeyVolume)
	if _, err := os.Stat(dir); errors.Is(err, os.ErrNotExist) {
		return "", nil
	}
	if err := h.Stop(ctx, []string{migrate.ValkeyService}); err != nil {
		return "", err
	}
	name := migrate.ValkeyRetiredPrefix + suffix
	return name, os.Rename(dir, filepath.Join(h.n.Root(), name))
}

func (h *nativeHost) ReviveValkey(_ context.Context, name string) error {
	dir, _ := h.n.DataDir(stack.ValkeyVolume)
	if entries, err := os.ReadDir(dir); err == nil && len(entries) > 0 {
		return fmt.Errorf("%s already holds data", dir)
	}
	if err := os.Remove(dir); err != nil && !errors.Is(err, os.ErrNotExist) {
		return err
	}
	return os.Rename(filepath.Join(h.n.Root(), filepath.Base(name)), dir)
}

func (h *nativeHost) HoldsData(_ context.Context, what string) (bool, error) {
	dir, ok := h.n.DataDir(what)
	if !ok {
		return false, fmt.Errorf("rexctl doesn't know %s", what)
	}
	entries, err := os.ReadDir(dir)
	if errors.Is(err, os.ErrNotExist) {
		return false, nil
	}
	if err != nil {
		return false, err
	}
	return slices.ContainsFunc(entries, func(e os.DirEntry) bool { return e.Name() != "lost+found" }), nil
}

func (h *nativeHost) DropData(_ context.Context, what string) error {
	dir, ok := h.n.DataDir(what)
	if !ok {
		if !strings.HasPrefix(what, migrate.ValkeyRetiredPrefix) || filepath.Base(what) != what {
			return fmt.Errorf("rexctl doesn't delete %s", what)
		}
		return os.RemoveAll(filepath.Join(h.n.Root(), what))
	}
	entries, err := os.ReadDir(dir)
	if errors.Is(err, os.ErrNotExist) {
		return nil
	}
	if err != nil {
		return err
	}
	for _, e := range entries {
		if e.Name() == "lost+found" {
			continue
		}
		if err := os.RemoveAll(filepath.Join(dir, e.Name())); err != nil {
			return err
		}
	}
	return nil
}

const nativeMigrateFix = "Keep this instance in Docker for now, and try again after the next Fluxer release."

type nativeTarget struct {
	app      *App
	inst     *instance.Instance
	dir      string
	chosen   map[string]string
	unitPath string
	unit     []byte
}

func (t *nativeTarget) Dir() string { return t.dir }

func (t *nativeTarget) Preflight(ctx context.Context, chosen map[string]string) error {
	t.chosen = chosen
	if err := nativeRoot(t.inst.Dir); err != nil {
		return err
	}
	if err := nativeFolder(t.dir, "--native-dir"); err != nil {
		return err
	}
	t.dir = instance.RealPath(t.dir)
	if err := nativeAlone(ctx); err != nil {
		return err
	}
	database, uploads := backup.ExternalData(t.inst)
	switch {
	case database:
		return ui.Refused("Fluxer uses your own Postgres server, and a native instance runs its own.", "Keep this instance in Docker.")
	case uploads:
		return ui.Refused("Fluxer keeps uploads in your own object store at storage.endpoint, and a native instance keeps them on this machine.", "Keep this instance in Docker.")
	}
	if err := t.nativeEquivalent(ctx, chosen); err != nil {
		return err
	}
	entries, err := os.ReadDir(t.dir)
	if err == nil && len(entries) > 0 {
		return ui.Refused(fmt.Sprintf("The folder for the native instance, %s, already has files in it.", t.dir), "Pass --native-dir with an empty or new folder that root owns.")
	}
	if err != nil && !errors.Is(err, os.ErrNotExist) {
		return err
	}
	_, err = nativeRelease(ctx, stack.Embedded().Release, nativeMigrateFix)
	return err
}

func (t *nativeTarget) nativeEquivalent(ctx context.Context, chosen map[string]string) error {
	custom, err := t.inst.Custom()
	if err != nil {
		return err
	}
	if slices.ContainsFunc(strings.Split(string(custom), "\n"), func(line string) bool {
		line = strings.TrimSpace(line)
		return line != "" && !strings.HasPrefix(line, "#")
	}) {
		return fmt.Errorf("%w: %s changes the Docker services, and a native instance has none. Move what it does into rexctl config set, then remove it", migrate.ErrNoNativeEquivalent, instance.CustomCompose)
	}
	if len(t.inst.State.Voice) > 0 {
		ids := make([]string, len(t.inst.State.Voice))
		for i, v := range t.inst.State.Voice {
			ids[i] = v.ID
		}
		return fmt.Errorf("%w: the voice servers %s run in Docker on this machine. Remove them with rexctl voice remove first", migrate.ErrNoNativeEquivalent, strings.Join(ids, ", "))
	}
	if mode := chosen[settings.EdgeMode]; mode != "" && mode != settings.EdgeBundled {
		if _, err := native.ProxyListen(chosen[settings.EdgeBind]); err != nil {
			return fmt.Errorf("%w: edge.bind is %s, and a native instance behind a proxy listens on port %s. Run rexctl config set edge.bind=127.0.0.1:%s first and point your proxy there", migrate.ErrNoNativeEquivalent, chosen[settings.EdgeBind], native.ProxyPort, native.ProxyPort)
		}
	}
	a, err := autoUpgradeFor(t.inst, nil)
	if err != nil {
		return err
	}
	if a.Scheduler != "" {
		if st, err := host.CheckAutoUpgrade(ctx, a, time.Now()); err == nil && st.On {
			return fmt.Errorf("%w: automatic upgrades are on, and they upgrade the Docker instance. Run rexctl auto-upgrade off first", migrate.ErrNoNativeEquivalent)
		}
	}
	return nil
}

func (t *nativeTarget) Install(ctx context.Context) error {
	if err := t.self(ctx, "install", "--experimental-native", "--dir", t.dir, "--domain", t.inst.Domain(), "--setup", "browser", "--no-start", "--yes"); err != nil {
		return fmt.Errorf("installing the native instance in %s: %w", t.dir, err)
	}
	target, err := instance.Find(t.dir)
	if err != nil {
		return err
	}
	target.State.Settings = merged(target.State.Settings, t.chosen)
	if err := saveNative(target); err != nil {
		return err
	}
	if t.unitPath, err = (host.BootUnit{Project: target.Project()}).Path(); err != nil {
		return err
	}
	if t.unit, err = os.ReadFile(t.unitPath); err != nil && !errors.Is(err, os.ErrNotExist) {
		return err
	}
	return nil
}

func (t *nativeTarget) Restore(ctx context.Context, archive string) error {
	target, err := instance.Find(t.dir)
	if err != nil {
		return err
	}
	set, err := deploy.ReleaseSet(target)
	if err != nil {
		return err
	}
	n := native.New(target)
	if err := n.Restore(ctx, set, archive, migrateOutput(t.app)); err != nil {
		return fmt.Errorf("restoring %s into %s: %w", archive, t.dir, err)
	}
	return adoptNativeSecrets(target, n)
}

func (t *nativeTarget) Open(ctx context.Context, chosen map[string]string) (migrate.Host, error) {
	target, err := instance.Find(t.dir)
	if err != nil {
		return nil, err
	}
	if !target.State.IsNative() {
		return nil, fmt.Errorf("%s holds no native instance after the restore", t.dir)
	}
	target.State.Settings = merged(target.State.Settings, chosen)
	target.State.BackupDir = t.inst.State.BackupDir
	target.State.KeepBackups = t.inst.State.KeepBackups
	target.State.AdminImport = t.inst.State.AdminImport
	if err := saveNative(target); err != nil {
		return nil, err
	}
	return migrateHost(ctx, t.app, target)
}

func (t *nativeTarget) Discard(ctx context.Context) error {
	if target, err := instance.Find(t.dir); err == nil && native.New(target).OwnsUnit(ctx) {
		if err := t.self(ctx, "stop", "--dir", t.dir); err != nil {
			return err
		}
	}
	if err := t.restoreUnit(ctx); err != nil {
		return err
	}
	t.app.forget(t.dir)
	return os.RemoveAll(t.dir)
}

func (t *nativeTarget) restoreUnit(ctx context.Context) error {
	if t.unitPath == "" {
		return nil
	}
	current, err := os.ReadFile(t.unitPath)
	if errors.Is(err, os.ErrNotExist) && t.unit == nil {
		return nil
	}
	if err != nil && !errors.Is(err, os.ErrNotExist) {
		return err
	}
	if bytes.Equal(current, t.unit) {
		return nil
	}
	if t.unit == nil {
		err = os.Remove(t.unitPath)
	} else {
		err = envfile.WriteFileAtomic(t.unitPath, t.unit, false)
	}
	if err != nil {
		return err
	}
	return exec.CommandContext(ctx, "systemctl", "daemon-reload").Run()
}

func (t *nativeTarget) self(ctx context.Context, args ...string) error {
	bin, err := os.Executable()
	if err != nil {
		return err
	}
	c := exec.CommandContext(ctx, bin, args...)
	c.Stdin = nil
	c.Stdout, c.Stderr = migrateOutput(t.app), migrateOutput(t.app)
	return c.Run()
}

func migrateOutput(app *App) io.Writer {
	if app.UI.Quiet() {
		return io.Discard
	}
	return app.UI.Stderr()
}

func offerMigrations(ctx context.Context, app *App, list string) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	if list == "" {
		if len(migrate.Done(runtimeOf(inst), migrateValues(inst))) == len(migrate.All) {
			return nil
		}
		app.UI.Info("Fluxer can now run with fewer moving parts. Each change below is optional.")
	}
	return runMigrate(ctx, app, &migrateFlags{Native: true, Migrate: list})
}
