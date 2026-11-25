// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"runtime"
	"slices"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime/upgrade"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	readyTimeout = 10 * time.Minute
	scratchDir   = "render"
)

var fatalChecks = map[string]bool{"services": true, "health": true}

var (
	selfUpdate     = release.SelfUpdate
	reexec         = release.Reexec
	createBackup   = backup.Create
	estimateBackup = backup.Estimate
)

type upgradeFlags struct {
	Check    bool
	To       string
	Prefetch bool
	NoBackup bool
	Backup   string
	DryRun   bool
	Resume   string
	SelfOnly bool
	Settle   bool

	declined bool
}

type upgradeCheckJSON struct {
	Current       string `json:"current"`
	Latest        string `json:"latest"`
	Available     bool   `json:"available"`
	MinFrom       string `json:"min_from"`
	SchemaChanged bool   `json:"schema_changed"`
	Blocked       bool   `json:"blocked"`
	NeedsMigrate  bool   `json:"needs_migrate,omitempty"`
}

func newUpgradeCmd(app *App) *cobra.Command {
	var f upgradeFlags
	c := &cobra.Command{
		Use:   "upgrade",
		Short: "Upgrade rexctl and Fluxer to the latest release",
		Example: "rexctl upgrade\n" +
			"rexctl upgrade --check\n" +
			"rexctl upgrade --prefetch",
		Args: noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			if !c.Flags().Changed("backup") {
				f.Backup = ""
			}
			if f.SelfOnly {
				return updateRexctlOnly(c.Context(), app)
			}
			if f.Settle {
				return settleRelease(c.Context(), app)
			}
			err := runUpgrade(c.Context(), app, &f)
			if !f.Check && !f.DryRun {
				notifyUpgrade(c.Context(), app, err)
			}
			for _, name := range changedFlags(c, "experimental", "experimental-native") {
				app.UI.Info("rexctl upgrade no longer needs --%s. It offers the changes after an upgrade by itself.", name)
			}
			if err != nil || f.declined || app.exit != 0 || f.Check || f.DryRun || f.Prefetch || f.Resume != "" || !app.UI.Interactive() || app.UI.Yes() {
				return err
			}
			return offerMigrations(c.Context(), app)
		},
	}
	fl := c.Flags()
	fl.BoolVar(&f.Check, "check", false, "show whether an upgrade is available without changing anything")
	fl.StringVar(&f.To, "to", "", "upgrade to this version, not the latest")
	fl.BoolVar(&f.Prefetch, "prefetch", false, "download the new version and upgrade later")
	fl.BoolVar(&f.NoBackup, "no-backup", false, "skip the backup before upgrading")
	fl.StringVar(&f.Backup, "backup", backup.ScopeFull, "what the backup before this upgrade holds, full or without-uploads. When the upgrade changes how uploads are stored, the backup includes them anyway.")
	fl.BoolVar(&f.DryRun, "dry-run", false, "show what would happen without changing anything")
	fl.Bool("experimental", false, "")
	fl.Bool("experimental-native", false, "")
	_ = fl.MarkHidden("experimental")
	_ = fl.MarkHidden("experimental-native")
	fl.StringVar(&f.Resume, release.ResumeFlag, "", "")
	_ = fl.MarkHidden(release.ResumeFlag)
	fl.BoolVar(&f.SelfOnly, host.SelfOnlyFlag, false, "")
	_ = fl.MarkHidden(host.SelfOnlyFlag)
	fl.BoolVar(&f.Settle, settleFlag, false, "")
	_ = fl.MarkHidden(settleFlag)
	app.jsonFlag(c)
	return c
}

func runUpgrade(ctx context.Context, app *App, f *upgradeFlags) error {
	if f.Check {
		if f.Prefetch || f.NoBackup || f.DryRun || f.Backup != "" {
			return ui.Usage("Use --check on its own, or with --to and --json.")
		}
		return runUpgradeCheck(ctx, app, f)
	}
	if app.Flags.JSON {
		return ui.Usage("Use --json together with --check.")
	}
	if f.Prefetch && f.NoBackup {
		return ui.Usage("Use --no-backup when you upgrade, not with --prefetch.")
	}
	if f.Prefetch && f.Backup != "" {
		return ui.Usage("Use --backup when you upgrade, not with --prefetch.")
	}
	if f.Backup != "" && f.NoBackup {
		return ui.Usage("Use --backup or --no-backup, not both.")
	}
	if _, err := parseBackupScope(f.Backup); err != nil {
		return err
	}
	if f.Resume != "" && f.Resume != release.ResumeRecovery {
		return resumeUpgrade(ctx, app, f)
	}
	app.keepLayout = true
	inst, tookOver, err := upgradeInstance(ctx, app, f.DryRun)
	if err != nil || inst == nil {
		return err
	}
	if inst.State.IsVoice() {
		if f.Prefetch || f.To != "" {
			return ui.Usage("A voice machine follows the release its instance runs. Run rexctl upgrade without --prefetch or --to.")
		}
		return runVoiceMachineUpgrade(ctx, app, inst, f.DryRun)
	}
	if err := managed(inst); err != nil {
		return err
	}
	if msg, fix, ok := inst.RestoreUnfinished(); ok && !f.DryRun && !f.Prefetch {
		return ui.Refused(msg, fix)
	}
	if legacyCompose(inst) {
		return runLegacyUpgrade(ctx, app, inst, f, false)
	}
	native := rt.KindOf(inst) != rt.Compose
	if rt.KindOf(inst) == rt.Native && !f.DryRun {
		if err := app.nativeRoot(inst.Dir); err != nil {
			return err
		}
		if err := rt.NativeAlone(ctx); err != nil {
			return err
		}
	}
	asked := f.Backup != "" || f.NoBackup
	applyBackupPolicy(inst, f)
	if !native && !f.DryRun && inst.State.Upgrade != nil {
		if handedOver, err := updateBeforeRecovery(ctx, app, f.Resume, f.To, "rexctl upgrade"); err != nil || handedOver {
			return err
		}
	}
	eng, err := upgradeEngine(ctx, app, inst, f)
	if err != nil {
		return err
	}
	cur := inst.State.Release
	if pend := inst.State.Upgrade; pend != nil {
		if f.DryRun {
			cur = recoveryPlan(app, eng, pend)
		} else {
			again := pend.From != pend.To && !upgrade.Rollback(pend)
			res, err := recoverPending(ctx, app, inst, eng)
			if err != nil {
				return err
			}
			if res.Outcome == upgrade.WentBack && !(again && errors.Is(res.Cause, upgrade.ErrInterrupted)) {
				return reportUpgrade(app, inst, res)
			}
			reportRecovery(app, inst, res)
			cur = inst.State.Release
		}
	}
	rel, err := resolveRelease(ctx, f.To)
	if err != nil {
		return err
	}
	if err := release.CheckPath(cur, rel); err != nil {
		return tooOldError(err)
	}
	from, to := cur, rel.Version
	switch cmp := release.Compare(rel.Version, cur); {
	case cmp == 0:
		app.UI.OK("Fluxer %s is up to date.", to)
		return nil
	case cmp < 0:
		return ui.Refused(fmt.Sprintf("Fluxer %s is older than the release you run, %s.", to, from), "Run rexctl rollback to go back to the release you ran before.")
	}
	var e *engine.Engine
	if !native && !f.DryRun {
		if e, err = app.Engine(ctx, inst); err != nil {
			return err
		}
	}
	if err := release.CheckTarget(e, rel, inst.State.Schema); err != nil {
		return floorRefusal(err, to)
	}
	if rt.KindOf(inst) == rt.Native {
		if _, err := rel.DistFor(runtime.GOARCH); err != nil {
			return rt.NativeError(err, "")
		}
	}
	upgradeCard(ctx, app, inst, eng, rel, from, to, enginePolicy(eng, f))
	needSelf := release.Compare(rel.Version, app.Version) != 0
	app.keepLayout = needSelf && rel.StateSchema > 0 && rel.StateSchema < instance.LayoutSchema
	if f.DryRun {
		printUpgradePlan(ctx, app, inst, f, needSelf, rel.Version)
		return nil
	}
	if f.Prefetch {
		app.UI.Info("Fluxer keeps running while the new version downloads.")
	} else {
		switch {
		case inst.State.Stopped:
			app.UI.Info("Fluxer is stopped and stays stopped. It starts on %s when you run rexctl start.", to)
		case native:
			app.UI.Info("Fluxer keeps running while the new version downloads, then restarts.")
		default:
			app.UI.Info("Fluxer keeps running while the new version downloads, then stops for about 2 minutes.")
		}
		ok, err := app.UI.Confirm("Upgrade?", true)
		if err != nil {
			return err
		}
		if !ok {
			f.declined = true
			if tookOver {
				app.UI.Info("rexctl took over your instance. Run rexctl upgrade when you're ready.")
			} else {
				app.UI.Info("Nothing changed.")
			}
			return nil
		}
	}
	if !needSelf {
		return upgradeLocked(ctx, app, inst, native, func() error {
			if err := upgrade.Begin(inst, cur, rel.Version, release.PhaseResolved); err != nil {
				return err
			}
			return runEngine(ctx, app, inst, eng, rel, f)
		})
	}
	asset, err := rel.AssetFor(runtime.GOOS, runtime.GOARCH)
	if err != nil {
		return ui.Failed(fmt.Sprintf("There's no rexctl %s for this machine.", to), "")
	}
	downgrade := release.Valid(app.Version) && release.Compare(rel.Version, app.Version) < 0
	switch {
	case !downgrade:
	case asked && f.Backup == backup.ScopeWithoutUploads:
		app.UI.Info("rexctl %s may not know --backup. This upgrade makes a full backup.", to)
	case !asked && (f.Backup == backup.ScopeWithoutUploads || f.NoBackup):
		app.UI.Info("rexctl %s doesn't read the backup choice you saved. This upgrade makes a full backup.", to)
	}
	err = upgradeLocked(ctx, app, inst, native, func() error {
		if err := upgrade.Begin(inst, cur, rel.Version, release.PhaseResolved); err != nil {
			return err
		}
		err := app.UI.Step(ctx, "Updating rexctl", func(ctx context.Context, s *ui.Step) error {
			if err := selfUpdate(ctx, asset); err != nil {
				return err
			}
			if downgrade {
				s.Done("Switched rexctl back to %s.", to)
			} else {
				s.Done("Updated rexctl to %s.", to)
			}
			return nil
		})
		if err != nil {
			clearUpgrade(inst)
			return selfUpdateError(err, inst)
		}
		return upgrade.Begin(inst, cur, rel.Version, release.PhaseSelfUpdated)
	})
	if err != nil {
		return err
	}
	if err := reexec(resumeArgs(os.Args[1:], release.PhaseSelfUpdated, app.pickedDir(), downgrade)); err != nil {
		return ui.Wrap(err, "Updated rexctl but couldn't start it again.", "Run rexctl upgrade again.")
	}
	return nil
}

func upgradeLocked(ctx context.Context, app *App, inst *instance.Instance, native bool, fn func() error) error {
	if native {
		return app.Locked(ctx, inst, fn)
	}
	return app.Change(ctx, inst, func(*engine.Engine) error { return fn() })
}

func updateBeforeRecovery(ctx context.Context, app *App, resume, to, command string) (bool, error) {
	if resume == release.ResumeRecovery || !release.Valid(app.Version) {
		return false, nil
	}
	rel, err := release.Resolve(ctx, to)
	if err != nil {
		return false, keepRexctl(ctx, app, "Couldn't check for a newer rexctl.", err)
	}
	if release.Compare(rel.Version, app.Version) <= 0 {
		return false, nil
	}
	next := rel.Version
	asset, err := rel.AssetFor(runtime.GOOS, runtime.GOARCH)
	if err != nil {
		return false, keepRexctl(ctx, app, fmt.Sprintf("There's no rexctl %s for this machine.", next), err)
	}
	err = app.UI.Step(ctx, "Updating rexctl", func(ctx context.Context, s *ui.Step) error {
		s.Done("Updated rexctl to %s.", next)
		return selfUpdate(ctx, asset)
	})
	if err != nil {
		return false, keepRexctl(ctx, app, fmt.Sprintf("Couldn't update rexctl to %s.", next), err)
	}
	if err := reexec(resumeArgs(os.Args[1:], release.ResumeRecovery, app.pickedDir(), false)); err != nil {
		return true, ui.Wrap(err, "Updated rexctl but couldn't start it again.", fmt.Sprintf("Run %s again.", command))
	}
	return true, nil
}

func updateRexctlOnly(ctx context.Context, app *App) error {
	if !release.Valid(app.Version) {
		return nil
	}
	rel, err := release.Resolve(ctx, "")
	if err != nil {
		return keepRexctl(ctx, app, "Couldn't check for a newer rexctl.", err)
	}
	if release.Compare(rel.Version, app.Version) <= 0 {
		return nil
	}
	next := rel.Version
	asset, err := rel.AssetFor(runtime.GOOS, runtime.GOARCH)
	if err != nil {
		return keepRexctl(ctx, app, fmt.Sprintf("There's no rexctl %s for this machine.", next), err)
	}
	return app.UI.Step(ctx, "Updating rexctl", func(ctx context.Context, s *ui.Step) error {
		if err := selfUpdate(ctx, asset); err != nil {
			return ui.Wrap(err, fmt.Sprintf("Couldn't update rexctl to %s.", next), "Run rexctl upgrade --verbose to see why.")
		}
		s.Done("Updated rexctl to %s.", next)
		return nil
	})
}

func keepRexctl(ctx context.Context, app *App, msg string, err error) error {
	if ctx.Err() != nil {
		return ctx.Err()
	}
	app.UI.Info("%s Continuing with rexctl %s.", msg, app.Version)
	app.UI.Detail("%v", err)
	return nil
}

func floorRefusal(err error, to string) error {
	var old *release.EngineTooOldError
	if errors.As(err, &old) {
		need, have := strings.TrimSuffix(old.Need, ".0"), old.Have
		msg := fmt.Sprintf("Fluxer keeps running. This release needs %s %s or newer and this machine has %s.", old.Kind.Name(), need, have)
		if old.Kind == engine.Podman {
			return &ui.ExitError{Code: ui.ExitRefused, Msg: msg, Fix: "Upgrade Podman, or move to Docker with rexctl backup and then rexctl restore <file> --engine docker.", Err: err}
		}
		return &ui.ExitError{Code: ui.ExitRefused, Msg: msg, Fix: "Upgrade Docker, then run rexctl upgrade again.", Err: err}
	}
	var schema *release.OlderSchemaError
	if errors.As(err, &schema) {
		return &ui.ExitError{Code: ui.ExitRefused, Msg: fmt.Sprintf("Fluxer %s can't read this instance's settings, which a newer rexctl saved.", to), Fix: "Run rexctl upgrade without --to to stay on a newer release.", Err: err}
	}
	return err
}

func runUpgradeCheck(ctx context.Context, app *App, f *upgradeFlags) error {
	dir, err := app.Dir()
	if err != nil {
		return err
	}
	var cur string
	inst, err := instance.Find(dir)
	switch {
	case errors.Is(err, instance.ErrLegacy):
		if legacy, lerr := instance.DetectLegacy(dir); lerr == nil {
			cur = legacy.ImageTag
		}
	case err != nil:
		if _, ierr := app.Instance(); ierr != nil {
			return ierr
		}
		return err
	case inst.State.IsVoice():
		if f.To != "" {
			return ui.Usage("A voice machine follows the release its instance runs. Run rexctl upgrade --check without --to.")
		}
		return runVoiceMachineCheck(ctx, app, inst)
	default:
		cur = inst.State.Release
	}
	rel, err := resolveRelease(ctx, f.To)
	if err != nil {
		return err
	}
	out := upgradeCheckJSON{
		Current:       cur,
		Latest:        rel.Version,
		Available:     release.Compare(rel.Version, cur) > 0,
		MinFrom:       rel.MinFrom,
		SchemaChanged: rel.SchemaChanged,
	}
	pathErr := release.CheckPath(cur, rel)
	out.Blocked = pathErr != nil
	legacy := inst == nil || legacyCompose(inst)
	if legacy {
		out.NeedsMigrate = out.Available
		out.Blocked = out.Blocked || out.Available
	}
	if app.Flags.JSON {
		return app.UI.JSON(out)
	}
	if legacy && out.Available {
		app.UI.Info("%s", legacyNeedsMoveLine(cur, moveRelease(rel.Version)))
		if inst == nil {
			app.UI.Info("%s", takeoverFix(ctx))
			return nil
		}
		app.UI.Next("rexctl migrate")
		return nil
	}
	from, to := cur, rel.Version
	if !out.Available {
		app.UI.OK("Fluxer %s is up to date.", from)
		return nil
	}
	upgradeCheckCard(app, cur, rel)
	if inst != nil && inst.State.FailedRelease == rel.Version {
		fix := "Run rexctl logs api to see why before you try again."
		if log := inst.Path(instance.StateDirName, upgrade.LogName(rel.Version)); fileExists(log) {
			fix = fmt.Sprintf("Read %s to see why before you try again.", log)
		}
		app.UI.Warn(fmt.Sprintf("The last upgrade to Fluxer %s didn't start.", to), fix)
		return nil
	}
	var tooOld *release.TooOldError
	if errors.As(pathErr, &tooOld) {
		app.UI.Warn(tooOld.Error(), fmt.Sprintf("Run %s first, then run rexctl upgrade again.", tooOld.Command()))
		return nil
	}
	app.UI.Next("rexctl upgrade")
	return nil
}

func upgradeInstance(ctx context.Context, app *App, dryRun bool) (*instance.Instance, bool, error) {
	dir, err := app.Dir()
	if err != nil {
		return nil, false, err
	}
	inst, err := instance.Find(dir)
	if errors.Is(err, instance.ErrLegacy) {
		return nil, false, preRexctlRefusal(ctx, dir)
	}
	if err != nil {
		inst, err := app.Instance()
		return inst, false, err
	}
	return inst, false, nil
}

func resumeUpgrade(ctx context.Context, app *App, f *upgradeFlags) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	if legacyCompose(inst) {
		if err := managed(inst); err != nil {
			return err
		}
		return runLegacyUpgrade(ctx, app, inst, f, f.Resume == release.PhaseSelfUpdated)
	}
	native := rt.KindOf(inst) != rt.Compose
	if rt.KindOf(inst) == rt.Native {
		if err := app.nativeRoot(inst.Dir); err != nil {
			return err
		}
	}
	applyBackupPolicy(inst, f)
	eng, err := upgradeEngine(ctx, app, inst, f)
	if err != nil {
		return err
	}
	return upgradeLocked(ctx, app, inst, native, func() error {
		p := inst.State.Upgrade
		if p == nil || p.Phase != f.Resume {
			return ui.Refused("There's no upgrade to continue.", "Run rexctl upgrade.")
		}
		if release.Valid(app.Version) && release.Compare(app.Version, p.To) != 0 {
			clearUpgrade(inst)
			return ui.Failed(fmt.Sprintf("rexctl didn't update to %s.", p.To), "Run rexctl upgrade again.")
		}
		rel := &release.Release{Version: p.To}
		if native {
			if rel, err = resolveRelease(ctx, p.To); err != nil {
				clearUpgrade(inst)
				return err
			}
		} else if e, err := app.Engine(ctx, inst); err == nil {
			if err := refreshBootUnit(ctx, app, inst, e, nil); err != nil {
				app.UI.Detail("%v", err)
				app.UI.Warn("rexctl couldn't update the service that starts Fluxer with this machine.", "Run rexctl doctor --fix to update it.")
			}
		}
		return runEngine(ctx, app, inst, eng, rel, f)
	})
}

func printUpgradePlan(ctx context.Context, app *App, inst *instance.Instance, f *upgradeFlags, needSelf bool, to string) {
	if needSelf {
		app.UI.Info("rexctl updates itself to %s.", to)
	}
	app.UI.Info("Fluxer %s downloads while your instance keeps running.", to)
	noBackup := f.NoBackup || backupPolicy(inst, f) == settings.BackupOff
	if !f.Prefetch && !noBackup && rt.KindOf(inst) == rt.Native {
		printNativeBackupSpace(ctx, app, inst, to)
	}
	if f.Prefetch {
		app.UI.Info("Dry run. Nothing changed.")
		return
	}
	if noBackup {
		app.UI.Info("Your instance isn't backed up first.")
	} else {
		app.UI.Info("Your instance is backed up first.")
	}
	if inst.State.Stopped {
		app.UI.Info("Fluxer stays stopped. It starts on %s when you run rexctl start.", to)
	} else {
		app.UI.Info("Fluxer stops for about 2 minutes while it moves to %s.", to)
	}
	app.UI.Info("If %s doesn't start, rexctl goes back to the release you have today and keeps your database.", to)
	app.UI.Info("Dry run. Nothing changed.")
}

func printNativeBackupSpace(ctx context.Context, app *App, inst *instance.Instance, to string) {
	r, err := app.Runtime(ctx, inst)
	if err != nil {
		app.UI.Detail("%v", err)
		return
	}
	plan, err := r.Backups().Plan(ctx, rt.BackupOptions{})
	if err != nil {
		app.UI.Detail("%v", err)
		return
	}
	line := fmt.Sprintf("The backup before Fluxer %s needs about %s of free disk space in %s", to, ui.Bytes(plan.NeedBytes), inst.BackupDir())
	switch {
	case plan.FreeBytes < 0:
		app.UI.Info("%s.", line)
	case plan.FreeBytes < plan.NeedBytes:
		app.UI.Warn(fmt.Sprintf("%s, and %s is free.", line, ui.Bytes(plan.FreeBytes)), "Free up space there, then run rexctl upgrade again.")
	default:
		app.UI.Info("%s, and %s is free.", line, ui.Bytes(plan.FreeBytes))
	}
}

func upgradeScope(f *upgradeFlags) string {
	if f.Backup == backup.ScopeWithoutUploads {
		return backup.ScopeWithoutUploads
	}
	return backup.ScopeFull
}

func startRelease(ctx context.Context, app *App, target *instance.Instance, e *engine.Engine, p *engine.Project, s *ui.Step, search, version string) ([]string, error) {
	var waiting []string
	title := "Starting Fluxer " + version
	if err := p.Pull(ctx, app.progress(s, "Downloading")); err != nil {
		return nil, err
	}
	if err := upAll(ctx, p, app.progress(s, "Starting")); err != nil {
		return nil, err
	}
	if err := keepSearchReadable(ctx, app, target, p, search, version); err != nil {
		return nil, err
	}
	show := rt.ShowReadiness(s, title)
	err := p.WaitReady(ctx, readyTimeout, func(r engine.Readiness) {
		waiting = append(append([]string(nil), r.NotReady...), r.Failed...)
		show(r)
	})
	if err != nil {
		return waiting, err
	}
	for _, r := range checks.Run(ctx, checkEnv(ctx, target, e, p), checks.Installed) {
		if r.Status != checks.Fail || r.ID == "upgrade-unfinished" {
			continue
		}
		if fatalChecks[r.ID] {
			return nil, errors.New(r.Message)
		}
		app.UI.Warn(r.Message, r.Fix)
	}
	return nil, nil
}

func postgresRef(s *instance.State) string {
	return settings.ImageRef(s.Settings, s.Images.Refs, "postgres")
}

func composeProblem(err error) string {
	var ee *engine.Error
	if errors.As(err, &ee) && ee.Err != nil {
		err = ee.Err
	}
	return strings.TrimSuffix(strings.TrimSpace(err.Error()), ": invalid compose project")
}

func upAll(ctx context.Context, p *engine.Project, onEvent func(engine.Event)) error {
	err := p.Up(ctx, engine.UpOptions{RemoveOrphans: true, WaitTimeout: readyTimeout, OnEvent: onEvent})
	if engine.ClassOf(err) != engine.ClassDependencyFailed {
		return err
	}
	return startEach(ctx, p, true)
}

func startEach(ctx context.Context, p *engine.Project, stopOnCrash bool) error {
	for _, s := range p.Services() {
		if stopOnCrash {
			if err := p.Crashing(ctx); err != nil {
				return err
			}
		}
		if err := p.Up(ctx, engine.UpOptions{Services: []string{s}, NoDeps: true}); err != nil {
			return err
		}
	}
	return nil
}

func checkEnv(ctx context.Context, inst *instance.Instance, e *engine.Engine, p *engine.Project) *checks.Env {
	h, _ := host.Detect(ctx, inst.Dir)
	mode := inst.State.Settings[settings.EdgeMode]
	return &checks.Env{
		Dir:      inst.Dir,
		Domain:   inst.Domain(),
		Instance: inst,
		Engine:   e,
		Project:  p,
		Host:     h,
		Behind:   mode == settings.EdgeProxy || mode == settings.EdgeTunnel,
	}
}

func resolveRelease(ctx context.Context, to string) (*release.Release, error) {
	rel, err := release.Resolve(ctx, to)
	if errors.Is(err, release.ErrUnknownVersion) {
		return nil, ui.Refused(fmt.Sprintf("There's no Fluxer release called %s.", to), "Run rexctl upgrade --check to see the latest release.")
	}
	if err != nil {
		if ctx.Err() != nil {
			return nil, ctx.Err()
		}
		if errors.Is(err, release.ErrBadResponse) {
			return nil, ui.Wrap(err, "Couldn't check for a new release. Fluxer's release site sent an error.", "Try again later.")
		}
		return nil, ui.Wrap(err, "Couldn't check for a new release.", "Check this machine's internet connection, then try again.")
	}
	return rel, nil
}

func tooOldError(err error) error {
	var e *release.TooOldError
	if errors.As(err, &e) {
		return ui.Refused(e.Error(), fmt.Sprintf("Run %s first, then run rexctl upgrade again.", e.Command()))
	}
	return err
}

func selfUpdateError(err error, inst *instance.Instance) error {
	switch {
	case errors.Is(err, context.Canceled):
		return err
	case errors.Is(err, fs.ErrPermission):
		fix := "Run " + sudoAgain(inst.Dir) + "."
		switch {
		case runtime.GOOS == "windows":
			fix = "Run rexctl upgrade from a terminal opened as administrator."
		case inst.State != nil && inst.State.Engine.Kind == string(engine.Podman) && os.Geteuid() != 0:
			fix = "Move rexctl to a folder your account can write to, like ~/.local/bin, then run rexctl upgrade."
		}
		path, _ := release.Executable()
		return ui.Refused(fmt.Sprintf("rexctl can't replace itself at %s.", path), fix)
	case errors.Is(err, release.ErrChecksum):
		return ui.Wrap(err, "The rexctl download didn't match its checksum.", "Run rexctl upgrade again.")
	case errors.Is(err, release.ErrBadResponse):
		return ui.Wrap(err, "Couldn't update rexctl. Fluxer's release site sent an error.", "Try rexctl upgrade again later.")
	}
	return ui.Wrap(err, "Couldn't update rexctl.", "Check this machine's internet connection, then run rexctl upgrade again.")
}

func applyBackupPolicy(inst *instance.Instance, f *upgradeFlags) {
	if f.Prefetch || f.NoBackup || f.Backup != "" {
		return
	}
	switch backupPolicy(inst, f) {
	case settings.BackupOff:
		f.NoBackup = true
	case settings.BackupWithoutUploads:
		f.Backup = backup.ScopeWithoutUploads
	}
}

func clearUpgrade(inst *instance.Instance) {
	if pend := inst.State.Upgrade; pend != nil {
		_ = inst.PutBackSites(pend.Sites)
	}
	inst.State.Upgrade = nil
	_ = inst.Save()
}

func phaseIndex(phase string) int {
	for i, p := range release.Phases {
		if p == phase {
			return i
		}
	}
	return -1
}

var downgradeArgs = []string{"--yes", "-y", "--no-backup", "--prefetch", "--verbose", "--quiet", "-q"}

func resumeArgs(args []string, phase, dir string, downgrade bool) []string {
	out := make([]string, 0, len(args)+4)
	flag := "--" + release.ResumeFlag
	if downgrade {
		out = append(out, "upgrade")
	}
	for i := 0; i < len(args); i++ {
		a := args[i]
		if a == flag {
			i++
			continue
		}
		if strings.HasPrefix(a, flag+"=") {
			continue
		}
		if downgrade && !slices.Contains(downgradeArgs, a) {
			continue
		}
		out = append(out, a)
	}
	if dir != "" {
		out = append(out, "--dir", dir)
	}
	return append(out, flag, phase)
}

func fileExists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

func fluxerName(short string) string {
	if short == "" {
		return "the Fluxer you ran before rexctl"
	}
	return "Fluxer " + short
}

func fixLine(app *App, fix string) {
	if app.UI.Mode() == ui.ModeJSON {
		return
	}
	fmt.Fprintf(app.UI.Stderr(), "  %s\n", fix)
}
