// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"maps"
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/setup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	readyTimeout    = 10 * time.Minute
	recoveryTimeout = 20 * time.Minute
	scratchDir      = "render"
)

var fatalChecks = map[string]bool{"services": true, "health": true}

var (
	selfUpdate = release.SelfUpdate
	reexec     = release.Reexec
)

type upgradeFlags struct {
	Check    bool
	To       string
	Prefetch bool
	NoBackup bool
	DryRun   bool
	Resume   string
}

type upgradeCheckJSON struct {
	Current       string `json:"current"`
	Latest        string `json:"latest"`
	Available     bool   `json:"available"`
	MinFrom       string `json:"min_from"`
	SchemaChanged bool   `json:"schema_changed"`
	Blocked       bool   `json:"blocked"`
}

func newUpgradeCmd(app *App) *cobra.Command {
	var f upgradeFlags
	c := &cobra.Command{
		Use:   "upgrade",
		Short: "Upgrade rexctl and Fluxer to the latest release",
		Args:  noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			err := runUpgrade(c.Context(), app, &f)
			if !f.Check && !f.DryRun {
				notifyUpgrade(c.Context(), app, err)
			}
			return err
		},
	}
	fl := c.Flags()
	fl.BoolVar(&f.Check, "check", false, "show whether an upgrade is available without changing anything")
	fl.StringVar(&f.To, "to", "", "upgrade to this version, not the latest")
	fl.BoolVar(&f.Prefetch, "prefetch", false, "download the new version and upgrade later")
	fl.BoolVar(&f.NoBackup, "no-backup", false, "skip the backup before upgrading")
	fl.BoolVar(&f.DryRun, "dry-run", false, "show what would happen without changing anything")
	fl.StringVar(&f.Resume, release.ResumeFlag, "", "")
	_ = fl.MarkHidden(release.ResumeFlag)
	app.jsonFlag(c)
	return c
}

func runUpgrade(ctx context.Context, app *App, f *upgradeFlags) error {
	if f.Check {
		if f.Prefetch || f.NoBackup || f.DryRun {
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
	if f.Resume != "" && f.Resume != release.ResumeRecovery {
		return resumeUpgrade(ctx, app, f)
	}
	app.keepLayout = true
	inst, tookOver, err := upgradeInstance(ctx, app, f.DryRun)
	if err != nil || inst == nil {
		return err
	}
	if err := managed(inst); err != nil {
		return err
	}
	if !f.DryRun && inst.State.Upgrade != nil {
		if handedOver, err := updateBeforeRecovery(ctx, app, f.Resume, f.To, "rexctl upgrade"); err != nil || handedOver {
			return err
		}
	}
	var e *engine.Engine
	if !f.DryRun {
		if e, err = app.Engine(ctx, inst); err != nil {
			return err
		}
		if inst.State.Upgrade != nil {
			if err := app.Locked(ctx, inst, func() error { return recoverUpgrade(ctx, app, inst, true) }); err != nil {
				return err
			}
		}
	}
	cur := inst.State.Release
	if f.DryRun && inst.State.Upgrade != nil {
		cur = printRecoveryPlan(app, inst)
	}
	rel, err := resolveRelease(ctx, f.To)
	if err != nil {
		return err
	}
	if err := release.CheckPath(cur, rel); err != nil {
		return tooOldError(err)
	}
	from, to := versionPair(cur, rel.Version)
	switch cmp := release.Compare(rel.Version, cur); {
	case cmp == 0:
		app.UI.OK("Fluxer %s is up to date.", to)
		return nil
	case cmp < 0:
		return ui.Refused(fmt.Sprintf("Fluxer %s is older than the release you run, %s.", to, from), "Run rexctl rollback to go back to the release you ran before.")
	}
	if err := release.CheckTarget(e, rel, inst.State.Schema); err != nil {
		return floorRefusal(err, to)
	}
	if release.Valid(cur) {
		app.UI.Info("Fluxer %s is available. You're on %s.", to, from)
	} else {
		app.UI.Info("Fluxer %s is available.", to)
	}
	needSelf := release.Compare(rel.Version, app.Version) != 0
	app.keepLayout = needSelf && rel.StateSchema > 0 && rel.StateSchema < instance.StateSchema
	if f.DryRun {
		for _, k := range keptCopies(inst) {
			warnKeptCopy(app.UI, k.Name, k.Copy, true)
		}
		printUpgradePlan(ctx, app, inst, f, needSelf, to)
		return nil
	}
	if f.Prefetch {
		app.UI.Info("Fluxer keeps running while the new version downloads.")
	} else {
		if ok, err := confirmKeptCopies(app, inst); err != nil || !ok {
			if err == nil {
				app.UI.Info("Nothing changed.")
			}
			return err
		}
		app.UI.Info("Fluxer keeps running while the new version downloads, then stops for about 2 minutes.")
		ok, err := app.UI.Confirm("Upgrade?", true)
		if err != nil {
			return err
		}
		if !ok {
			if tookOver {
				app.UI.Info("rexctl took over your instance. Run rexctl upgrade when you're ready.")
			} else {
				app.UI.Info("Nothing changed.")
			}
			return nil
		}
	}
	if !needSelf {
		return app.Change(ctx, inst, func(e *engine.Engine) error {
			if err := setPhase(inst, cur, rel.Version, release.PhaseResolved); err != nil {
				return err
			}
			return upgradeStack(ctx, app, inst, e, f)
		})
	}
	asset, err := rel.AssetFor(runtime.GOOS, runtime.GOARCH)
	if err != nil {
		return ui.Failed(fmt.Sprintf("There's no rexctl %s for this machine.", to), "")
	}
	err = app.Change(ctx, inst, func(*engine.Engine) error {
		if err := setPhase(inst, cur, rel.Version, release.PhaseResolved); err != nil {
			return err
		}
		err := app.UI.Step(ctx, "Updating rexctl", func(ctx context.Context, s *ui.Step) error {
			if release.Valid(app.Version) && release.Compare(rel.Version, app.Version) < 0 {
				s.Done("Switched rexctl back to %s.", to)
			} else {
				s.Done("Updated rexctl to %s.", to)
			}
			return selfUpdate(ctx, asset)
		})
		if err != nil {
			clearUpgrade(inst)
			return selfUpdateError(err, inst)
		}
		return setPhase(inst, cur, rel.Version, release.PhaseSelfUpdated)
	})
	if err != nil {
		return err
	}
	if err := reexec(resumeArgs(os.Args[1:], release.PhaseSelfUpdated, app.pickedDir())); err != nil {
		return ui.Wrap(err, "Updated rexctl but couldn't start it again.", "Run rexctl upgrade again.")
	}
	return nil
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
	_, next := versionPair(app.Version, rel.Version)
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
	if err := reexec(resumeArgs(os.Args[1:], release.ResumeRecovery, app.pickedDir())); err != nil {
		return true, ui.Wrap(err, "Updated rexctl but couldn't start it again.", fmt.Sprintf("Run %s again.", command))
	}
	return true, nil
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
	if app.Flags.JSON {
		return app.UI.JSON(out)
	}
	from, to := versionPair(cur, rel.Version)
	if !out.Available {
		app.UI.OK("Fluxer %s is up to date.", from)
		return nil
	}
	if release.Valid(cur) {
		app.UI.Info("Fluxer %s is available. You're on %s.", to, from)
	} else {
		app.UI.Info("Fluxer %s is available.", to)
	}
	if inst != nil && inst.State.FailedRelease == rel.Version {
		fix := "Run rexctl logs api to see why before you try again."
		if log := inst.Path(instance.StateDirName, "upgrade-"+release.DirName(rel.Version)+".log"); fileExists(log) {
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
	for _, k := range keptCopies(inst) {
		warnKeptCopy(app.UI, k.Name, k.Copy, true)
	}
	app.UI.Info("Run rexctl upgrade when you're ready.")
	return nil
}

func upgradeInstance(ctx context.Context, app *App, dryRun bool) (*instance.Instance, bool, error) {
	dir, err := app.Dir()
	if err != nil {
		return nil, false, err
	}
	inst, err := instance.Find(dir)
	if errors.Is(err, instance.ErrLegacy) {
		legacy, err := instance.DetectLegacy(dir)
		if err != nil {
			return nil, false, err
		}
		inst, kept, err := takeoverDir(ctx, app, legacy.Dir, takeoverOptions{DryRun: dryRun, Then: "Then rexctl upgrades it to the latest Fluxer release."})
		if err != nil || inst == nil {
			return nil, false, err
		}
		if len(kept) > 0 {
			app.UI.Info("rexctl took over your instance and didn't upgrade it. Your changes keep working until you run rexctl upgrade.")
			return nil, false, nil
		}
		return inst, true, nil
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
	return app.Change(ctx, inst, func(e *engine.Engine) error {
		p := inst.State.Upgrade
		if p == nil || p.Phase != f.Resume {
			return ui.Refused("There's no upgrade to continue.", "Run rexctl upgrade.")
		}
		if release.Valid(app.Version) && release.Compare(app.Version, p.To) != 0 {
			clearUpgrade(inst)
			return ui.Failed(fmt.Sprintf("rexctl didn't update to %s.", p.To), "Run rexctl upgrade again.")
		}
		if err := refreshBootUnit(ctx, inst, e, nil); err != nil {
			app.UI.Detail("%v", err)
		}
		return upgradeStack(ctx, app, inst, e, f)
	})
}

func printUpgradePlan(ctx context.Context, app *App, inst *instance.Instance, f *upgradeFlags, needSelf bool, to string) {
	if needSelf {
		app.UI.Info("rexctl updates itself to %s.", to)
	}
	app.UI.Info("Fluxer %s downloads while your instance keeps running.", to)
	if e, err := app.Engine(ctx, inst); err != nil {
		app.UI.Detail("%v", err)
	} else if needSelf {
		printSpace(ctx, app, inst, e, nil, f, to)
	} else {
		printSpace(ctx, app, inst, e, embeddedRefs(), f, to)
	}
	if f.Prefetch {
		app.UI.Info("Dry run. Nothing changed.")
		return
	}
	if f.NoBackup {
		app.UI.Info("Your instance isn't backed up first.")
	} else {
		app.UI.Info("Your instance is backed up first.")
	}
	app.UI.Info("Fluxer stops for about 2 minutes while it moves to %s.", to)
	app.UI.Info("If %s doesn't start, rexctl goes back to the release you have today.", to)
	app.UI.Info("Dry run. Nothing changed.")
}

func printSpace(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine, next map[string]string, f *upgradeFlags, to string) {
	for _, sp := range upgradeSpace(ctx, app, inst, e, next, f) {
		if sp.Short() {
			app.UI.Warn(spaceLine(sp, to), spaceFix(e, sp))
		} else {
			app.UI.Info("%s", spaceLine(sp, to))
		}
	}
}

func embeddedRefs() map[string]string {
	images, err := stack.EmbeddedImages()
	if err != nil {
		return nil
	}
	return images.Images
}

func upgradeSpace(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine, next map[string]string, f *upgradeFlags) []checks.Space {
	old := e.Named(inst.Project(), inst.Dir)
	pull, err := checks.PullNeed(ctx, e, inst.State.Images.Refs, next)
	if err != nil {
		app.UI.Detail("%v", err)
	}
	var saved uint64
	if !f.Prefetch && !f.NoBackup {
		if saved, err = backup.Estimate(ctx, inst, old); err != nil {
			app.UI.Detail("%v", err)
		}
	}
	return checkEnv(ctx, inst, e, old).Space(pull, saved)
}

func spaceLine(sp checks.Space, to string) string {
	return fmt.Sprintf("Fluxer %s needs about %s of free disk space in %s, and %s is free.", to, ui.Bytes(int64(sp.Need)), sp.Path, ui.Bytes(int64(sp.Free)))
}

func spaceFix(e *engine.Engine, sp checks.Space) string {
	if sp.Images {
		return fmt.Sprintf("Free up space there, for example with %s image prune -a, which removes the images no container uses. Then run rexctl upgrade again.", strings.ToLower(e.Kind.Name()))
	}
	return "Free up space there, then run rexctl upgrade again."
}

func upgradeStack(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine, f *upgradeFlags) error {
	pend := inst.State.Upgrade
	from, to := pend.From, pend.To
	fromShort, toShort := versionPair(from, to)
	set := stack.Embedded()
	images, err := stack.EmbeddedImages()
	if err != nil {
		clearUpgrade(inst)
		return ui.Wrap(err, "This rexctl has no Fluxer release built in.", "Download rexctl again, then run rexctl upgrade.")
	}
	merged, err := instance.DropMergedShards(inst.Dir)
	if err != nil {
		clearUpgrade(inst)
		return ui.Wrap(err, fmt.Sprintf("Couldn't update %s.", instance.CustomCompose), "Check that you can write to it, then run rexctl upgrade again.")
	}
	if len(merged) > 0 {
		bases := make([]string, len(merged))
		for i, name := range merged {
			bases[i] = strings.TrimSuffix(name, "-shard")
		}
		app.UI.Info("Removed %s from %s. Fluxer %s runs them inside %s, which already have the same changes.", ui.JoinAnd(merged), instance.CustomCompose, toShort, ui.JoinAnd(bases))
	}
	if retired := instance.RetiredService(inst.Dir); retired != "" {
		clearUpgrade(inst)
		return ui.Refused(fmt.Sprintf("%s changes %s, which Fluxer %s no longer runs.", instance.CustomCompose, retired, toShort), retiredFix(retired, fromShort))
	}
	if err := release.Snapshot(inst, from, refList(inst.State.Images.Refs)); err != nil {
		clearUpgrade(inst)
		return ui.Wrap(err, fmt.Sprintf("Couldn't save the files of %s.", fluxerName(fromShort)), "Check that the disk isn't full, then run rexctl upgrade again.")
	}
	if err := release.WriteRelease(inst, set); err != nil {
		clearUpgrade(inst)
		return ui.Wrap(err, fmt.Sprintf("Couldn't save the files of Fluxer %s.", toShort), "Check that the disk isn't full, then run rexctl upgrade again.")
	}
	next, pins, err := nextState(inst, set, images, to)
	if err != nil {
		clearUpgrade(inst)
		return ui.Wrap(err, fmt.Sprintf("Couldn't update your settings for Fluxer %s.", toShort), "Run rexctl upgrade --verbose to see why.")
	}
	if err := release.CheckPostgres(postgresRef(inst.State), postgresRef(next)); err != nil {
		clearUpgrade(inst)
		var pg *release.PostgresMajorError
		if errors.As(err, &pg) {
			return ui.Refused(fmt.Sprintf("Fluxer %s moves your database to a new major version, and rexctl can't do that yet.", toShort), fmt.Sprintf("Nothing changed. You're still on %s.", fluxerName(fromShort)))
		}
		return ui.Wrap(err, fmt.Sprintf("Couldn't read the database version for Fluxer %s.", toShort), "Run rexctl upgrade --verbose to see why.")
	}
	if err := release.Snapshot(&instance.Instance{Dir: inst.Dir, State: next}, to, refList(next.Images.Refs)); err != nil {
		clearUpgrade(inst)
		return ui.Wrap(err, fmt.Sprintf("Couldn't save the settings for Fluxer %s.", toShort), "Check that the disk isn't full, then run rexctl upgrade again.")
	}
	old := e.Named(inst.Project(), inst.Dir)
	for _, sp := range upgradeSpace(ctx, app, inst, e, next.Images.Refs, f) {
		if sp.Short() {
			clearUpgrade(inst)
			return ui.Refused(spaceLine(sp, toShort)+" "+capitalise(fluxerName(fromShort))+" is still running.", spaceFix(e, sp))
		}
	}
	if err := pullRelease(ctx, app, inst, e, set, next, toShort); err != nil {
		clearUpgrade(inst)
		if ctx.Err() != nil {
			app.UI.Info("Stopped the upgrade. %s is still running.", capitalise(fluxerName(fromShort)))
			return ctx.Err()
		}
		return err
	}
	if err := setPhase(inst, from, to, release.PhasePulled); err != nil {
		return err
	}
	if f.Prefetch {
		clearUpgrade(inst)
		app.UI.Info("Run rexctl upgrade when you're ready. It takes about 2 minutes.")
		return nil
	}
	if f.NoBackup {
		app.UI.Warn("Skipped the backup.", "")
	} else {
		err := app.UI.Step(ctx, "Backing up your instance", func(ctx context.Context, s *ui.Step) error {
			info, err := backup.Create(ctx, inst, old, backup.CreateOptions{Reason: "upgrade", OnEvent: app.progress(s, "Starting"), Progress: func(stage string) {
				if t := backupStageTitle(stage); t != "" {
					s.Title(t)
				}
			}})
			if errors.Is(err, backup.ErrNeverStarted) {
				s.Done("There was nothing to back up yet.")
				return nil
			}
			if err != nil {
				return err
			}
			inst.State.Upgrade.Backup = info.Path
			s.Done("Backed up your instance.")
			return nil
		})
		if err != nil {
			fix := "Run rexctl backup to see why, then run rexctl upgrade again."
			if logPath := writeBackupLog(inst, "upgrade-"+release.DirName(to)+".log", err); logPath != "" {
				fix = fmt.Sprintf("Read %s to see why, then run rexctl upgrade again.", logPath)
			}
			report := ui.Wrap(err, fmt.Sprintf("Couldn't back up your instance. %s is still running.", capitalise(fluxerName(fromShort))), fix)
			var be *ui.ExitError
			if errors.As(backupCreateErr(err), &be) && be.Err == nil {
				report.Msg = be.Msg + " " + capitalise(fluxerName(fromShort)) + " is still running."
				report.Fix = be.Fix
			}
			return abortUpgrade(ctx, app, inst, e, err, report)
		}
		if err := setPhase(inst, from, to, release.PhaseBackedUp); err != nil {
			return err
		}
		pruneBackups(app, inst)
	}
	if err := setPhase(inst, from, to, release.PhaseMigrated); err != nil {
		return abortUpgrade(ctx, app, inst, e, err, ui.Wrap(err, "Couldn't save your settings.", "Run rexctl upgrade --verbose to see why."))
	}
	r, err := inst.Render(set, stack.OverlaysFor(next.Settings), next.Images.Refs)
	if err != nil {
		return abortUpgrade(ctx, app, inst, e, err, ui.Wrap(err, fmt.Sprintf("Couldn't write the files for Fluxer %s.", toShort), "Run rexctl upgrade --verbose to see why."))
	}
	inst.State.Rendered = r.Hashes
	next.Rendered = r.Hashes
	if err := setPhase(inst, from, to, release.PhaseRecreated); err != nil {
		return abortUpgrade(ctx, app, inst, e, err, ui.Wrap(err, "Couldn't save your settings.", "Run rexctl upgrade --verbose to see why."))
	}
	target := &instance.Instance{Dir: inst.Dir, State: next}
	p, err := deploy.Load(ctx, target, e)
	if err != nil {
		return abortUpgrade(ctx, app, inst, e, err, engineFailure(err))
	}
	waiting, err := startRelease(ctx, app, target, e, p, toShort)
	if err != nil {
		if ctx.Err() == nil {
			return failedStart(ctx, app, inst, e, p, waiting, err)
		}
		return abortUpgrade(ctx, app, inst, e, err, ctx.Err())
	}
	if err := inst.DropRootCopies(ctx, true); err != nil {
		app.UI.Detail("%v", err)
	}
	if err := setPhase(inst, from, to, release.PhaseVerified); err != nil {
		return err
	}
	for _, pin := range pins {
		app.UI.Info("rexctl kept your own %s image.", pin)
	}
	return finishUpgrade(ctx, app, inst, e)
}

func startRelease(ctx context.Context, app *App, target *instance.Instance, e *engine.Engine, p *engine.Project, toShort string) ([]string, error) {
	var waiting []string
	err := app.UI.Step(ctx, "Starting Fluxer "+toShort, func(ctx context.Context, s *ui.Step) error {
		if err := p.Pull(ctx, app.progress(s, "Downloading")); err != nil {
			return err
		}
		if err := upAll(ctx, p, app.progress(s, "Starting")); err != nil {
			return err
		}
		show := showReadiness(s, "Starting Fluxer "+toShort)
		err := p.WaitReady(ctx, readyTimeout, func(r engine.Readiness) {
			waiting = append(append([]string(nil), r.NotReady...), r.Failed...)
			show(r)
		})
		if err != nil {
			return err
		}
		waiting = nil
		for _, r := range checks.Run(ctx, checkEnv(ctx, target, e, p), checks.Installed) {
			if r.Status != checks.Fail {
				continue
			}
			if fatalChecks[r.ID] {
				return errors.New(r.Message)
			}
			app.UI.Warn(r.Message, r.Fix)
		}
		s.Done("Started Fluxer %s.", toShort)
		return nil
	})
	return waiting, err
}

func retryUpgrade(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine) error {
	pend := inst.State.Upgrade
	_, toShort := versionPair(pend.From, pend.To)
	next, err := release.SnapshotState(inst, pend.To)
	if err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't read the settings for Fluxer %s.", toShort), "Run rexctl doctor to see what's wrong.")
	}
	set, err := release.LoadSet(inst, pend.To)
	if err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't read the files for Fluxer %s.", toShort), "Run rexctl doctor to see what's wrong.")
	}
	r, err := inst.Render(set, stack.OverlaysFor(next.Settings), next.Images.Refs)
	if err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't write the files for Fluxer %s.", toShort), "Run rexctl upgrade --verbose to see why.")
	}
	inst.State.Rendered = r.Hashes
	next.Rendered = r.Hashes
	if err := inst.Save(); err != nil {
		return ui.Wrap(err, "Couldn't save your instance's state.", "Run rexctl doctor to see what's wrong.")
	}
	if err := e.Named(inst.Project(), inst.Dir).RemoveReplacements(ctx); err != nil {
		return engineFailure(err)
	}
	target := &instance.Instance{Dir: inst.Dir, State: next}
	p, err := deploy.Load(ctx, target, e)
	if err != nil {
		return engineFailure(err)
	}
	waiting, err := startRelease(ctx, app, target, e, p, toShort)
	if err != nil {
		if ctx.Err() != nil {
			return ctx.Err()
		}
		fix := release.UnfinishedFix(pend)
		if logPath := writeFailureLog(ctx, inst, p, "upgrade-"+release.DirName(pend.To)+".log", err, waiting); logPath != "" {
			fix = fmt.Sprintf("Read %s to see why. %s", logPath, fix)
		}
		return ui.Wrap(err, fmt.Sprintf("Fluxer %s didn't start either.", toShort), fix)
	}
	if err := inst.DropRootCopies(ctx, true); err != nil {
		app.UI.Detail("%v", err)
	}
	if err := setPhase(inst, pend.From, pend.To, release.PhaseVerified); err != nil {
		return err
	}
	return finishUpgrade(ctx, app, inst, e)
}

func forwardable(inst *instance.Instance) bool {
	pend := inst.State.Upgrade
	return pend != nil && pend.BackFailed && phaseIndex(pend.Phase) >= phaseIndex(release.PhaseRecreated) && release.HasSnapshot(inst, pend.To)
}

func printRecoveryPlan(app *App, inst *instance.Instance) string {
	pend := inst.State.Upgrade
	fromShort, toShort := versionPair(pend.From, pend.To)
	phase := phaseIndex(pend.Phase)
	switch {
	case phase >= phaseIndex(release.PhaseVerified):
		app.UI.Info("The upgrade to Fluxer %s didn't finish. rexctl finishes it first.", toShort)
		return pend.To
	case forwardable(inst):
		app.UI.Info("The upgrade to Fluxer %s didn't finish. rexctl starts Fluxer %s again.", toShort, toShort)
		return pend.To
	case phase >= phaseIndex(release.PhasePulled) && release.HasSnapshot(inst, pend.From):
		app.UI.Info("The upgrade to Fluxer %s didn't finish. rexctl goes back to %s first.", toShort, fluxerName(fromShort))
		if phase >= phaseIndex(release.PhaseRecreated) && release.HasSnapshot(inst, pend.To) {
			app.UI.Info("If %s doesn't start, rexctl starts Fluxer %s again.", fluxerName(fromShort), toShort)
		}
	}
	return pend.From
}

func retiredFix(name, fromShort string) string {
	still := fmt.Sprintf("You're still on %s.", fluxerName(fromShort))
	if base, ok := strings.CutSuffix(name, "-shard"); ok && slices.Contains(stack.Services, base) {
		return fmt.Sprintf("%s Move those changes to %s, then run rexctl upgrade again.", still, base)
	}
	return fmt.Sprintf("%s Remove %s from %s, then run rexctl upgrade again.", still, name, instance.CustomCompose)
}

func volumeMismatch(inst *instance.Instance, keys []string, toShort string) error {
	names := make([]string, len(keys))
	for i, k := range keys {
		names[i] = inst.Project() + "_" + k
	}
	still := "Your instance hasn't changed."
	if pend := inst.State.Upgrade; pend != nil {
		fromShort, _ := versionPair(pend.From, pend.To)
		still = fmt.Sprintf("You're still on %s.", fluxerName(fromShort))
	}
	fix := "Run rexctl doctor to see what's wrong, then run rexctl upgrade again."
	if fileExists(inst.Path(instance.CustomCompose)) {
		fix = fmt.Sprintf("Compare them with docker volume inspect, change %s to match how they were made, then run rexctl upgrade again.", instance.CustomCompose)
	}
	return ui.Refused(fmt.Sprintf("Fluxer %s can't use %s, because they were made with other options than the compose files ask for. %s", toShort, strings.Join(names, ", "), still), fix)
}

func nextState(inst *instance.Instance, set *stack.Set, images stack.ImageSet, to string) (*instance.State, []string, error) {
	cur := inst.State
	next := *cur
	next.Release = to
	next.Upgrade = nil
	next.Pending = nil
	next.Settings = maps.Clone(cur.Settings)
	next.Secrets = maps.Clone(cur.Secrets)
	next.Rendered = maps.Clone(cur.Rendered)
	next.Images = instance.Images{Release: to, Refs: maps.Clone(images.Images)}
	if next.Images.Refs == nil {
		next.Images.Refs = map[string]string{}
	}
	var pins []string
	if cur.Release == stack.LegacyRelease {
		pins = slices.Sorted(maps.Keys(instance.ConvertLegacy(&next, set, next.Images.Refs)))
	}
	if _, err := secrets.Mint(next.Secrets, true); err != nil {
		return nil, nil, err
	}
	return &next, pins, nil
}

func postgresRef(s *instance.State) string {
	for _, v := range []string{s.Settings[settings.ImagesPrefix+"postgres"], s.Settings[settings.ImageVars["postgres"]], s.Images.Refs["postgres"]} {
		if v != "" {
			return v
		}
	}
	return ""
}

func refList(refs map[string]string) []string {
	return slices.Sorted(maps.Values(refs))
}

func pullRelease(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine, set *stack.Set, next *instance.State, toShort string) error {
	scratch := filepath.Join(release.Dir(inst, next.Release), scratchDir)
	defer os.RemoveAll(scratch)
	r, err := stack.RenderAt(set, stack.OverlaysFor(next.Settings), next.Images.Refs, inst.Project(), scratch, inst.OldLayout())
	if err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't prepare the files for Fluxer %s.", toShort), "Run rexctl upgrade --verbose to see why.")
	}
	files := slices.Clone(r.ComposeFiles)
	for i, f := range files {
		if !filepath.IsAbs(f) {
			files[i] = filepath.Join(scratch, f)
		}
	}
	base := slices.Clone(files)
	custom := fileExists(inst.Path(instance.CustomCompose))
	if custom {
		files = append(files, inst.Path(instance.CustomCompose))
	}
	var admin map[string]string
	if next.AdminImport != nil {
		admin = next.AdminImport.Values
	}
	env, err := settings.Effective(set, next.Settings, next.Secrets, admin)
	if err != nil {
		return ui.Wrap(err, fmt.Sprintf("Fluxer %s doesn't accept the settings on this machine.", toShort), "Run rexctl upgrade --verbose to see why.")
	}
	still := "Your instance hasn't changed."
	if pend := inst.State.Upgrade; pend != nil {
		fromShort, _ := versionPair(pend.From, pend.To)
		still = fmt.Sprintf("Your instance is still on %s.", fluxerName(fromShort))
	}
	p, err := e.Load(ctx, engine.Spec{Name: inst.Project(), Dir: inst.Dir, Files: files, Env: env})
	if err != nil {
		if ctx.Err() != nil {
			return ctx.Err()
		}
		if custom && engine.ClassOf(err) == engine.ClassInvalidConfig {
			return ui.Refused(fmt.Sprintf("%s doesn't work with Fluxer %s: %s.", instance.CustomCompose, toShort, composeProblem(err)),
				fmt.Sprintf("Change %s, then run rexctl upgrade again. %s", instance.CustomCompose, still))
		}
		return ui.Wrap(engineFailure(err), fmt.Sprintf("Fluxer %s doesn't accept the settings on this machine.", toShort), "Run rexctl upgrade --verbose to see why.")
	}
	if plan, err := p.Plan(ctx); engine.ClassOf(err) == engine.ClassVolumeChanged {
		return volumeMismatch(inst, plan.Volumes, toShort)
	}
	if custom {
		shipped, err := e.Load(ctx, engine.Spec{Name: inst.Project(), Dir: inst.Dir, Files: base, Env: env})
		if err != nil {
			return ui.Wrap(engineFailure(err), fmt.Sprintf("Fluxer %s doesn't accept the settings on this machine.", toShort), "Run rexctl upgrade --verbose to see why.")
		}
		if dropped := p.DroppedMounts(shipped); len(dropped) > 0 {
			return ui.Refused(fmt.Sprintf("%s leaves out what Fluxer %s mounts at %s.", instance.CustomCompose, toShort, strings.Join(dropped, ", ")),
				fmt.Sprintf("Add those mounts back to %s, then run rexctl upgrade again. %s", instance.CustomCompose, still))
		}
	}
	err = app.UI.Step(ctx, "Downloading Fluxer "+toShort, func(ctx context.Context, s *ui.Step) error {
		if err := p.Pull(ctx, app.progress(s, "Downloading")); err != nil {
			return err
		}
		s.Done("Downloaded Fluxer %s.", toShort)
		return nil
	})
	if err == nil {
		return nil
	}
	if ctx.Err() != nil {
		return ctx.Err()
	}
	report := engineFailure(err)
	var ee *ui.ExitError
	if errors.As(report, &ee) && ee.Err != nil {
		ee.Msg = fmt.Sprintf("Couldn't download Fluxer %s. %s %s", toShort, ee.Msg, still)
		return ee
	}
	return ui.Wrap(err, fmt.Sprintf("Couldn't download Fluxer %s. %s", toShort, still), "Check this machine's internet connection, then run rexctl upgrade again.")
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

func failedStart(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine, p *engine.Project, waiting []string, cause error) error {
	pend := inst.State.Upgrade
	fromShort, toShort := versionPair(pend.From, pend.To)
	if len(waiting) == 0 {
		waiting = notRunning(context.WithoutCancel(ctx), p)
	}
	msg := fmt.Sprintf("Fluxer %s didn't start.", toShort)
	if len(waiting) > 0 {
		msg = fmt.Sprintf("Fluxer %s didn't start because %s", toShort, checks.Services(waiting, "isn't ready.", "aren't ready."))
	}
	app.UI.Fail(msg, "")
	app.UI.Detail("%v", cause)
	logPath := writeFailureLog(ctx, inst, p, "upgrade-"+release.DirName(pend.To)+".log", cause, waiting)
	if err := goBack(ctx, app, inst, e, pend.From); err != nil {
		return err
	}
	inst.State.FailedRelease = pend.To
	_ = inst.Save()
	app.outcome = fmt.Sprintf("%s Went back to %s.", msg, fluxerName(fromShort))
	app.UI.OK("Went back to %s. Your instance is running.", fluxerName(fromShort))
	if logPath != "" {
		fixLine(app, fmt.Sprintf("Read %s to see why.", logPath))
	} else {
		fixLine(app, "Run rexctl upgrade --verbose to see why.")
	}
	app.SetExit(ui.ExitFailed)
	return nil
}

func writeFailureLog(ctx context.Context, inst *instance.Instance, p *engine.Project, name string, cause error, waiting []string) string {
	ctx = context.WithoutCancel(ctx)
	if len(waiting) == 0 {
		waiting = notRunning(ctx, p)
	}
	path := inst.Path(instance.StateDirName, name)
	f, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o600)
	if err != nil {
		return ""
	}
	defer f.Close()
	fmt.Fprintf(f, "rexctl: %v\n", cause)
	if len(waiting) > 0 {
		fmt.Fprintf(f, "rexctl: not ready: %s\n", strings.Join(waiting, ", "))
	}
	fmt.Fprintln(f)
	_ = p.Logs(ctx, engine.LogsOptions{Tail: 200, Stdout: f}, waiting...)
	return path
}

func writeBackupLog(inst *instance.Instance, name string, cause error) string {
	text := fmt.Sprintf("rexctl: backup failed: %v\n", cause)
	var ee *ui.ExitError
	if errors.As(cause, &ee) && ee.Err != nil && ee.Msg != "" {
		text += fmt.Sprintf("rexctl: %v\n", ee.Err)
	}
	var dump *backup.DumpError
	if errors.As(cause, &dump) && dump.Output != "" {
		text += "\n" + dump.Output + "\n"
	}
	path := inst.Path(instance.StateDirName, name)
	if err := os.WriteFile(path, []byte(text), 0o600); err != nil {
		return ""
	}
	return path
}

func notRunning(ctx context.Context, p *engine.Project) []string {
	containers, err := p.PS(ctx)
	if err != nil {
		return nil
	}
	var out []string
	for _, c := range containers {
		ok := c.State == "running" && c.Health != "unhealthy" || stack.OneShot[c.Service] && c.State == "exited" && c.ExitCode == 0
		if !ok && !slices.Contains(out, c.Service) {
			out = append(out, c.Service)
		}
	}
	return out
}

func abortUpgrade(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine, cause, report error) error {
	pend := inst.State.Upgrade
	app.UI.Detail("%v", cause)
	if err := goBack(ctx, app, inst, e, pend.From); err != nil {
		var ee *ui.ExitError
		if errors.As(report, &ee) && ee.Msg != "" {
			msg, _, _ := strings.Cut(ee.Msg, ". ")
			app.UI.Fail(strings.TrimSuffix(msg, ".")+".", "")
		}
		return err
	}
	if ctx.Err() != nil {
		fromShort, _ := versionPair(pend.From, pend.To)
		app.UI.Info("Stopped the upgrade. %s is still running.", capitalise(fluxerName(fromShort)))
		return ctx.Err()
	}
	return report
}

func goBack(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine, version string) error {
	pend := inst.State.Upgrade
	cctx, cancel := context.WithTimeout(context.WithoutCancel(ctx), recoveryTimeout)
	defer cancel()
	if _, err := restoreRelease(cctx, app, inst, e, version); err != nil {
		logPath := writeFailureLog(cctx, inst, e.Named(inst.Project(), inst.Dir), "go-back-"+release.DirName(version)+".log", err, nil)
		fix := "Run rexctl doctor to see what's wrong."
		if logPath != "" {
			fix = fmt.Sprintf("Read %s to see why.", logPath)
		}
		if pend != nil && pend.Backup != "" {
			fix = strings.TrimSuffix(fix, ".") + ", or run rexctl rollback --restore-database to restore the backup from before the upgrade."
		}
		switch {
		case pend == nil:
		case phaseIndex(pend.Phase) < phaseIndex(release.PhaseRecreated):
			inst.State.Upgrade = nil
			_ = inst.Save()
		default:
			pend.BackFailed = true
			_ = inst.Save()
			fix = release.UnfinishedFix(pend)
			if logPath != "" {
				fix = fmt.Sprintf("Read %s to see why. %s", logPath, fix)
			}
		}
		return ui.Wrap(err, fmt.Sprintf("Couldn't go back to %s.", fluxerName(version)), fix)
	}
	inst.State.Upgrade = nil
	if err := inst.Save(); err != nil {
		return ui.Wrap(err, "Couldn't save your instance's state.", "Run rexctl doctor to see what's wrong.")
	}
	return nil
}

func restoreRelease(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine, version string) ([]string, error) {
	var put []string
	search := runningImage(ctx, e.Named(inst.Project(), inst.Dir), searchService)
	if version != inst.State.Release {
		restored, err := release.Restore(inst, version)
		if err != nil {
			return nil, err
		}
		put = restored.Settings
		if err := inst.Save(); err != nil {
			return put, err
		}
	}
	pull := false
	for _, ref := range inst.State.Images.Refs {
		if _, found, err := e.LocalImage(ctx, ref); err != nil || !found {
			pull = true
			break
		}
	}
	if err := e.Named(inst.Project(), inst.Dir).RemoveReplacements(ctx); err != nil {
		return put, err
	}
	_, err := deploy.Apply(ctx, inst, e, deploy.ApplyOptions{Pull: pull})
	if err != nil && engine.ClassOf(err) != engine.ClassDependencyFailed {
		return put, err
	}
	p, lerr := deploy.Load(ctx, inst, e)
	if lerr != nil {
		return put, lerr
	}
	if serr := keepSearchReadable(ctx, app, inst, p, search, version); serr != nil {
		return put, serr
	}
	if err != nil {
		if err := startEach(ctx, p, false); err != nil {
			return put, err
		}
	}
	started := map[string]bool{}
	return put, p.WaitReady(ctx, readyTimeout, func(r engine.Readiness) {
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

func finishUpgrade(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine) error {
	pend := inst.State.Upgrade
	next, err := release.SnapshotState(inst, pend.To)
	if err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't read the settings for Fluxer %s.", release.Short(pend.To)), "Run rexctl doctor to see what's wrong.")
	}
	p := e.Named(inst.Project(), inst.Dir)
	app.sendAdminImport(ctx, inst, p)
	if done, err := setup.Done(ctx, app.adminClient(inst, p)); err == nil {
		inst.State.SetupDone = done
	}
	inst.State.PreviousRelease = pend.From
	inst.State.Release = pend.To
	inst.State.Images = next.Images
	inst.State.Settings = next.Settings
	inst.State.Secrets = next.Secrets
	if ai := inst.State.AdminImport; ai != nil && ai.SentAt != "" {
		inst.State.AdminImport = nil
	}
	inst.State.FailedRelease = ""
	inst.State.Upgrade = nil
	if err := inst.Save(); err != nil {
		return ui.Wrap(err, "Couldn't save your instance's state.", "Run rexctl doctor to see what's wrong.")
	}
	if loaded, err := deploy.Load(ctx, inst, e); err == nil {
		if err := refreshBootUnit(ctx, inst, e, loaded); err != nil {
			app.UI.Detail("%v", err)
		}
	}
	if err := release.Prune(ctx, inst, e); err != nil {
		app.UI.Detail("%v", err)
	}
	_, toShort := versionPair(pend.From, pend.To)
	app.outcome = fmt.Sprintf("Upgraded to Fluxer %s.", toShort)
	app.UI.OK("Your instance is on Fluxer %s.", toShort)
	return nil
}

func recoverUpgrade(ctx context.Context, app *App, inst *instance.Instance, forward bool) error {
	pend := inst.State.Upgrade
	if pend == nil {
		return nil
	}
	phase := phaseIndex(pend.Phase)
	switch {
	case phase >= phaseIndex(release.PhaseVerified):
		e, err := app.Engine(ctx, inst)
		if err != nil {
			return err
		}
		return finishUpgrade(ctx, app, inst, e)
	case forward && forwardable(inst):
		e, err := app.Engine(ctx, inst)
		if err != nil {
			return err
		}
		_, toShort := versionPair(pend.From, pend.To)
		app.UI.Warn(fmt.Sprintf("The upgrade to Fluxer %s didn't finish.", toShort), "")
		return retryUpgrade(ctx, app, inst, e)
	case phase >= phaseIndex(release.PhasePulled) && release.HasSnapshot(inst, pend.From):
		e, err := app.Engine(ctx, inst)
		if err != nil {
			return err
		}
		fromShort, toShort := versionPair(pend.From, pend.To)
		app.UI.Warn(fmt.Sprintf("The upgrade to Fluxer %s didn't finish.", toShort), "")
		err = app.UI.Step(ctx, "Going back to "+fluxerName(fromShort), func(ctx context.Context, s *ui.Step) error {
			s.Done("Went back to %s. Your instance is running.", fluxerName(fromShort))
			return goBack(ctx, app, inst, e, pend.From)
		})
		if err == nil || !forward || !forwardable(inst) || ctx.Err() != nil {
			return err
		}
		var ee *ui.ExitError
		if errors.As(err, &ee) {
			app.UI.Fail(ee.Msg, "")
		}
		app.UI.Detail("%v", err)
		return retryUpgrade(ctx, app, inst, e)
	}
	clearUpgrade(inst)
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
		fix := fmt.Sprintf("Run sudo rexctl upgrade --dir %s.", inst.Dir)
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

func setPhase(inst *instance.Instance, from, to, phase string) error {
	if inst.State.Upgrade == nil {
		inst.State.Upgrade = &instance.Upgrade{From: from, To: to, StartedAt: time.Now().UTC()}
	}
	inst.State.Upgrade.Phase = phase
	return inst.Save()
}

func clearUpgrade(inst *instance.Instance) {
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

func resumeArgs(args []string, phase, dir string) []string {
	out := make([]string, 0, len(args)+4)
	flag := "--" + release.ResumeFlag
	for i := 0; i < len(args); i++ {
		a := args[i]
		if a == flag {
			i++
			continue
		}
		if strings.HasPrefix(a, flag+"=") {
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

func versionPair(a, b string) (string, string) {
	sa, sb := release.Short(a), release.Short(b)
	if sa == sb {
		return a, b
	}
	return sa, sb
}

func fixLine(app *App, fix string) {
	if app.UI.Mode() == ui.ModeJSON {
		return
	}
	fmt.Fprintf(app.UI.Stderr(), "  %s\n", fix)
}
