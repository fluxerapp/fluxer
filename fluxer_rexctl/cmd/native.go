// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"maps"
	"os"
	"runtime"
	"slices"
	"strconv"
	"strings"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/migrate"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/setup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const nativeCommands = "rexctl start, stop, restart, down, status, logs, config, backup, restore or upgrade"

var nativeComposeFlags = []string{"engine", "install-docker", "setup-podman", "take-over", "delete-old-data"}

func (a *App) composeOnly() error {
	name := a.command
	if name == "" {
		name = "This command"
	}
	return ui.Refused(fmt.Sprintf("%s needs Docker or Podman, and this instance runs natively without containers.", name), "With a native instance, use "+nativeCommands+".")
}

func nativeTransport(inst *instance.Instance) adminapi.Transport {
	n := native.New(inst)
	return adminapi.NewHost(n.OperatorSocket(), native.APIAddr, inst.State.Settings["FLUXER_CLIENT_IP_HEADER_NAME"])
}

func nativeRoot(again string) error {
	if runtime.GOOS != "linux" || !host.BootUnitSupported() {
		return ui.Refused("A native instance runs only on Linux with systemd.", "Run rexctl install without --experimental-native to use Docker or Podman.")
	}
	if euid() != 0 {
		return ui.Refused("A native instance is managed as root.", "Run sudo "+again+".")
	}
	return nil
}

func nativeAlone(ctx context.Context) error {
	if host.StandaloneNativeInUse(ctx) {
		return nativeFailure(host.ErrStandaloneNative, "")
	}
	return nil
}

func nativeFailure(err error, what string) error {
	var ee *ui.ExitError
	switch {
	case err == nil:
		return nil
	case errors.As(err, &ee), errors.Is(err, context.Canceled):
		return err
	case errors.Is(err, host.ErrStandaloneNative):
		return ui.Refused("fluxerd.service already runs Fluxer on this machine.", "Run systemctl disable --now fluxerd first, or keep using that install.")
	case errors.Is(err, native.ErrNoApt):
		return ui.Refused("The native runtime installs its packages with apt, and this machine has no apt.", "Use Debian 13, or run rexctl install without --experimental-native.")
	case errors.Is(err, native.ErrNotRoot):
		return ui.Refused("A native instance is managed as root.", "Run the command again with sudo.")
	case errors.Is(err, native.ErrProxyPort):
		return ui.Refused("Behind a proxy, a native instance listens on port "+native.ProxyPort+".", "Set edge.bind in "+instance.StateDirName+"/"+instance.StateFileName+" to an address on port "+native.ProxyPort+", like 127.0.0.1:"+native.ProxyPort+".")
	case errors.Is(err, native.ErrChecksum):
		return ui.Wrap(err, "The Fluxer download didn't match its checksum.", "Try again.")
	}
	return ui.Wrap(err, what, "Run the command again with --verbose to see why.")
}

func saveNative(inst *instance.Instance) error {
	if err := inst.Save(); err != nil {
		return ui.Wrap(err, "Couldn't save your instance's state.", "Check that you can write to "+inst.Dir+", then try again.")
	}
	return nil
}

func applyNative(ctx context.Context, inst *instance.Instance, n *native.Stack, set *stack.Set) (native.Applied, error) {
	a, err := n.Apply(ctx, set)
	if err != nil {
		return a, nativeFailure(err, "Couldn't set up Fluxer's service.")
	}
	return a, saveNative(inst)
}

func waitForNative(ctx context.Context, n *native.Stack, s *ui.Step, verb string) error {
	return nativeReadyFailure(n, n.WaitReady(ctx, engine.DefaultReadyTimeout, showReadiness(s, verb+" Fluxer")))
}

func nativeReadyFailure(n *native.Stack, err error) error {
	if err == nil || errors.Is(err, context.Canceled) {
		return err
	}
	fix := fmt.Sprintf("Run rexctl logs, or journalctl -u %s, to see why.", n.Unit())
	var nr *engine.NotReadyError
	if errors.As(err, &nr) {
		msg := "Fluxer didn't become ready in time."
		switch {
		case nr.Fatal || slices.Contains(nr.Crashing, native.Supervisor):
			msg = "Fluxer's service keeps stopping."
		case len(nr.Crashing) > 0:
			msg = checks.Services(nr.Crashing, "keeps stopping.", "keep stopping.")
		}
		return ui.Wrap(err, msg, fix)
	}
	return ui.Wrap(err, "Fluxer didn't start.", fix)
}

func startNative(ctx context.Context, inst *instance.Instance, n *native.Stack, set *stack.Set, s *ui.Step, verb string) error {
	a, err := applyNative(ctx, inst, n, set)
	if err != nil {
		return err
	}
	if !a.Started && !a.Restarted {
		if err := n.Restart(ctx); err != nil {
			return nativeFailure(err, "Couldn't restart Fluxer.")
		}
	}
	return waitForNative(ctx, n, s, verb)
}

func startNativeStep(ctx context.Context, app *App, inst *instance.Instance, n *native.Stack, set *stack.Set, title, verb, done string) error {
	return app.UI.Step(ctx, title, func(ctx context.Context, s *ui.Step) error {
		if err := startNative(ctx, inst, n, set, s, verb); err != nil {
			return err
		}
		s.Done("%s", done)
		return nil
	})
}

func runNativeStart(ctx context.Context, app *App, inst *instance.Instance) error {
	if err := nativeRoot("rexctl start"); err != nil {
		return err
	}
	if err := nativeAlone(ctx); err != nil {
		return err
	}
	return app.Locked(ctx, inst, func() error {
		set, err := deploy.ReleaseSet(inst)
		if err != nil {
			return err
		}
		n := native.New(inst)
		inst.State.Stopped = false
		if err := saveNative(inst); err != nil {
			return err
		}
		err = app.UI.Step(ctx, "Starting Fluxer", func(ctx context.Context, s *ui.Step) error {
			a, err := applyNative(ctx, inst, n, set)
			if err != nil {
				return err
			}
			if !a.Started && !a.Restarted {
				if err := n.StartStopped(ctx); err != nil {
					return nativeFailure(err, "Couldn't start Fluxer's stopped services.")
				}
			}
			if err := waitForNative(ctx, n, s, "Starting"); err != nil {
				return err
			}
			s.Done("Started Fluxer at %s.", inst.Origin())
			return nil
		})
		if err != nil {
			return err
		}
		if inst.State.Installing {
			inst.State.Installing = false
			if err := saveNative(inst); err != nil {
				return err
			}
		}
		app.sendAdminImport(ctx, inst, nil)
		return nil
	})
}

func runNativeStop(ctx context.Context, app *App, inst *instance.Instance, down bool) error {
	again := "rexctl stop"
	if down {
		again = "rexctl down"
	}
	if err := nativeRoot(again); err != nil {
		return err
	}
	return app.Locked(ctx, inst, func() error {
		n := native.New(inst)
		err := app.UI.Step(ctx, "Stopping Fluxer", func(ctx context.Context, s *ui.Step) error {
			if err := n.Stop(ctx); err != nil {
				return ui.Wrap(err, "Couldn't stop Fluxer.", "Run rexctl status to see what's still running.")
			}
			if err := n.Disable(ctx); err != nil {
				app.UI.Detail("%v", err)
			}
			s.Done("Stopped Fluxer.")
			return nil
		})
		if err != nil {
			return err
		}
		inst.State.Stopped = true
		if err := inst.Save(); err != nil {
			return ui.Wrap(err, "Couldn't save your instance's state.", "Run "+again+" again.")
		}
		if down {
			app.UI.Info("A native instance has no containers to remove, so Fluxer is stopped as with rexctl stop. Your data and settings are kept.")
		}
		app.UI.Info("Run rexctl start to start it again.")
		return nil
	})
}

func runNativeRestart(ctx context.Context, app *App, inst *instance.Instance) error {
	if err := nativeRoot("rexctl restart"); err != nil {
		return err
	}
	if err := nativeAlone(ctx); err != nil {
		return err
	}
	return app.Locked(ctx, inst, func() error {
		set, err := deploy.ReleaseSet(inst)
		if err != nil {
			return err
		}
		n := native.New(inst)
		return app.UI.Step(ctx, "Restarting Fluxer", func(ctx context.Context, s *ui.Step) error {
			if err := startNative(ctx, inst, n, set, s, "Restarting"); err != nil {
				return err
			}
			if inst.State.Stopped {
				inst.State.Stopped = false
				if err := inst.Save(); err != nil {
					return ui.Wrap(err, "Couldn't save your instance's state.", "Run rexctl restart again.")
				}
			}
			s.Done("Restarted Fluxer.")
			return nil
		})
	})
}

func runNativeLogs(ctx context.Context, app *App, inst *instance.Instance, follow bool, since string, services []string) error {
	opts := engine.LogsOptions{Follow: follow, Since: since, Stdout: app.UI.Stdout()}
	if since == "" {
		opts.Tail = 200
	}
	if err := native.New(inst).Logs(ctx, opts, services...); err != nil {
		if ctx.Err() != nil {
			return nil
		}
		return ui.Wrap(err, "Couldn't read the logs.", "Run rexctl logs as root, or as an account in the systemd-journal group.")
	}
	return nil
}

func runNativeStatus(ctx context.Context, app *App, inst *instance.Instance) error {
	n := native.New(inst)
	st := n.Status(ctx)
	out := statusJSON{
		URL:      inst.Origin(),
		Domain:   inst.Domain(),
		Release:  n.Running(),
		Engine:   instance.RuntimeNative,
		Project:  inst.Project(),
		Dir:      inst.Dir,
		Services: []statusService{},
	}
	running := st.Unit == "active" || st.Unit == "activating" || st.Unit == "reloading"
	var down, waiting []string
	for _, c := range st.Containers {
		out.Services = append(out.Services, statusService{Name: c.Service, State: c.State, Health: c.Health})
		switch {
		case c.State == "running":
			if c.Health == "unhealthy" || c.Health == "starting" {
				waiting = append(waiting, c.Service)
			}
		case stack.OneShot[c.Service] && c.State == "exited" && c.ExitCode == 0:
		case c.State == "created":
			waiting = append(waiting, c.Service)
		default:
			down = append(down, c.Service)
		}
	}
	out.Running = running
	out.Healthy = st.Ready()
	if !running || st.PSErr != nil || len(down) > 0 {
		app.SetExit(ui.ExitRefused)
	}
	if app.Flags.JSON {
		return app.UI.JSON(out)
	}
	switch {
	case inst.Busy() && inst.State.Upgrade != nil:
		app.UI.Warn(fmt.Sprintf("Fluxer is being upgraded to %s.", inst.State.Upgrade.To), "Run rexctl status again when the upgrade is done.")
	case st.Unit == "failed":
		app.UI.Fail("Fluxer's service stopped after an error.", "Run rexctl logs to see why, then run rexctl start.")
	case !running && inst.State.InstallUnfinished():
		app.UI.Fail("The install didn't finish.", "Run rexctl install --experimental-native again to finish it.")
	case !running && inst.State.Stopped:
		app.UI.Fail("Fluxer is stopped.", "Run rexctl start.")
	case !running:
		app.UI.Fail("Fluxer isn't running.", "Run rexctl start.")
	case st.PSErr != nil && euid() != 0:
		app.UI.Warn("Fluxer's service is running, and only root can see its services.", "Run sudo rexctl status.")
	case st.PSErr != nil:
		app.UI.Warn("Fluxer's service is starting.", "Run rexctl status again in a minute.")
	case len(down) > 0:
		app.UI.Fail(checks.Services(down, "isn't running.", "aren't running."), "Run rexctl logs to see why.")
	case len(waiting) > 0:
		app.UI.Warn(checks.Services(waiting, "isn't ready yet.", "aren't ready yet."), "Run rexctl logs if this doesn't clear up in a few minutes.")
	case !out.Healthy:
		app.UI.Warn("Fluxer is starting.", "Run rexctl status again in a minute.")
	default:
		app.UI.OK("Fluxer is running.")
	}
	if st.Restarts > 0 {
		app.UI.Info("systemd restarted Fluxer's service %s since it last started.", plural(st.Restarts, "time", "times"))
	}
	if out.Healthy && !inst.State.SetupDone {
		client := app.adminClient(inst, nil)
		if done, err := setup.Done(ctx, client); err == nil && !done {
			app.UI.Warn("Setup isn't finished yet.", fmt.Sprintf("Open %s and enter the setup token, or run rexctl install --experimental-native again to see it.", inst.Origin()))
		}
	}
	if dir, ok := n.DataDir(stack.ObjectsVolume); ok && inst.State.Settings[settings.StorageBackend] == settings.StorageFilesystem {
		warnCheck(app, checks.ObjectsFS(dir))
	}
	for _, s := range out.Services {
		health := s.Health
		if health == "" {
			health = "-"
		}
		app.UI.Detail("%-20s %-10s %s", s.Name, s.State, health)
	}
	rows := [][]string{{"Address", out.URL}}
	if out.Release != "" {
		rows = append(rows, []string{"Version", out.Release})
	}
	rows = append(rows, []string{"Service", n.Unit()}, []string{"Folder", out.Dir})
	for _, r := range rows {
		app.UI.Result(fmt.Sprintf("%-9s %s", r[0], r[1]))
	}
	return nil
}

func nativeBackup(ctx context.Context, app *App, inst *instance.Instance, n *native.Stack, set *stack.Set, title, reason string) (string, error) {
	var path string
	err := app.UI.Step(ctx, title, func(ctx context.Context, s *ui.Step) error {
		var err error
		if path, err = n.Backup(ctx, set, inst.BackupDir(), reason, detailWriter{app.UI}); err != nil {
			return ui.Wrap(err, "Couldn't back up your instance.", "Run rexctl backup --verbose to see why.")
		}
		size := int64(0)
		if info, err := os.Stat(path); err == nil {
			size = info.Size()
		}
		s.Done("Backed up your instance (%s).", ui.Bytes(size))
		return nil
	})
	return path, err
}

func runNativeBackup(ctx context.Context, app *App, inst *instance.Instance) error {
	if err := nativeRoot("rexctl backup"); err != nil {
		return err
	}
	return app.Locked(ctx, inst, func() error {
		set, err := deploy.ReleaseSet(inst)
		if err != nil {
			return err
		}
		n := native.New(inst)
		path, err := nativeBackup(ctx, app, inst, n, set, "Backing up your instance", "manual")
		if err != nil {
			return err
		}
		if !inst.State.Stopped {
			err := app.UI.Step(ctx, "Waiting for Fluxer to start again", func(ctx context.Context, s *ui.Step) error {
				if err := waitForNative(ctx, n, s, "Starting"); err != nil {
					return err
				}
				s.Done("Fluxer is running again.")
				return nil
			})
			if err != nil {
				return ui.Wrap(err, fmt.Sprintf("Saved the backup to %s, but Fluxer didn't start again.", path), fmt.Sprintf("Run rexctl logs, or journalctl -u %s, to see why.", n.Unit()))
			}
		}
		pruneBackups(app, inst)
		app.UI.Result(path)
		return nil
	})
}

func adoptNativeSecrets(inst *instance.Instance, n *native.Stack) error {
	restored, err := n.ReadSecrets()
	if err != nil {
		return err
	}
	keys := []string{setup.TokenKey}
	for _, s := range secrets.Required {
		keys = append(keys, s.Key)
	}
	for _, k := range keys {
		if v, ok := restored[k]; ok && v != "" {
			inst.State.Secrets[k] = v
		}
	}
	return inst.Save()
}

func runNativeRestore(ctx context.Context, app *App, inst *instance.Instance, path string, m *backup.Manifest, confirm string) error {
	if err := nativeRoot("rexctl restore"); err != nil {
		return err
	}
	what := fmt.Sprintf("This replaces everything on %s with the backup made on %s.", inst.Domain(), backupTime(m.CreatedAt))
	if err := app.UI.ConfirmDestructive(what, inst.Domain(), confirm); err != nil {
		return err
	}
	return app.Locked(ctx, inst, func() error {
		set, err := deploy.ReleaseSet(inst)
		if err != nil {
			return err
		}
		n := native.New(inst)
		return app.UI.Step(ctx, "Restoring your instance", func(ctx context.Context, s *ui.Step) error {
			return restoreNative(ctx, app, inst, n, set, path, m, s)
		})
	})
}

func restoreNative(ctx context.Context, app *App, inst *instance.Instance, n *native.Stack, set *stack.Set, path string, m *backup.Manifest, s *ui.Step) error {
	if err := n.Restore(ctx, set, path, detailWriter{app.UI}); err != nil {
		return ui.Wrap(err, "Couldn't restore the backup.", "Run rexctl restore --verbose to see why.")
	}
	if err := adoptNativeSecrets(inst, n); err != nil {
		return ui.Wrap(err, "Restored the backup, but couldn't read its secrets.", "Check "+n.SecretsFile()+", then run rexctl restart.")
	}
	if err := forgetRestoredMigrations(inst, m.CreatedAt); err != nil {
		return err
	}
	if err := startNative(ctx, inst, n, set, s, "Starting"); err != nil {
		return err
	}
	s.Done("Restored %s from the backup made on %s.", inst.Domain(), backupTime(m.CreatedAt))
	return nil
}

func runNativeUpgrade(ctx context.Context, app *App, inst *instance.Instance, f *upgradeFlags) error {
	if err := nativeRoot("rexctl upgrade"); err != nil {
		return err
	}
	if err := nativeAlone(ctx); err != nil {
		return err
	}
	n := native.New(inst)
	if u := inst.State.Upgrade; u != nil && !f.DryRun {
		if err := app.Locked(ctx, inst, func() error { return recoverNativeUpgrade(ctx, app, inst, n) }); err != nil {
			return err
		}
	}
	cur := n.Running()
	rel, err := resolveRelease(ctx, f.To)
	if err != nil {
		return err
	}
	if err := release.CheckPath(cur, rel); err != nil {
		return tooOldError(err)
	}
	from, to := versionPair(cur, rel.Version)
	switch cmp := release.Compare(rel.Version, cur); {
	case cmp == 0 && n.Running() == rel.Version:
		app.UI.OK("Fluxer %s is up to date.", to)
		return nil
	case cmp < 0:
		return ui.Refused(fmt.Sprintf("Fluxer %s is older than the release you run, %s.", to, from), "Pass --to with a newer release.")
	}
	if err := release.CheckTarget(nil, rel, inst.State.Schema); err != nil {
		return floorRefusal(err, to)
	}
	needSelf := release.Compare(rel.Version, app.Version) != 0
	if f.DryRun {
		app.UI.Info("rexctl would do this:")
		if needSelf {
			app.UI.Info("  Update rexctl to %s.", to)
		}
		app.UI.Info("  Download the native build of Fluxer %s.", to)
		if !f.Prefetch && !f.NoBackup {
			app.UI.Info("  Back up your instance.")
		}
		if !f.Prefetch {
			app.UI.Info("  Switch to Fluxer %s and restart, or go back to %s if it doesn't start.", to, from)
		}
		return nil
	}
	if !f.Prefetch {
		app.UI.Info("Fluxer keeps running while the new version downloads, then restarts.")
		ok, err := app.UI.Confirm("Upgrade?", true)
		if err != nil {
			return err
		}
		if !ok {
			app.UI.Info("Nothing changed.")
			return nil
		}
	}
	if !needSelf {
		return app.Locked(ctx, inst, func() error {
			if err := setPhase(inst, cur, rel.Version, release.PhaseResolved); err != nil {
				return err
			}
			return nativeUpgradeStack(ctx, app, inst, n, rel, f)
		})
	}
	asset, err := rel.AssetFor(runtime.GOOS, runtime.GOARCH)
	if err != nil {
		return ui.Failed(fmt.Sprintf("There's no rexctl %s for this machine.", to), "")
	}
	err = app.Locked(ctx, inst, func() error {
		if err := setPhase(inst, cur, rel.Version, release.PhaseResolved); err != nil {
			return err
		}
		err := app.UI.Step(ctx, "Updating rexctl", func(ctx context.Context, s *ui.Step) error {
			if err := selfUpdate(ctx, asset); err != nil {
				return err
			}
			s.Done("Updated rexctl to %s.", to)
			return nil
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

func resumeNativeUpgrade(ctx context.Context, app *App, inst *instance.Instance, f *upgradeFlags) error {
	if err := nativeRoot("rexctl upgrade"); err != nil {
		return err
	}
	return app.Locked(ctx, inst, func() error {
		p := inst.State.Upgrade
		if p == nil || p.Phase != f.Resume {
			return ui.Refused("There's no upgrade to continue.", "Run rexctl upgrade.")
		}
		if release.Valid(app.Version) && release.Compare(app.Version, p.To) != 0 {
			clearUpgrade(inst)
			return ui.Failed(fmt.Sprintf("rexctl didn't update to %s.", p.To), "Run rexctl upgrade again.")
		}
		rel, err := resolveRelease(ctx, p.To)
		if err != nil {
			clearUpgrade(inst)
			return err
		}
		return nativeUpgradeStack(ctx, app, inst, native.New(inst), rel, f)
	})
}

func nativeUpgradeStack(ctx context.Context, app *App, inst *instance.Instance, n *native.Stack, rel *release.Release, f *upgradeFlags) error {
	from := inst.State.Upgrade.From
	_, toShort := versionPair(from, rel.Version)
	oldSet, err := deploy.ReleaseSet(inst)
	if err != nil {
		clearUpgrade(inst)
		return err
	}
	set := stack.Embedded()
	if err := release.WriteRelease(inst, set); err != nil {
		clearUpgrade(inst)
		return ui.Wrap(err, fmt.Sprintf("Couldn't write Fluxer's files to %s.", inst.Dir), "Run rexctl upgrade again.")
	}
	err = app.UI.Step(ctx, fmt.Sprintf("Downloading Fluxer %s", toShort), func(ctx context.Context, s *ui.Step) error {
		if _, err := n.Fetch(ctx, rel, s.Progress); err != nil {
			return nativeFailure(err, fmt.Sprintf("Couldn't download Fluxer %s.", toShort))
		}
		s.Done("Downloaded Fluxer %s.", toShort)
		return nil
	})
	if err == nil {
		err = nativePackagesStep(ctx, app, n, rel.Version)
	}
	if err != nil {
		clearUpgrade(inst)
		return err
	}
	if f.Prefetch {
		clearUpgrade(inst)
		app.UI.Info("Run rexctl upgrade to switch to Fluxer %s.", toShort)
		return nil
	}
	if err := setPhase(inst, from, rel.Version, release.PhasePulled); err != nil {
		return err
	}
	backupPath := ""
	if !f.NoBackup {
		if backupPath, err = nativeBackup(ctx, app, inst, n, oldSet, "Backing up your instance before the upgrade", "pre-upgrade"); err != nil {
			clearUpgrade(inst)
			return err
		}
		inst.State.Upgrade.Backup = backupPath
		if err := setPhase(inst, from, rel.Version, release.PhaseBackedUp); err != nil {
			return err
		}
	}
	if err := n.Flip(rel.Version); err != nil {
		clearUpgrade(inst)
		return nativeFailure(err, fmt.Sprintf("Couldn't switch to Fluxer %s.", toShort))
	}
	inst.State.PreviousRelease = inst.State.Release
	inst.State.Release = set.Release
	if err := setPhase(inst, from, rel.Version, release.PhaseRecreated); err != nil {
		return err
	}
	err = startNativeStep(ctx, app, inst, n, set, fmt.Sprintf("Starting Fluxer %s", toShort), "Starting", fmt.Sprintf("Fluxer %s is running.", toShort))
	if err != nil {
		return nativeGoBack(ctx, app, inst, n, oldSet, rel, backupPath, err)
	}
	inst.State.Upgrade = nil
	inst.State.FailedRelease = ""
	if backupPath != "" {
		inst.State.PreviousBackup = backupPath
	}
	if err := saveNative(inst); err != nil {
		return err
	}
	if err := n.Prune(); err != nil {
		app.UI.Detail("%v", err)
	}
	pruneBackups(app, inst)
	app.UI.OK("Upgraded Fluxer to %s.", toShort)
	return nil
}

func nativeGoBack(ctx context.Context, app *App, inst *instance.Instance, n *native.Stack, oldSet *stack.Set, rel *release.Release, backupPath string, cause error) error {
	from := inst.State.Upgrade.From
	fromShort, toShort := versionPair(from, rel.Version)
	err := app.UI.Step(ctx, fmt.Sprintf("Going back to Fluxer %s", fromShort), func(ctx context.Context, s *ui.Step) error {
		if _, err := n.GoBack(); err != nil {
			return err
		}
		inst.State.Release = inst.State.PreviousRelease
		inst.State.FailedRelease = rel.Version
		if err := saveNative(inst); err != nil {
			return err
		}
		if rel.SchemaChanged && backupPath != "" {
			if err := n.Restore(ctx, oldSet, backupPath, detailWriter{app.UI}); err != nil {
				return err
			}
		}
		if err := startNative(ctx, inst, n, oldSet, s, "Starting"); err != nil {
			return err
		}
		s.Done("Fluxer %s is running again.", fromShort)
		return nil
	})
	inst.State.Upgrade = nil
	_ = inst.Save()
	if err != nil {
		return ui.Wrap(errors.Join(cause, err), fmt.Sprintf("Fluxer %s didn't start, and going back to %s didn't work either.", toShort, fromShort), fmt.Sprintf("Run rexctl logs, or journalctl -u %s, to see why.", n.Unit()))
	}
	return ui.Wrap(cause, fmt.Sprintf("Fluxer %s didn't start, so rexctl went back to %s.", toShort, fromShort), fmt.Sprintf("Run rexctl logs, or journalctl -u %s, to see why.", n.Unit()))
}

func recoverNativeUpgrade(ctx context.Context, app *App, inst *instance.Instance, n *native.Stack) error {
	u := inst.State.Upgrade
	if phaseIndex(u.Phase) < phaseIndex(release.PhaseRecreated) {
		clearUpgrade(inst)
		return nil
	}
	app.UI.Warn(fmt.Sprintf("The upgrade to %s didn't finish.", u.To), "rexctl goes back to the release you ran before, then upgrades again.")
	if _, err := n.GoBack(); err != nil {
		return nativeFailure(err, "Couldn't go back to the release you ran before.")
	}
	inst.State.Release = inst.State.PreviousRelease
	inst.State.Upgrade = nil
	if err := saveNative(inst); err != nil {
		return err
	}
	set, err := deploy.ReleaseSet(inst)
	if err != nil {
		return err
	}
	return startNativeStep(ctx, app, inst, n, set, "Starting Fluxer", "Starting", "Fluxer is running again.")
}

func otherNativeInstance(dir string) string {
	for _, d := range instance.ReadRecord(instance.OwnScope()).Instances {
		if d == dir {
			continue
		}
		if inst, err := instance.Find(d); err == nil && inst.State.IsNative() {
			return d
		}
	}
	return ""
}

func runNativeInstall(ctx context.Context, app *App, c *cobra.Command, f *installFlags) error {
	if err := nativeRoot("rexctl install --experimental-native"); err != nil {
		return err
	}
	for _, name := range nativeComposeFlags {
		if c.Flags().Changed(name) {
			return ui.Usage(fmt.Sprintf("--experimental-native doesn't take --%s.", name))
		}
	}
	if f.Storage.passed(c) {
		return ui.Usage("--experimental-native doesn't take the storage options yet.")
	}
	if err := nativeAlone(ctx); err != nil {
		return err
	}
	dir, err := installTarget(app)
	if err != nil {
		return err
	}
	in := &installer{app: app, c: c, f: f, dir: dir, ran: map[string]bool{}}
	inst, err := instance.Find(dir)
	switch {
	case errors.Is(err, instance.ErrNotFound) && native.HoldsReleases(dir):
		return ui.Refused(fmt.Sprintf("%s holds a Fluxer install from install.sh, which runs as %s.", dir, host.StandaloneNativeUnit), "Pass --dir with another folder, or keep using that install.")
	case errors.Is(err, instance.ErrNotFound):
	case errors.Is(err, instance.ErrLegacy):
		return ui.Refused(fmt.Sprintf("%s has a Fluxer install from the old installer.", dir), "Pass --dir with another folder.")
	case err != nil:
		return err
	case inst.State.IsNative() && !inst.State.SetupDone:
		app.UI.Info("Continuing the install in %s.", inst.Dir)
		f.BehindProxy = inst.State.Settings[settings.EdgeMode] == settings.EdgeProxy || inst.State.Settings[settings.EdgeMode] == settings.EdgeTunnel
		if err := in.askSetupWhere(); err != nil {
			return err
		}
		if err := in.collectAnswers(inst.Domain()); err != nil {
			return err
		}
		return app.Locked(ctx, inst, func() error { return in.startNative(ctx, inst) })
	default:
		return ui.Refused(fmt.Sprintf("Fluxer is already installed in %s.", dir), "Run rexctl status to check it, or rexctl upgrade to upgrade it.")
	}
	if other := otherNativeInstance(dir); other != "" {
		return ui.Refused(fmt.Sprintf("This machine already runs a native Fluxer instance in %s.", other), "A machine runs one native instance. Pass --dir "+other+" to manage it.")
	}
	if err := in.missingFlags(); err != nil {
		return err
	}
	app.UI.Welcome()
	if in.host, err = host.Detect(ctx, dir); err != nil {
		return err
	}
	in.ran["engine"], in.ran["podman-subids"], in.ran["windows-linux-containers"] = true, true, true
	if in.runChecks(ctx, checks.Preinstall, "memory", "disk") {
		return notReady()
	}
	if err := in.offerProxy(); err != nil {
		return err
	}
	if err := app.UI.Ask(c, ui.Field{
		Flag:     "domain",
		Title:    "What address will people use to open your instance?",
		Value:    &f.Domain,
		Validate: validateDomain,
	}); err != nil {
		return err
	}
	f.Domain = normaliseDomain(f.Domain)
	if f.BehindProxy {
		if _, err := native.ProxyListen(f.Bind); err != nil {
			return ui.Usage("Behind a proxy, a native instance listens on port " + native.ProxyPort + ". Pass --bind with port " + native.ProxyPort + ", like 127.0.0.1:" + native.ProxyPort + ".")
		}
	}
	if err := in.openFirewall(ctx); err != nil {
		return err
	}
	if err := in.askSetupWhere(); err != nil {
		return err
	}
	if err := in.collectAnswers(f.Domain); err != nil {
		return err
	}
	if f.DryRun {
		return in.printNativePlan()
	}
	inst, err = instance.Create(dir, f.Project)
	if err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't create %s.", dir), "Check that you can write to it, or pass --dir.")
	}
	inst.State.Installing = true
	inst.State.Runtime = instance.RuntimeNative
	return app.Locked(ctx, inst, func() error {
		if err := in.writeNative(inst); err != nil {
			return err
		}
		return in.startNative(ctx, inst)
	})
}

func (in *installer) printNativePlan() error {
	u := in.app.UI
	u.Info("rexctl would do this:")
	steps := append([]string{}, in.plan...)
	steps = append(steps,
		"Install the system packages Fluxer needs with apt.",
		fmt.Sprintf("Save Fluxer %s and its settings in %s.", stack.Embedded().Release, in.dir),
		"Download the native build of Fluxer.",
		in.nativeModesStep(),
	)
	if in.f.NoStart {
		steps = append(steps, "Leave Fluxer stopped.")
	} else {
		steps = append(steps, fmt.Sprintf("Start it as the systemd service %s.service.", in.f.Project))
	}
	for _, s := range steps {
		u.Info("  %s", s)
	}
	return nil
}

func (in *installer) nativeModesStep() string {
	if in.f.NoVoice {
		return "Run the Rust services in one process, keep uploads as plain files and keep the cache in Postgres."
	}
	return "Run the Rust services in one process, keep uploads as plain files, keep the cache in Postgres and run the bundled LiveKit for calls."
}

func (in *installer) writeNative(inst *instance.Instance) error {
	f := in.f
	st := inst.State
	set := stack.Embedded()
	st.Settings[settings.Domain] = normaliseDomain(f.Domain)
	if f.BehindProxy {
		st.Settings[settings.EdgeMode] = settings.EdgeProxy
		st.Settings[settings.EdgeBind] = f.Bind
	}
	if in.plainHTTP() {
		st.Settings[publicSchemeVar] = "http"
		in.app.UI.Info(plainHTTPNote, normaliseDomain(f.Domain))
	}
	st.Settings[settings.RustProcess] = settings.RustSingle
	st.Settings[settings.StorageBackend] = settings.StorageFilesystem
	st.Settings[settings.KVMode] = settings.KVPostgres
	if f.NoVoice {
		st.Settings[settings.VoiceBundled] = "false"
	} else {
		st.Settings[migrate.LiveKitEnabledVar] = "true"
	}
	if f.VoiceTCPPort > 0 {
		st.Settings[settings.VoiceTCPPort] = strconv.Itoa(f.VoiceTCPPort)
	}
	if f.VoiceUDPPort > 0 {
		st.Settings[settings.VoiceUDPPort] = strconv.Itoa(f.VoiceUDPPort)
	}
	if f.VoiceIP != "" {
		st.Settings[settings.VoicePublicIP] = f.VoiceIP
	}
	if f.MemoryProfile != "" {
		st.Settings[settings.MemoryProfile] = f.MemoryProfile
	}
	if _, err := secrets.Mint(st.Secrets, false); err != nil {
		return ui.Wrap(err, "Couldn't create your instance's secrets.", "Run rexctl install --experimental-native again.")
	}
	if _, err := setup.EnsureToken(st.Secrets); err != nil {
		return ui.Wrap(err, "Couldn't create a setup token.", "Run rexctl install --experimental-native again.")
	}
	st.Release = set.Release
	if err := release.WriteRelease(inst, set); err != nil {
		return saveFailed(inst, err, fmt.Sprintf("Couldn't write Fluxer's files to %s.", inst.Dir), "rexctl install --experimental-native")
	}
	if err := inst.Save(); err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't save your settings in %s.", inst.Dir), "Check that you can write to it, then run rexctl install --experimental-native again.")
	}
	in.showNativeSizing(inst, set)
	return nil
}

var nativeSizingRows = []struct{ label, key string }{
	{"Rust service threads", stack.RustWorkerThreadsVar},
	{"Media proxy threads", "FLUXER_MEDIA_PROXY_TOKIO_WORKER_THREADS"},
	{"API database connections", "FLUXER_API_POSTGRES_MAX_CONNECTIONS"},
	{"Cache database connections", stack.KVPoolVar},
	{"Cache size limit", stack.KVMaxBytesVar},
	{"Postgres connections", "FLUXER_POSTGRES_SERVER_MAX_CONNECTIONS"},
	{"Postgres shared buffers", "FLUXER_POSTGRES_SHARED_BUFFERS"},
	{"Postgres work memory", "FLUXER_POSTGRES_WORK_MEM"},
	{"Search indexing threads", "FLUXER_MEILISEARCH_MAX_INDEXING_THREADS"},
	{"Search indexing memory", "FLUXER_MEILISEARCH_MAX_INDEXING_MEMORY"},
}

func (in *installer) showNativeSizing(inst *instance.Instance, set *stack.Set) {
	env, _, err := native.New(inst).Environment(set)
	if err != nil {
		in.app.UI.Detail("%v", err)
		return
	}
	hw := settings.Hardware()
	if profile := inst.State.Settings[settings.MemoryProfile]; profile != "" {
		in.app.UI.Info("Sized Fluxer for %s with the %s memory profile:", plural(hw.CPUs, "CPU", "CPUs"), profile)
	} else {
		in.app.UI.Info("Sized Fluxer for %s and %s of memory:", plural(hw.CPUs, "CPU", "CPUs"), ui.Bytes(int64(hw.MemoryBytes)))
	}
	for _, r := range nativeSizingRows {
		v := strings.TrimSpace(env[r.key])
		if v == "" {
			continue
		}
		if r.key == stack.KVMaxBytesVar {
			if n, err := strconv.ParseInt(v, 10, 64); err == nil {
				v = ui.Bytes(n)
			}
		}
		in.app.UI.Info("  %-27s %s", r.label, v)
	}
}

func (in *installer) startNative(ctx context.Context, inst *instance.Instance) error {
	u := in.app.UI
	in.app.remember(inst.Dir)
	origin := inst.Origin()
	token := setup.Token(inst.State.Secrets)
	n := native.New(inst)
	set, err := deploy.ReleaseSet(inst)
	if err != nil {
		return err
	}
	version := inst.State.Release
	if !release.Valid(version) {
		version = ""
	}
	rel, err := resolveRelease(ctx, version)
	if err != nil {
		return err
	}
	err = u.Step(ctx, "Downloading Fluxer", func(ctx context.Context, s *ui.Step) error {
		if _, err := n.Fetch(ctx, rel, s.Progress); err != nil {
			return nativeFailure(err, "Couldn't download Fluxer.")
		}
		if err := n.Flip(rel.Version); err != nil {
			return nativeFailure(err, "Couldn't unpack Fluxer.")
		}
		s.Done("Downloaded Fluxer %s.", rel.Version)
		return nil
	})
	if err != nil {
		return err
	}
	if err := nativePackagesStep(ctx, in.app, n, rel.Version); err != nil {
		return err
	}
	if in.f.NoStart {
		if _, err := n.Render(set); err != nil {
			return nativeFailure(err, "Couldn't write Fluxer's settings.")
		}
		inst.State.Stopped = true
		inst.State.Installing = false
		if err := saveNative(inst); err != nil {
			return err
		}
		u.OK("Saved Fluxer and its settings in %s.", inst.Dir)
		u.Info("Run rexctl start to start it.")
		if in.f.SetupWhere != "terminal" {
			u.Info("Then open %s and enter this setup token to create your admin account.", origin)
			u.Highlight(token)
		} else {
			u.Info("Then run rexctl install --experimental-native again to create your admin account.")
		}
		return nil
	}
	err = u.Step(ctx, "Starting Fluxer", func(ctx context.Context, s *ui.Step) error {
		if _, err := applyNative(ctx, inst, n, set); err != nil {
			return err
		}
		if err := waitForNative(ctx, n, s, "Starting"); err != nil {
			return err
		}
		s.Done("Started Fluxer.")
		return nil
	})
	if err != nil {
		return err
	}
	inst.State.Stopped = false
	inst.State.Installing = false
	if err := saveNative(inst); err != nil {
		return err
	}
	client := in.app.adminClient(inst, nil)
	if done, err := setup.Done(ctx, client); err == nil && done {
		inst.State.SetupDone = true
		if err := saveNative(inst); err != nil {
			return err
		}
		u.Finish(origin)
		return nil
	}
	if in.f.SetupWhere != "terminal" {
		u.Info("Your instance is ready at %s.", origin)
		u.Info("Open it and enter this setup token to create your admin account.")
		u.Highlight(token)
		return nil
	}
	adminID, err := runSetup(ctx, in.app, in.c, inst, nil, &in.f.Setup, in.answers)
	if err != nil {
		return err
	}
	u.Finish(origin)
	signInAfterSetup(ctx, in.app, inst, nil, adminID)
	return nil
}

func refuseDockerOnly(changes map[string]*string) error {
	var names []string
	for _, name := range slices.Sorted(maps.Keys(changes)) {
		if changes[name] != nil && settings.DockerOnly(name) {
			names = append(names, name)
		}
	}
	switch len(names) {
	case 0:
		return nil
	case 1:
		return ui.Refused(fmt.Sprintf("%s only changes Docker containers, and this instance runs natively without them.", names[0]), "Leave it out. Fluxer without Docker has no setting for it.")
	}
	return ui.Refused(fmt.Sprintf("%s only change Docker containers, and this instance runs natively without them.", ui.JoinAnd(names)), "Leave them out. Fluxer without Docker has no setting for them.")
}

func applyNativeSettings(ctx context.Context, app *App, inst *instance.Instance, changes map[string]*string) (*deploy.Change, error) {
	if err := refuseDockerOnly(changes); err != nil {
		return nil, err
	}
	if err := nativeRoot(app.command); err != nil {
		return nil, err
	}
	if err := deploy.Changeable(inst); err != nil {
		return nil, err
	}
	pr, err := deploy.Prepare(inst, changes)
	if err != nil {
		return nil, err
	}
	if pr.Unchanged {
		return &deploy.Change{Keys: pr.Keys, Unchanged: true}, nil
	}
	st := inst.State
	oldSettings, oldSecrets := st.Settings, st.Secrets
	changed := &deploy.Change{Keys: pr.Preview.Keys}
	if st.Upgrade != nil || st.Stopped {
		st.Settings, st.Secrets = pr.Settings, pr.Secrets
		return changed, saveNative(inst)
	}
	if err := nativeAlone(ctx); err != nil {
		return nil, err
	}
	closePorts, err := openPorts(ctx, app, inst, changes)
	if err != nil {
		return nil, err
	}
	preview := pr.Preview
	preview.Restarts = slices.Clone(stack.Services)
	ok, err := confirmPreview(app, preview)
	if err != nil {
		return nil, err
	}
	if !ok {
		return nil, &ui.ExitError{Code: ui.ExitRefused, Msg: "rexctl left your settings as they were.", Err: deploy.ErrDeclined}
	}
	n := native.New(inst)
	st.Settings, st.Secrets = pr.Settings, pr.Secrets
	err = startNativeStep(ctx, app, inst, n, pr.Set, "Applying the new settings", "Restarting", "Restarted Fluxer with the new settings.")
	if err == nil {
		changed.Applied = true
		closePorts()
		return changed, nil
	}
	st.Settings, st.Secrets = oldSettings, oldSecrets
	if rerr := startNativeStep(ctx, app, inst, n, pr.Set, "Putting the old settings back", "Restarting", "Fluxer runs with the old settings again."); rerr != nil {
		return nil, ui.Wrap(errors.Join(err, rerr), "Fluxer didn't start with the new settings, and rexctl couldn't start it with the old ones either.", fmt.Sprintf("Run rexctl logs, or journalctl -u %s, to see why.", n.Unit()))
	}
	return nil, ui.Wrap(err, "Fluxer didn't start with the new settings. rexctl put the old ones back.", "Run rexctl logs to see why.")
}
