// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/setup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type takeoverOptions struct {
	DryRun bool
	Then   string
}

func newTakeoverCmd(app *App) *cobra.Command {
	var dryRun bool
	c := &cobra.Command{
		Use:   "takeover",
		Short: "Let rexctl manage an instance it doesn't manage yet",
		Long:  "Hands an instance made by the old installer, by hand with docker compose, or released with rexctl eject over to rexctl. Fluxer keeps running as it is.",
		Args:  noArgs,
		RunE: func(c *cobra.Command, args []string) error {
			dir, err := app.Dir()
			if err != nil {
				return err
			}
			inst, _, err := takeoverDir(c.Context(), app, dir, takeoverOptions{DryRun: dryRun})
			if err != nil || inst == nil {
				return err
			}
			app.UI.OK("rexctl manages your instance in %s.", inst.Dir)
			return nil
		},
	}
	c.Flags().BoolVar(&dryRun, "dry-run", false, "Show what would change without changing anything")
	return c
}

func isBootstrap(path string) bool {
	data, err := os.ReadFile(path)
	return err == nil && strings.Contains(string(data), "REXCTL_RELEASE_URL")
}

func takeoverDir(ctx context.Context, app *App, dir string, opts takeoverOptions) (*instance.Instance, []string, error) {
	u := app.UI
	dir, err := filepath.Abs(dir)
	if err != nil {
		return nil, nil, err
	}
	var current *instance.Instance
	switch found, err := instance.Find(dir); {
	case err == nil:
		current = found
	case errors.Is(err, instance.ErrNotFound):
		return nil, nil, ui.Refused(fmt.Sprintf("There's no Fluxer instance in %s.", dir), "Run rexctl install, or pass --dir.")
	case !errors.Is(err, instance.ErrLegacy):
		return nil, nil, ui.Wrap(err, fmt.Sprintf("rexctl can't read the instance in %s.", dir), "Run rexctl as the account that installed Fluxer.")
	}
	e, err := app.Engine(ctx, current)
	if err != nil && !opts.DryRun {
		return nil, nil, err
	}
	report, err := instance.PreviewImport(dir, e)
	if err != nil {
		return nil, nil, takeoverError(err)
	}
	if report.Source == instance.SourceManaged {
		u.Info("rexctl already manages the instance in %s.", dir)
		return current, nil, nil
	}
	describeTakeover(u, report)
	if opts.DryRun {
		if opts.Then != "" && len(report.KeepAsMine) == 0 {
			u.Info("%s", opts.Then)
			if e != nil {
				legacy := &instance.Instance{Dir: dir, State: &instance.State{Project: report.Project, Images: instance.Images{Refs: report.Images}}}
				printSpace(ctx, app, legacy, e, embeddedRefs(), &upgradeFlags{}, release.Short(stack.Embedded().Release))
			}
		}
		u.Info("Dry run. Nothing changed.")
		return nil, nil, nil
	}
	ok, err := u.Confirm("Take over this instance?", true)
	if err != nil {
		return nil, nil, err
	}
	if !ok {
		return nil, nil, ui.Refused("Your instance was left as it was.", "Run rexctl takeover when you're ready.")
	}
	lock, err := (&instance.Instance{Dir: dir}).Lock(ctx)
	if errors.Is(err, instance.ErrLocked) {
		return nil, nil, ui.Refused("Another rexctl command is changing this instance.", "Wait for it to finish, then try again.")
	}
	if err != nil {
		return nil, nil, err
	}
	defer lock.Release()
	var inst *instance.Instance
	err = u.Step(ctx, "Taking over your instance", func(ctx context.Context, s *ui.Step) error {
		state, done, err := instance.Import(dir, e)
		if err != nil {
			return takeoverError(err)
		}
		report = done
		inst = &instance.Instance{Dir: dir, State: state}
		s.Done("Took over your instance.")
		return nil
	})
	if err != nil {
		return nil, nil, err
	}
	installTakeoverBootUnit(ctx, app, inst, e)
	app.remember(inst.Dir)
	if e != nil {
		sendAdminImport(ctx, app, inst, e)
	}
	if refreshSetupDone(ctx, app, inst, e) {
		if err := inst.Save(); err != nil {
			return nil, nil, err
		}
	}
	for _, name := range report.KeepAsMine {
		warnKeptCopy(u, name, report.Mine[name], inst.State.Release == instance.LegacyRelease)
	}
	if line := report.DroppedLine(); line != "" && report.Source != instance.SourceEjected {
		u.Info("%s", line)
	}
	return inst, report.KeepAsMine, nil
}

func describeTakeover(u *ui.UI, r *instance.ImportReport) {
	if r.Source == instance.SourceEjected {
		u.Info("You manage the instance in %s yourself. rexctl takes it back with the settings in its .env file.", r.Dir)
	} else {
		u.Info("Found a Fluxer instance in %s.", r.Dir)
	}
	if r.Release != "" {
		u.Info("Your instance now runs Fluxer %s. rexctl takes it over on that release.", release.Short(r.Release))
	}
	u.Info("After this, rexctl manages it. Fluxer keeps running as it is, and your data and secrets stay.")
	for _, f := range r.CustomFiles {
		if f != instance.CustomCompose {
			u.Info("Your own compose changes in %s will move to %s.", f, instance.CustomCompose)
		}
	}
	for _, name := range r.KeepAsMine {
		u.Info("Your copy of %s has changes. It keeps running, and a copy is saved as %s.", name, keptName(r, name))
	}
	if n := len(r.Minted); n == 1 {
		u.Info("One setting your instance is missing will be added.")
	} else if n > 1 {
		u.Info("%d settings your instance is missing will be added.", n)
	}
	if r.DedupedEnv {
		u.Info("Settings that appear twice in .env keep their last value.")
	}
	if len(r.Placeholders) > 0 {
		u.Info("These settings still have the example value from the old installer: %s. rexctl keeps them. Run rexctl config list to check them.", ui.JoinAnd(r.Placeholders))
	}
	if n := len(r.AdminKeys); n > 0 {
		u.Info("%s move into Fluxer's own settings, where rexctl config set changes them.", countWord(n, "One email, branding or billing setting", "email, branding and billing settings"))
	}
	if len(r.Changed) > 0 {
		u.Info("rexctl keeps the changes you made: %s.", ui.JoinAnd(r.Changed))
	}
	if r.PodmanCompose {
		u.Info("The first time rexctl starts or changes Fluxer, it replaces the containers podman-compose made. Fluxer restarts once and your data stays.")
	}
	for _, name := range r.DeleteFiles {
		if (name == "install.sh" || name == "install.ps1") && !isBootstrap(filepath.Join(r.Dir, name)) {
			u.Info("The old installer will be removed.")
			break
		}
	}
	if r.EngineProxies {
		u.Detail("proxy settings from the engine's config.json")
	}
	for _, f := range r.CustomFiles {
		u.Detail("custom file %s", f)
	}
	for _, f := range r.DeleteFiles {
		u.Detail("delete %s", f)
	}
	for _, k := range r.Minted {
		u.Detail("mint %s", k)
	}
	for _, k := range r.Defaults {
		u.Detail("default %s", k)
	}
	for _, k := range r.AdminKeys {
		u.Detail("admin %s", k)
	}
}

func keptName(r *instance.ImportReport, name string) string {
	if kept := r.Mine[name]; kept != "" {
		return kept
	}
	return name + ".mine"
}

func countWord(n int, one, many string) string {
	if n == 1 {
		return one
	}
	return fmt.Sprintf("%d %s", n, many)
}

func warnKeptCopy(u *ui.UI, name, kept string, legacy bool) {
	when := "before you run rexctl upgrade. The upgrade replaces"
	if !legacy {
		when = "now. The next time rexctl restarts or changes Fluxer, it replaces"
	}
	switch {
	case name == stack.Caddyfile:
		u.Warn(fmt.Sprintf("Your Caddyfile changes are also saved in %s.", kept), fmt.Sprintf("Move them into a file in %s %s the Caddyfile.", instance.CaddyDropInDir, when))
	case isComposeName(name):
		u.Warn(fmt.Sprintf("Your changes to %s are also saved in %s.", name, kept), fmt.Sprintf("Move them into %s %s %s.", instance.CustomCompose, when, name))
	}
}

type keptCopy struct {
	Name string
	Copy string
}

func keptCopies(inst *instance.Instance) []keptCopy {
	if inst == nil || inst.State == nil || inst.State.Release != instance.LegacyRelease {
		return nil
	}
	entries, err := os.ReadDir(inst.Dir)
	if err != nil {
		return nil
	}
	var out []keptCopy
	for _, ent := range entries {
		if ent.IsDir() {
			continue
		}
		m := keptPattern.FindStringSubmatch(ent.Name())
		if m == nil || (m[1] != stack.Caddyfile && !isComposeName(m[1])) {
			continue
		}
		out = append(out, keptCopy{Name: m[1], Copy: ent.Name()})
	}
	return out
}

var keptPattern = regexp.MustCompile(`^(.+)\.mine(?:\.[0-9]+)?$`)

func confirmKeptCopies(app *App, inst *instance.Instance) (bool, error) {
	kept := keptCopies(inst)
	if len(kept) == 0 {
		return true, nil
	}
	names := make([]string, 0, len(kept))
	for _, k := range kept {
		warnKeptCopy(app.UI, k.Name, k.Copy, true)
		names = append(names, k.Copy)
	}
	msg := fmt.Sprintf("Your changes in %s stop working after the upgrade.", ui.JoinAnd(names))
	if !app.UI.Interactive() || app.UI.Yes() {
		return false, ui.Refused(msg, "Move them, then delete the copies and run rexctl upgrade again.")
	}
	app.UI.Info("%s", msg)
	return app.UI.Confirm("Upgrade anyway?", false)
}

func isComposeName(name string) bool {
	ext := filepath.Ext(name)
	return name != instance.CustomCompose && (ext == ".yml" || ext == ".yaml")
}

func takeoverError(err error) error {
	var ae *instance.AdoptError
	if errors.As(err, &ae) {
		return ui.Refused(ae.Msg, ae.Fix)
	}
	var ej *instance.EjectedError
	if errors.As(err, &ej) {
		return ui.Refused(ej.Error(), ej.Fix())
	}
	return err
}

func installTakeoverBootUnit(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine) {
	if e == nil || !host.BootUnitSupported() {
		return
	}
	if err := installBootUnit(ctx, inst, e, nil); err != nil {
		app.UI.Detail("%v", err)
		app.UI.Warn("Fluxer won't start by itself when this machine restarts.", "Run rexctl doctor --fix to set that up.")
	}
}

func refreshSetupDone(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine) bool {
	if e == nil {
		return false
	}
	client := app.adminClient(inst, e.Named(inst.Project(), inst.Dir))
	client.OnSkew = nil
	done, err := setup.Done(ctx, client)
	if err != nil || done == inst.State.SetupDone {
		return false
	}
	inst.State.SetupDone = done
	return true
}
