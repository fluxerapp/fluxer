// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const bootServiceQuestion = "Add a systemd service that starts Fluxer with this machine? Docker already starts Fluxer again after a restart. The service also starts its services in order and stops them cleanly."

type hostStepsAnswer int

const (
	hostStepsRun hostStepsAnswer = iota
	hostStepsAsk
	hostStepsSkip
)

func hostStepsMode(steps int, interactive, yes bool) hostStepsAnswer {
	switch {
	case steps == 0, yes:
		return hostStepsRun
	case interactive:
		return hostStepsAsk
	}
	return hostStepsSkip
}

func confirmHostSteps(app *App, steps []host.Step, question string) (bool, error) {
	mode := hostStepsMode(len(steps), app.UI.Interactive(), app.UI.Yes())
	if mode == hostStepsRun {
		return true, nil
	}
	app.UI.Info("rexctl needs to change these files and settings on this machine:")
	for _, s := range steps {
		app.UI.Info("  %s", s.Do)
		if s.Command != "" {
			app.UI.Info("    %s", s.Command)
		}
	}
	if mode == hostStepsSkip {
		return false, nil
	}
	return app.UI.Confirm(question, true)
}

func parseBootServiceFlag(v string) (string, error) {
	switch v {
	case "", instance.BootServiceOn, instance.BootServiceOff:
		return v, nil
	}
	return "", ui.Usage("Pass --boot-service as on or off.")
}

func decideBootService(want checks.BootWant, flag string, interactive, yes bool, ask func() (bool, error)) (bool, error) {
	switch {
	case flag != "":
		return flag == instance.BootServiceOn, nil
	case want != checks.BootAsk:
		return want == checks.BootOn, nil
	case !interactive || yes:
		return false, nil
	}
	return ask()
}

func setupBootService(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine, p *engine.Project, flag string) error {
	if e == nil || !host.BootUnitSupported() {
		return nil
	}
	u, err := checks.BootUnitFor(inst, e, p)
	if err != nil {
		return err
	}
	s, err := host.CheckBootUnit(ctx, u)
	if err != nil {
		return err
	}
	want := checks.BootServiceWant(inst.State.BootService, e.Kind == engine.Podman, !s.Missing)
	on, err := decideBootService(want, flag, app.UI.Interactive(), app.UI.Yes(), func() (bool, error) {
		return app.UI.Confirm(bootServiceQuestion, false)
	})
	if err != nil {
		return err
	}
	if !on {
		return saveBootService(inst, instance.BootServiceOff)
	}
	if err := saveBootService(inst, instance.BootServiceOn); err != nil {
		return err
	}
	return installBootService(ctx, app, u)
}

func installBootService(ctx context.Context, app *App, u host.BootUnit) error {
	steps, err := u.InstallSteps(ctx)
	if err != nil {
		return err
	}
	ok, err := confirmHostSteps(app, steps, "Set up the service that starts Fluxer with this machine?")
	if err != nil {
		return err
	}
	if !ok {
		app.UI.Warn("Fluxer won't start by itself when this machine restarts.", "Run rexctl boot on when you want it to.")
		return nil
	}
	return host.RunSteps(ctx, steps)
}

func saveBootService(inst *instance.Instance, v string) error {
	if inst.State.BootService == v {
		return nil
	}
	inst.State.BootService = v
	if err := inst.Save(); err != nil {
		return ui.Wrap(err, "Couldn't save your instance's state.", "Check that you can write to "+inst.Dir+", then try again.")
	}
	return nil
}

func newBootCmd(app *App) *cobra.Command {
	c := &cobra.Command{
		Use:   "boot",
		Short: "Show whether a service starts Fluxer with this machine",
		Long:  "Show whether a systemd service starts Fluxer when this machine starts. With Docker, Docker already starts Fluxer again after a restart, and the service also starts its services in order and stops them cleanly. Podman has no daemon to do that, so it needs the service.",
		Args:  noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runBootShow(c.Context(), app)
		},
	}
	on := &cobra.Command{
		Use:   "on",
		Short: "Start Fluxer with this machine through a systemd service",
		Long:  "Write a systemd service that starts Fluxer when this machine starts and stops it cleanly when it shuts down. On a root install it also copies rexctl to " + host.LibexecBinary + " for the service to run. rexctl lists every change and asks first, unless you pass --yes.",
		Args:  noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runBootSet(c.Context(), app, instance.BootServiceOn)
		},
	}
	off := &cobra.Command{
		Use:   "off",
		Short: "Remove the service that starts Fluxer with this machine",
		Long:  "Remove the systemd service that starts Fluxer with this machine, and the copy of rexctl in " + host.LibexecBinary + " when nothing else uses it. With Docker, Docker still starts Fluxer again after a restart. rexctl lists every change and asks first, unless you pass --yes.",
		Args:  noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runBootSet(c.Context(), app, instance.BootServiceOff)
		},
	}
	c.AddCommand(on, off)
	return c
}

func bootUnitFor(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine) (host.BootUnit, host.BootUnitState, error) {
	var p *engine.Project
	if loaded, err := deploy.Load(ctx, inst, e); err == nil {
		p = loaded
	}
	u, err := checks.BootUnitFor(inst, e, p)
	if err != nil {
		return host.BootUnit{}, host.BootUnitState{}, err
	}
	s, err := host.CheckBootUnit(ctx, u)
	return u, s, err
}

func runBootShow(ctx context.Context, app *App) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	e, err := app.Engine(ctx, inst)
	if err != nil {
		return err
	}
	if !host.BootUnitSupported() {
		return ui.Refused("This machine doesn't run systemd, so rexctl can't add a service that starts Fluxer with it.", "")
	}
	_, s, err := bootUnitFor(ctx, app, inst, e)
	if err != nil {
		return err
	}
	if !s.Missing {
		app.UI.OK("A service starts Fluxer with this machine (%s).", s.Path)
		return nil
	}
	if e.Kind == engine.Podman {
		app.UI.Warn("No service starts Fluxer with this machine, and Podman doesn't start it by itself.", "Run rexctl boot on.")
		return nil
	}
	app.UI.OK("Docker starts Fluxer again after this machine restarts. Run rexctl boot on to also start its services in order and stop them cleanly.")
	return nil
}

func runBootSet(ctx context.Context, app *App, v string) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	e, err := app.Engine(ctx, inst)
	if err != nil {
		return err
	}
	if !host.BootUnitSupported() {
		return ui.Refused("This machine doesn't run systemd, so rexctl can't add a service that starts Fluxer with it.", "")
	}
	return app.Locked(ctx, inst, func() error {
		u, _, err := bootUnitFor(ctx, app, inst, e)
		if err != nil {
			return err
		}
		if v == instance.BootServiceOn {
			if err := saveBootService(inst, v); err != nil {
				return err
			}
			if err := installBootService(ctx, app, u); err != nil {
				return ui.Wrap(err, "Couldn't set up the service that starts Fluxer with this machine.", "Run rexctl boot on again.")
			}
			app.UI.OK("Fluxer starts with this machine.")
			return nil
		}
		steps, err := u.RemoveSteps(ctx)
		if err != nil {
			return err
		}
		steps = append(steps, host.LibexecRemoveSteps(ctx, u)...)
		ok, err := confirmHostSteps(app, steps, "Remove the service that starts Fluxer with this machine?")
		if err != nil {
			return err
		}
		if !ok {
			return ui.Refused("Stopped without changing anything.", "Run rexctl boot off --yes to remove it without asking.")
		}
		if err := host.RunSteps(ctx, steps); err != nil {
			return ui.Wrap(err, "Couldn't remove the service that starts Fluxer with this machine.", "Run rexctl boot off again.")
		}
		if err := saveBootService(inst, v); err != nil {
			return err
		}
		if e.Kind == engine.Podman {
			app.UI.OK("Removed the service. Run rexctl start after this machine restarts, because Podman doesn't start Fluxer by itself.")
			return nil
		}
		app.UI.OK("Removed the service. Docker still starts Fluxer again after this machine restarts.")
		return nil
	})
}
