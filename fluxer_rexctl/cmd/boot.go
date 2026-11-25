// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"strings"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const bootServiceQuestion = "Start Fluxer when this machine starts?"

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
	switch {
	case flag == instance.BootServiceOff || flag == "" && want == checks.BootOff:
		return saveBootService(inst, instance.BootServiceOff)
	case flag == instance.BootServiceOn || app.UI.Yes():
	case !app.UI.Interactive() && want == checks.BootOn:
		app.UI.Warn("Fluxer won't start by itself when this machine restarts.", "Run rexctl boot on when you want it to.")
		return nil
	case !app.UI.Interactive():
		return saveBootService(inst, instance.BootServiceOff)
	default:
		on, err := app.UI.Confirm(bootServiceQuestion, true)
		if err != nil {
			return err
		}
		if !on {
			return saveBootService(inst, instance.BootServiceOff)
		}
	}
	steps, err := u.InstallSteps(ctx)
	if err != nil {
		return err
	}
	for _, st := range steps {
		app.UI.Detail("%s", st.Do)
		if st.Command != "" {
			app.UI.Detail("  %s", st.Command)
		}
	}
	if err := host.RunSteps(ctx, steps); err != nil {
		return err
	}
	return saveBootService(inst, instance.BootServiceOn)
}

func saveBootService(inst *instance.Instance, v string) error {
	if inst.State.BootService == v {
		return nil
	}
	inst.State.BootService = v
	return saveInstance(inst)
}

func newBootCmd(app *App) *cobra.Command {
	c := &cobra.Command{
		Use:     "boot",
		Short:   "Show whether a service starts Fluxer with this machine",
		Long:    "Show whether a systemd service starts Fluxer when this machine starts. Docker already starts Fluxer again after a restart, and the service also starts its services in order and stops them cleanly. Podman has no daemon, so it needs the service. A systemd install always runs as a service, and this shows whether it starts with the machine.",
		Example: "rexctl boot\nrexctl boot on\nrexctl boot off",
		Args:    noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runBootShow(c.Context(), app)
		},
	}
	on := &cobra.Command{
		Use:     "on",
		Short:   "Start Fluxer with this machine through a systemd service",
		Long:    "Write a systemd service that starts Fluxer when this machine starts and stops it cleanly when it shuts down. On a root install it also copies rexctl to " + host.LibexecBinary + " for the service to run. On a systemd install it turns on the service that Fluxer already runs as. rexctl lists every change and asks before it makes them, unless you pass --yes.",
		Example: "rexctl boot on\nrexctl boot on --yes",
		Args:    noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runBootSet(c.Context(), app, instance.BootServiceOn)
		},
	}
	off := &cobra.Command{
		Use:     "off",
		Short:   "Remove the service that starts Fluxer with this machine",
		Long:    "Remove the systemd service that starts Fluxer with this machine, and the copy of rexctl in " + host.LibexecBinary + " if nothing else uses it. With Docker, Docker still starts Fluxer again after a restart. On a systemd install Fluxer keeps running, but no longer starts with the machine. rexctl lists every change and asks first, unless you pass --yes.",
		Example: "rexctl boot off\nrexctl boot off --yes",
		Args:    noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runBootSet(c.Context(), app, instance.BootServiceOff)
		},
	}
	c.AddCommand(on, off)
	return c
}

func runBootShow(ctx context.Context, app *App) error {
	_, r, ok, err := app.instanceRuntime(ctx, false)
	if err != nil || !ok {
		return err
	}
	st, err := r.Boot().State(ctx)
	if err != nil {
		return err
	}
	if st.Enabled {
		app.UI.OK("Fluxer starts with this machine through %s.", st.Unit)
		return nil
	}
	app.UI.Warn(strings.TrimSpace("No service starts Fluxer with this machine. "+st.Detail), "Run rexctl boot on.")
	return nil
}

func runBootSet(ctx context.Context, app *App, v string) error {
	inst, r, ok, err := app.instanceRuntime(ctx, true)
	if err != nil || !ok {
		return err
	}
	on := v == instance.BootServiceOn
	return app.Locked(ctx, inst, func() error {
		steps, err := rt.BootSteps(ctx, bare(r), on)
		if err != nil {
			return err
		}
		if len(steps) == 0 {
			if err := saveBootService(inst, v); err != nil {
				return err
			}
			if on {
				app.UI.OK("Fluxer already starts with this machine.")
				return nil
			}
			app.UI.Info("No service starts Fluxer with this machine. Nothing changed.")
			return nil
		}
		question := "Stop Fluxer starting with this machine?"
		if on {
			question = "Start Fluxer with this machine?"
		}
		ok, err := confirmHostSteps(app, steps, question)
		if err != nil {
			return err
		}
		if !ok && !app.UI.Interactive() {
			return &ui.ExitError{Code: ui.ExitUsage, Msg: question + " rexctl can't ask that here.", Fix: "Pass --yes to answer yes."}
		}
		if !ok {
			app.UI.Info("Nothing changed.")
			return nil
		}
		if on {
			err = r.Boot().Enable(ctx, nil)
		} else {
			err = r.Boot().Disable(ctx, nil)
		}
		if err != nil {
			return ui.Wrap(err, "Couldn't change the service that starts Fluxer with this machine.", "Run rexctl boot "+v+" again.")
		}
		if err := saveBootService(inst, v); err != nil {
			return err
		}
		if on {
			app.UI.OK("Fluxer starts with this machine.")
			return nil
		}
		app.UI.OK("Fluxer no longer starts with this machine through a service.")
		if after, err := r.Boot().State(ctx); err == nil && after.Detail != "" {
			app.UI.Info("%s", after.Detail)
		}
		return nil
	})
}
