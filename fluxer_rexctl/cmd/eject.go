// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"os"
	"runtime"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const podmanRestartsContainers = "5.8.2"

func newEjectCmd(app *App) *cobra.Command {
	return &cobra.Command{
		Use:   "eject",
		Short: "Manage this instance yourself with docker compose",
		Long:  "Writes every setting into the instance folder for docker compose and stops rexctl from changing the instance. Fluxer keeps running. Run rexctl takeover to hand it back.",
		Args:  noArgs,
		RunE: func(c *cobra.Command, args []string) error {
			return runEject(c.Context(), app)
		},
	}
}

func runEject(ctx context.Context, app *App) error {
	u := app.UI
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	return app.Locked(ctx, inst, func() error {
		if err := ejectRefusal(instance.EjectCheck(inst)); err != nil {
			return err
		}
		e, engineErr := app.Engine(ctx, inst)
		tool := composeTool(inst, e)
		if ok, err := confirmEject(u, inst, tool); err != nil || !ok {
			return err
		}
		removed := false
		err = instance.Eject(inst, instance.EjectOptions{
			Rexctl: app.Version,
			SendAdminImport: func() error {
				if engineErr == nil {
					sendAdminImport(ctx, app, inst, e)
				}
				return nil
			},
			RemoveBootUnit: func() error {
				removed = true
				if _, err := removeAutoUpgrade(ctx, inst, e); err != nil {
					return fmt.Errorf("removing automatic upgrades: %w", err)
				}
				return removeBootUnit(ctx, inst, e)
			},
		})
		if err != nil {
			if refusal := ejectRefusal(err); refusal != err {
				return refusal
			}
			return ui.Wrap(err, "Couldn't hand the instance over.", "Check that you can write to "+inst.Dir+", then try again.")
		}
		app.forget(inst.Dir)
		if removed && host.BootUnitSupported() && engineErr == nil && e.Kind == engine.Podman && olderPodman(e.Version) {
			u.Info("Your containers come back after a restart of this machine only once Podman's own podman-restart.service starts them.")
		}
		u.Info("If %s recreates Fluxer's containers the first time, that's expected. Your data stays.", tool)
		u.Info("To upgrade Fluxer later, run rexctl takeover, rexctl upgrade and rexctl eject one after the other.")
		u.OK("You manage this instance with %s in %s from here on. Run rexctl takeover to hand it back to rexctl.", tool, inst.Dir)
		return nil
	})
}

func confirmEject(u *ui.UI, inst *instance.Instance, tool string) (bool, error) {
	u.Info("You probably don't need to eject. rexctl keeps these changes when it upgrades Fluxer.")
	u.Info("  rexctl config set changes settings, including the FLUXER_* variables, and rexctl config list shows them.")
	u.Info("  rexctl config edit changes %s for labels, images, ports and volumes.", instance.CustomCompose)
	u.Info("  A .caddy file in %s adds your own Caddy directives.", instance.CaddyDropInDir)
	u.Info("  See %s for more.", changeHowFluxerRunsURL)
	u.Info("If you eject, rexctl writes every setting for %s into %s and stops managing the instance. You start, stop and change Fluxer with %s yourself, and rexctl turns off automatic upgrades. To upgrade later, you run rexctl takeover, rexctl upgrade and rexctl eject one after the other. Fluxer keeps running.", tool, inst.Dir, tool)
	if !u.Interactive() && !u.Yes() {
		return false, &ui.ExitError{Code: ui.ExitUsage, Msg: "rexctl eject asks before it hands the instance over.", Fix: "Pass --yes to eject without the question."}
	}
	ok, err := u.Confirm("Manage this instance yourself?", false)
	if err != nil {
		return false, err
	}
	if !ok {
		return false, ui.Refused("Your instance was left as it was.", "")
	}
	return true, nil
}

func sendAdminImport(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine) {
	client := app.adminClient(inst, e.Named(inst.Project(), inst.Dir))
	client.OnSkew = nil
	if err := deploy.SendAdminImport(ctx, inst, client); err != nil {
		app.UI.Detail("sending settings to Fluxer: %v", err)
	}
}

func ejectRefusal(err error) error {
	var ej *instance.EjectedError
	if errors.As(err, &ej) {
		return ui.Refused(ej.Error(), ej.Fix())
	}
	var ae *instance.AdoptError
	if errors.As(err, &ae) {
		return ui.Refused(ae.Msg, ae.Fix)
	}
	return err
}

func composeTool(inst *instance.Instance, e *engine.Engine) string {
	if e != nil {
		return instance.ComposeTool(string(e.Kind))
	}
	return instance.ComposeTool(inst.State.Engine.Kind)
}

func olderPodman(version string) bool {
	return version != "" && instance.VersionBelow(version, podmanRestartsContainers)
}

func removeBootUnit(ctx context.Context, inst *instance.Instance, e *engine.Engine) error {
	if !host.BootUnitSupported() {
		return nil
	}
	u := host.BootUnit{Project: inst.Project(), Dir: inst.Dir, Rootless: runtime.GOOS == "linux" && os.Geteuid() != 0}
	if e != nil {
		u.Podman, u.Rootless = e.Kind == engine.Podman, e.Rootless
	}
	if err := host.RemoveBootUnit(ctx, u); err != nil {
		return fmt.Errorf("removing the boot unit: %w", err)
	}
	return nil
}
