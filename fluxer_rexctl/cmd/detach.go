// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func newDetachCmd(app *App) *cobra.Command {
	return &cobra.Command{
		Use:     "detach",
		Short:   "Run Fluxer with docker compose and stop managing it",
		Long:    "Writes compose.yaml and fluxer.env into the instance folder, starts Fluxer from them with docker compose and stops managing the instance. For an instance on the default ports, the files are the ones each Fluxer release publishes with the project name added. Your data stays in the same volume. Automatic upgrades stop, and a backup schedule moves into Fluxer, which keeps the backups in its volume.",
		Example: "rexctl detach --yes\nrexctl detach --dir /opt/fluxer2",
		Args:    noArgs,
		RunE: func(c *cobra.Command, args []string) error {
			return runDetach(c.Context(), app)
		},
	}
}

func runDetach(ctx context.Context, app *App) error {
	u := app.UI
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	if err := managed(inst); err != nil {
		return err
	}
	switch rt.KindOf(inst) {
	case rt.Compose:
		return ui.Refused("This instance runs on the older Docker Compose layout.", legacyMoveFix)
	case rt.Native:
		return ui.Refused("This instance runs natively without containers, so there is no container to hand over.", "Keep managing it with rexctl.")
	}
	switch {
	case inst.State.Upgrade != nil:
		return ui.Refused("An upgrade is still in progress.", "Run rexctl upgrade to finish it, then run rexctl detach again.")
	case inst.State.Pending != nil:
		return ui.Refused("A settings change is still in progress.", "Run rexctl doctor to finish it, then run rexctl detach again.")
	}
	if err := handOverRefusal(inst, false); err != nil {
		return err
	}
	tool := instance.ComposeTool(inst.State.Engine.Kind)
	ok, err := u.ConfirmWith(ui.Question{
		Text:   "Hand Fluxer over to " + tool + "?",
		Detail: "rexctl writes compose.yaml and fluxer.env to " + inst.Dir + ", starts Fluxer from them and then leaves this instance alone. Automatic upgrades stop, and scheduled backups move into Fluxer, which keeps them in its volume. Upgrades and settings changes are yours to run from then on.",
		Flag:   "yes",
	})
	if err != nil {
		return err
	}
	if !ok {
		u.Info("Nothing changed.")
		return nil
	}
	var res handedOver
	err = app.Locked(ctx, inst, func() error {
		var err error
		res, err = handOver(ctx, app, inst, handOverOptions{})
		return err
	})
	if err != nil {
		return err
	}
	moving := inst.State.MovedFrom != nil
	if !moving {
		stepAway(app, inst)
	}
	body := []string{"Your settings are in " + res.out.EnvFile + " and rexctl leaves this instance alone from now on. Automatic upgrades are off."}
	if res.schedule != host.ScheduleOff {
		body = append(body, "Fluxer backs itself up "+res.schedule+" into its volume.")
	}
	body = append(body, fmt.Sprintf("Backups from now on stay in Fluxer's volume. Older ones stay in %s.", inst.BackupDir()))
	body = append(body, hostCopyLines(inst, tool)...)
	commands := []string{"cd " + shellWord(inst.Dir), tool + " ps", tool + " logs"}
	if moving {
		body = append(body, fmt.Sprintf("The old containers in %s are stopped and still have their data. Don't start them while you try the new one.", inst.State.MovedFrom.Dir))
		commands = append(commands, "rexctl migrate --clean-up --dir "+shellWord(inst.State.MovedFrom.Dir))
	}
	u.Card(ui.Card{Title: "Fluxer runs from " + tool, Body: body, Commands: commands})
	return nil
}

func hostCopyLines(inst *instance.Instance, tool string) []string {
	var out []string
	for _, c := range rt.HostCopies(inst) {
		out = append(out, fmt.Sprintf("rexctl copied %s into Fluxer's volume and no longer copies it from %s. Replace it with %s cp.", ui.JoinAnd(c.Names), c.Dir, tool))
	}
	return out
}
