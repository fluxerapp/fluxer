// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"

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
		Long:    "Writes compose.yaml and fluxer.env into the instance folder in the same form as the files each Fluxer release publishes, starts Fluxer from them and stops rexctl from changing the instance. Your data stays in the same volume. Automatic upgrades stop, and a backup schedule moves into Fluxer, which keeps the backups in its volume.",
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
	case inst.State.MovedFrom != nil:
		return ui.Refused("The old containers from before the move to this layout are still on this machine, and they share this instance's compose project name.", "Run "+migrateCommand+" --clean-up first, then run rexctl detach again.")
	case inst.State.Upgrade != nil:
		return ui.Refused("An upgrade is still in progress.", "Run rexctl upgrade to finish it, then run rexctl detach again.")
	case inst.State.Pending != nil:
		return ui.Refused("A settings change is still in progress.", "Run rexctl doctor to finish it, then run rexctl detach again.")
	}
	r, err := app.Runtime(ctx, inst)
	if err != nil {
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
	return app.Locked(ctx, inst, func() error {
		sched, schedErr := backupScheduleFor(r, inst)
		o := rt.DetachOptions{Rexctl: app.Version, Schedule: host.ScheduleOff, Keep: inst.KeepBackups()}
		if schedErr == nil {
			o.Schedule = sched.Current()
		}
		if o.Schedule == host.ScheduleOff {
			o.Schedule = fluxerdSchedule(inst)
		}
		var out rt.Detached
		err := u.Step(ctx, "Handing Fluxer over to "+tool, func(ctx context.Context, s *ui.Step) error {
			var err error
			out, err = rt.Detach(ctx, r, o)
			switch {
			case errors.Is(err, rt.ErrMultiline):
				return ui.Refused("A setting of this instance holds more than one line, which an env file can't hold.", "Change that setting with rexctl config set, then run rexctl detach again.")
			case errors.Is(err, rt.ErrUnquotable):
				return ui.Refused("A setting of this instance ends in a backslash before a quote, which an env file can't hold.", "Change that setting with rexctl config set, then run rexctl detach again.")
			case errors.Is(err, rt.ErrDetachStartedAgain):
				u.Detail("%v", err)
				return rt.ContainerError(err, "Couldn't hand Fluxer over to "+tool+", so rexctl started its own container again.")
			case errors.Is(err, rt.ErrDetachLeftStopped):
				u.Detail("%v", err)
				return rt.ContainerError(err, "Couldn't hand Fluxer over to "+tool+", and couldn't start rexctl's own container again. Run rexctl start.")
			case err != nil:
				u.Detail("%v", err)
				return rt.ContainerError(err, "Couldn't hand Fluxer over to "+tool+". Nothing changed, and Fluxer keeps running.")
			}
			s.Done("Fluxer runs from %s.", out.ComposeFile)
			return nil
		})
		if err != nil {
			return err
		}
		if e, err := app.anyEngine(ctx, inst); err == nil {
			if _, err := removeAutoUpgrade(ctx, inst, e); err != nil {
				u.Detail("%v", err)
			}
		}
		if schedErr == nil {
			if _, err := sched.Remove(ctx); err != nil {
				u.Detail("%v", err)
			}
		}
		app.forget(inst.Dir)
		body := []string{"Your settings are in " + out.EnvFile + " and rexctl leaves this instance alone from now on. Automatic upgrades are off."}
		if o.Schedule != host.ScheduleOff {
			body = append(body, "Fluxer backs itself up "+o.Schedule+" into its volume.")
		}
		u.Card(ui.Card{
			Title:    "Fluxer runs from " + tool,
			Body:     body,
			Commands: []string{"cd " + shellWord(inst.Dir), tool + " ps", tool + " logs"},
		})
		return nil
	})
}
