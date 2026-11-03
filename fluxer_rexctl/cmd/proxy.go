// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"cmp"
	"context"
	"fmt"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func newProxyCmd(app *App) *cobra.Command {
	c := &cobra.Command{
		Use:   "proxy",
		Short: "Use your own reverse proxy in front of Fluxer",
		Args:  noArgs,
	}
	var bind string
	on := &cobra.Command{
		Use:   "on",
		Short: "Serve plain HTTP for your own proxy",
		Args:  noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			mode := settings.EdgeProxy
			changes := map[string]*string{settings.EdgeMode: &mode}
			if c.Flags().Changed("bind") {
				changes[settings.EdgeBind] = &bind
			}
			return setProxy(c.Context(), app, changes)
		},
	}
	on.Flags().StringVar(&bind, "bind", "", "address your proxy connects to, kept when left out (127.0.0.1:8080 the first time)")
	off := &cobra.Command{
		Use:   "off",
		Short: "Serve HTTPS directly with a certificate from Let's Encrypt",
		Args:  noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return setProxy(c.Context(), app, map[string]*string{settings.EdgeMode: nil, settings.EdgeBind: nil})
		},
	}
	c.AddCommand(on, off)
	return c
}

func setProxy(ctx context.Context, app *App, changes map[string]*string) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	on := changes[settings.EdgeMode] != nil
	if _, ok := changes[settings.EdgeBind]; on && !ok {
		bind := cmp.Or(inst.State.Settings[settings.EdgeBind], "127.0.0.1:8080")
		changes[settings.EdgeBind] = &bind
	}
	if mode := inst.State.Settings[settings.EdgeMode]; !on && mode != "" && mode != settings.EdgeBundled {
		if err := allowLowPorts(ctx, app, inst); err != nil {
			return err
		}
	}
	return app.Change(ctx, inst, func(e *engine.Engine) error {
		title := "Switching Fluxer to HTTPS"
		if on {
			title = "Switching Fluxer to plain HTTP"
		}
		change, err := applySettings(ctx, app, inst, e, changes, title)
		if err != nil {
			return err
		}
		switch {
		case on && change.Unchanged:
			app.UI.OK("Fluxer already serves plain HTTP on %s.", *changes[settings.EdgeBind])
		case change.Unchanged:
			app.UI.OK("Fluxer already serves HTTPS itself.")
		case on:
			app.UI.OK("Fluxer serves plain HTTP on %s.", *changes[settings.EdgeBind])
			app.UI.Info("Send all traffic for %s from your proxy to %s.", inst.Domain(), *changes[settings.EdgeBind])
		default:
			app.UI.OK("Fluxer serves HTTPS for %s itself.", inst.Domain())
			app.UI.Info("Keep ports 80 and 443 open to this machine for the certificate.")
		}
		if !change.Unchanged && !change.Applied {
			app.UI.Info("Fluxer uses the new setup the next time it starts.")
		}
		return nil
	})
}

func allowLowPorts(ctx context.Context, app *App, inst *instance.Instance) error {
	e, err := app.Engine(ctx, inst)
	if err != nil {
		return err
	}
	if !host.LowPortsBlocked(e.Rootless) {
		return nil
	}
	name := e.Kind.Name()
	app.UI.Warn(fmt.Sprintf("Rootless %s can't use ports 80 and 443, because only root can use ports below 1024 on this machine.", name), "")
	ok, err := app.UI.Confirm("Let programs that don't run as root use ports 80 and up on this machine?", true)
	if err != nil {
		return err
	}
	if !ok {
		return ui.Refused(fmt.Sprintf("Fluxer can't serve HTTPS until %s can use ports 80 and 443.", name), "Run rexctl proxy off again and let rexctl allow them.")
	}
	if err := host.AllowLowPorts(ctx); err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't let %s use ports 80 and 443.", name), "Run rexctl proxy off --verbose to see why.")
	}
	app.UI.OK("Let %s use ports 80 and 443.", name)
	return nil
}
