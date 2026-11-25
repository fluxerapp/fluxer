// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func newVoiceLogsCmd(app *App) *cobra.Command {
	var o rt.LogOptions
	c := &cobra.Command{
		Use:   "logs [id]",
		Short: "Show the logs of this machine's voice servers",
		Example: "  rexctl voice logs fra-server-2\n" +
			"  rexctl voice logs fra-server-2 --follow\n" +
			"  rexctl voice logs --since 1h",
		Args: maxArgs(1),
	}
	c.Flags().BoolVarP(&o.Follow, "follow", "f", false, "keep printing new lines")
	c.Flags().StringVar(&o.Since, "since", "", "show lines from this long ago, such as 30m")
	c.Flags().IntVarP(&o.Tail, "tail", "n", 200, "how many lines to show from the end of each log")
	c.RunE = func(c *cobra.Command, args []string) error {
		inst, err := app.Instance()
		if err != nil {
			return err
		}
		return voiceLogs(c.Context(), app, inst, args, o)
	}
	return c
}

func voiceLogs(ctx context.Context, app *App, inst *instance.Instance, args []string, o rt.LogOptions) error {
	owner, servers, err := voiceServersHere(inst, args)
	if err != nil {
		return err
	}
	if len(servers) == 0 {
		return ui.Refused("This machine runs no voice servers.", "Run rexctl voice list to see the servers of your instance.")
	}
	if o.Follow && len(servers) > 1 {
		return ui.Usage("Pass the ID of one server with --follow.")
	}
	r, err := app.Runtime(ctx, owner)
	if err != nil {
		return err
	}
	w := app.UI.Stdout()
	for _, srv := range servers {
		if len(servers) > 1 {
			app.UI.Section(srv.ID)
		}
		if isBundled(owner, srv) {
			lo := o
			lo.Parts = []string{"voice"}
			err = r.Logs(ctx, w, lo)
		} else {
			err = rt.VoiceOf(r).Logs(ctx, w, srv, o)
		}
		if err != nil {
			return err
		}
	}
	return nil
}

func newVoiceRestartCmd(app *App) *cobra.Command {
	c := &cobra.Command{
		Use:   "restart [id]",
		Short: "Restart the voice servers on this machine",
		Long:  "Restart local voice servers. Their calls reconnect.",
		Example: "  rexctl voice restart fra-server-2\n" +
			"  rexctl voice restart",
		Args: maxArgs(1),
	}
	c.RunE = func(c *cobra.Command, args []string) error {
		inst, err := app.Instance()
		if err != nil {
			return err
		}
		return voiceRestart(c.Context(), app, inst, args, true)
	}
	return c
}

func voiceRestart(ctx context.Context, app *App, inst *instance.Instance, args []string, ask bool) error {
	owner, servers, err := voiceServersHere(inst, args)
	if err != nil {
		return err
	}
	if len(servers) == 0 {
		return ui.Refused("This machine runs no voice servers.", "Run rexctl voice list to see the servers of your instance.")
	}
	ids := make([]string, len(servers))
	for i, s := range servers {
		ids[i] = s.ID
	}
	if ask {
		if err := confirmVoice(app, fmt.Sprintf("Restart %s? Calls on it reconnect.", ui.JoinAnd(ids))); err != nil {
			return err
		}
	}
	r, err := app.Runtime(ctx, owner)
	if err != nil {
		return err
	}
	return app.Locked(ctx, owner, func() error {
		for _, srv := range servers {
			err := app.UI.Step(ctx, "Restarting voice server "+srv.ID, func(ctx context.Context, s *ui.Step) error {
				var err error
				if isBundled(owner, srv) {
					err = r.Restart(ctx, s, []string{"voice"})
				} else {
					err = rt.VoiceOf(r).Restart(ctx, srv)
				}
				if err != nil {
					return err
				}
				s.Done("Restarted voice server %s.", srv.ID)
				return nil
			})
			if err != nil {
				return err
			}
		}
		return nil
	})
}
