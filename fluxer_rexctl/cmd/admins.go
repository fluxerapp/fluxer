// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func newAdminsCmd(app *App) *cobra.Command {
	c := &cobra.Command{Use: "admins", Short: "Choose who can manage your instance", Args: noArgs}
	add := &cobra.Command{Use: "add <user>", Short: "Make an account an admin", Args: exactArgs(userArg)}
	add.RunE = func(c *cobra.Command, args []string) error {
		return adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			u, err := api.ResolveUser(ctx, args[0])
			if err != nil {
				return err
			}
			if err := api.SetAdmin(ctx, u.ID, true); err != nil {
				return err
			}
			app.UI.OK("%s is an admin.", u.Username)
			return nil
		})
	}
	remove := &cobra.Command{Use: "remove <user>", Short: "Remove admin access from an account", Args: exactArgs(userArg)}
	remove.RunE = func(c *cobra.Command, args []string) error {
		return adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			u, err := api.ResolveUser(ctx, args[0])
			if err != nil {
				return err
			}
			if !u.Admin {
				app.UI.OK("%s isn't an admin.", u.Username)
				return nil
			}
			if err := api.SetAdmin(ctx, u.ID, false); err != nil {
				return err
			}
			app.UI.OK("Removed admin access from %s.", u.Username)
			return nil
		})
	}
	signInLink := &cobra.Command{
		Use:   "sign-in-link <user>",
		Short: "Print a link that signs an admin in once",
		Long:  "Print a link that signs an admin in once. Use it to get in when Require SSO is on and your sign-in provider is down or set up wrong. See " + docsURL + "sign-in/#single-sign-on.",
		Args:  exactArgs(userArg),
	}
	signInLink.RunE = func(c *cobra.Command, args []string) error {
		return adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			u, err := api.ResolveUser(ctx, args[0])
			if err != nil {
				return err
			}
			if !u.Admin {
				return ui.Refused(fmt.Sprintf("%s isn't an admin.", u.Username), fmt.Sprintf("Run rexctl admins add %s first.", u.Username))
			}
			return printSignInLink(ctx, app, api, u.ID, u.Username)
		})
	}
	c.AddCommand(add, remove, signInLink)
	return c
}

func printSignInLink(ctx context.Context, app *App, api *adminapi.Client, id, username string) error {
	link, expires, err := api.SignInLink(ctx, id)
	if err != nil {
		return err
	}
	app.UI.OK("Made a link that signs %s in. It works once, until %s.", username, expires.Local().Format("15:04 on 2 January"))
	printLink(app, link)
	return nil
}
