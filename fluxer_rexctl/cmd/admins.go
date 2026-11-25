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
	c := &cobra.Command{
		Use:   "admins",
		Short: "Choose who can manage your instance",
		Example: `rexctl admins list
rexctl admins add alice
rexctl admins sign-in-link alice`,
		Args: noArgs,
	}
	list := &cobra.Command{
		Use:     "list",
		Short:   "List the admins",
		Example: "rexctl admins list\nrexctl admins list --json",
		Args:    noArgs,
	}
	app.jsonFlag(list)
	list.RunE = func(c *cobra.Command, _ []string) error {
		return listUsers(c, app, "", true, 0)
	}
	add := &cobra.Command{Use: "add <user>", Short: "Make an account an admin", Example: "rexctl admins add alice", Args: exactArgs(userArg)}
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
	remove := &cobra.Command{
		Use:     "remove <user>",
		Short:   "Remove admin access from an account",
		Long:    "Remove admin access from an account. rexctl refuses to remove the last admin.",
		Example: "rexctl admins remove alice",
		Args:    exactArgs(userArg),
	}
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
			admins, _, err := api.ListUsers(ctx, "", true, 0)
			if err != nil {
				return err
			}
			if lastAdmin(admins, u.ID) {
				return ui.Refused(fmt.Sprintf("%s is the only admin, and nobody could manage your instance without them.", u.Username), "Make someone else an admin first with rexctl admins add <user>.")
			}
			if err := api.SetAdmin(ctx, u.ID, false); err != nil {
				return err
			}
			app.UI.OK("Removed admin access from %s.", u.Username)
			return nil
		})
	}
	signInLink := &cobra.Command{
		Use:     "sign-in-link <user>",
		Short:   "Print a link that signs an admin in once",
		Example: "rexctl admins sign-in-link alice",
		Long:    "Print a link that signs an admin in once. Use it to get in while Require SSO is on and your sign-in provider is down or has been set up wrong.",
		Args:    exactArgs(userArg),
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
	c.AddCommand(list, add, remove, signInLink)
	return c
}

func lastAdmin(admins []adminapi.User, id string) bool {
	for _, a := range admins {
		if a.ID != id && !a.Banned {
			return false
		}
	}
	return true
}

func printSignInLink(ctx context.Context, app *App, api *adminapi.Client, id, username string) error {
	link, expires, err := api.SignInLink(ctx, id)
	if err != nil {
		return err
	}
	app.UI.OK("Made a link that signs %s in. %s", username, worksOnceUntil(expires))
	printLink(app, link)
	return nil
}
