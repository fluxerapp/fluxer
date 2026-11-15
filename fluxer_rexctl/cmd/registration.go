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

var registrationDone = map[string]string{
	"open":     "Anyone can create an account.",
	"approval": "Anyone can ask for an account, and an admin approves each one.",
	"invite":   "Only people with an invite link can create an account.",
	"closed":   "Nobody can create an account, and invite links stop working.",
}

func newRegistrationCmd(app *App) *cobra.Command {
	c := &cobra.Command{Use: "registration", Short: "Choose who can create an account", Args: noArgs}
	set := &cobra.Command{
		Use:       "set <open|approval|invite|closed>",
		Short:     "Set who can create an account",
		Args:      exactArgs("open, approval, invite or closed"),
		ValidArgs: adminapi.RegistrationModes,
	}
	set.RunE = func(c *cobra.Command, args []string) error {
		patch, err := adminapi.RegistrationPatch(args[0])
		if err != nil {
			return ui.Usage(fmt.Sprintf("Pass %s.", ui.JoinOr(adminapi.RegistrationModes)))
		}
		return adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			if err := api.PatchConfig(ctx, patch); err != nil {
				return err
			}
			app.UI.OK("%s", registrationDone[args[0]])
			return nil
		})
	}

	pending := &cobra.Command{Use: "pending", Short: "List people waiting for approval", Args: noArgs}
	app.jsonFlag(pending)
	pending.RunE = func(c *cobra.Command, _ []string) error {
		return adminRead(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			list, err := api.PendingRegistrations(ctx)
			if err != nil {
				return err
			}
			if app.Flags.JSON {
				return app.UI.JSON(list)
			}
			if len(list) == 0 {
				app.UI.Info("Nobody is waiting for approval.")
				return nil
			}
			rows := make([][]string, 0, len(list))
			for _, p := range list {
				rows = append(rows, []string{p.ID, p.Username, p.Email, shortDate(p.RequestedAt)})
			}
			app.UI.Table([]string{"ID", "Username", "Email", "Asked on"}, rows)
			return nil
		})
	}

	review := func(approve bool) func(*cobra.Command, []string) error {
		return func(c *cobra.Command, args []string) error {
			return adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
				p, err := api.ReviewRegistration(ctx, args[0], approve)
				if err != nil {
					return err
				}
				if approve {
					app.UI.OK("Approved %s.", p.Username)
				} else {
					app.UI.OK("Turned down %s.", p.Username)
				}
				return nil
			})
		}
	}
	approve := &cobra.Command{Use: "approve <id>", Short: "Approve a person waiting for approval", Args: exactArgs("the request ID"), RunE: review(true)}
	deny := &cobra.Command{Use: "deny <id>", Short: "Turn down a person waiting for approval", Args: exactArgs("the request ID"), RunE: review(false)}
	c.AddCommand(set, pending, approve, deny)
	return c
}
