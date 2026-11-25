// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"
	"strconv"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func parseExpiry(s string) (time.Duration, error) {
	s = strings.ToLower(strings.TrimSpace(s))
	bad := ui.Usage(fmt.Sprintf("Pass --expires as a time like 12h, 7d or 4w, or never. %q isn't one.", s))
	if s == "never" || s == "0" {
		return 0, nil
	}
	if len(s) < 2 {
		return 0, bad
	}
	n, err := strconv.Atoi(s[:len(s)-1])
	if err != nil || n <= 0 {
		return 0, bad
	}
	unit := map[byte]time.Duration{'m': time.Minute, 'h': time.Hour, 'd': 24 * time.Hour, 'w': 7 * 24 * time.Hour}[s[len(s)-1]]
	if unit == 0 {
		return 0, bad
	}
	return time.Duration(n) * unit, nil
}

func expiryText(expiresAt *string) string {
	if expiresAt == nil {
		return "never"
	}
	t, err := time.Parse(time.RFC3339, *expiresAt)
	if err != nil {
		return *expiresAt
	}
	t = t.Local()
	if time.Until(t) < 24*time.Hour {
		return t.Format("15:04 MST on 2 January")
	}
	return t.Format("2 January")
}

func expiryWhen(rfc string) string {
	t, err := time.Parse(time.RFC3339, rfc)
	if err != nil {
		return "on " + rfc
	}
	t = t.Local()
	if time.Until(t) < 24*time.Hour {
		return t.Format("at 15:04 MST on 2 January")
	}
	return t.Format("on 2 January")
}

func newInvitesCmd(app *App) *cobra.Command {
	c := &cobra.Command{
		Use:   "invites",
		Short: "Manage invite links for new accounts",
		Example: `rexctl invites create --uses 5 --expires 2d --label "book club"
rexctl invites list`,
		Args: noArgs,
	}

	var uses int
	var expires, label string
	create := &cobra.Command{
		Use:   "create",
		Short: "Create an invite link",
		Long:  "Create an invite link that lets people create an account while registration is set to invite. Send the link to the people you want to invite.",
		Example: `rexctl invites create
rexctl invites create --uses 0 --expires never
rexctl invites create --uses 10 --expires 12h --label "launch party"`,
		Args: noArgs,
	}
	create.Flags().IntVar(&uses, "uses", 1, "how many people can use the link, or 0 for no limit")
	create.Flags().StringVar(&expires, "expires", "7d", "how long the link works, like 12h, 7d or never")
	create.Flags().StringVar(&label, "label", "", "a note to help you recognise the link")
	create.RunE = func(c *cobra.Command, _ []string) error {
		if uses < 0 {
			return ui.Usage("Pass --uses as 0 or more.")
		}
		ttl, err := parseExpiry(expires)
		if err != nil {
			return err
		}
		return adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			return createInvite(ctx, app, api, adminapi.InviteOptions{Uses: uses, Expires: ttl, Label: label})
		})
	}

	list := &cobra.Command{Use: "list", Short: "List invite links", Example: "rexctl invites list\nrexctl invites list --json", Args: noArgs}
	app.jsonFlag(list)
	list.RunE = func(c *cobra.Command, _ []string) error {
		return adminRead(c, app, func(ctx context.Context, api *adminapi.Client, inst *instance.Instance) error {
			invites, err := api.ListInvites(ctx, inst.Origin())
			if err != nil {
				return err
			}
			if app.Flags.JSON {
				return app.UI.JSON(invites)
			}
			if len(invites) == 0 {
				app.UI.Info("There are no working invite links. Run rexctl invites create to make one.")
				return nil
			}
			rows := make([][]string, 0, len(invites))
			for _, inv := range invites {
				used := strconv.Itoa(inv.Uses)
				if inv.MaxUses != nil {
					used = fmt.Sprintf("%d of %d", inv.Uses, *inv.MaxUses)
				}
				rows = append(rows, []string{inv.ID, inv.Label, used, expiryText(inv.ExpiresAt), inv.URL})
			}
			app.UI.Table([]string{"ID", "Label", "Used", "Expires", "Link"}, rows)
			return nil
		})
	}

	revoke := &cobra.Command{Use: "revoke <id>", Short: "Stop an invite link from working", Example: "rexctl invites revoke 1234567890", Args: exactArgs("the invite ID")}
	revoke.RunE = func(c *cobra.Command, args []string) error {
		return adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			if err := api.RevokeInvite(ctx, args[0]); err != nil {
				return err
			}
			app.UI.OK("The invite link %s doesn't work anymore.", args[0])
			return nil
		})
	}

	c.AddCommand(create, list, revoke)
	return c
}

func createInvite(ctx context.Context, app *App, api *adminapi.Client, opts adminapi.InviteOptions) error {
	cfg, err := api.Config(ctx)
	if err != nil {
		return err
	}
	if !cfg.Registration.AdminRegistrationUrlsEnabled {
		return ui.Refused("Invite links are turned off on your instance.", "Run rexctl registration set invite to turn them on.")
	}
	inv, err := api.CreateInvite(ctx, opts)
	if err != nil {
		return err
	}
	who := "any number of people"
	if opts.Uses > 0 {
		who = plural(opts.Uses, "person", "people")
	}
	if inv.ExpiresAt == nil {
		app.UI.OK("Created an invite for %s. It never expires.", who)
	} else {
		app.UI.OK("Created an invite for %s. It expires %s.", who, expiryWhen(*inv.ExpiresAt))
	}
	printLink(app, inv.URL)
	return nil
}
