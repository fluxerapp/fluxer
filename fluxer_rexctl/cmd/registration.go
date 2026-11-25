// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"
	"strings"
	"time"

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

var registrationLabels = map[string]string{
	"open":     "Anyone can sign up",
	"approval": "Anyone can ask, an admin approves",
	"invite":   "Only people with an invite link",
	"closed":   "Nobody can sign up",
}

func registrationMode(ctx context.Context, api *adminapi.Client) (string, error) {
	values, err := api.ConfigValues(ctx)
	if err != nil {
		return "", err
	}
	k, err := adminapi.FindConfigKey(values, adminapi.RegistrationModeKey)
	if err != nil {
		return "", err
	}
	mode, _ := k.Value.(string)
	return mode, nil
}

func pickRegistration(app *App, current string) (string, error) {
	items := make([]ui.PickItem, 0, len(adminapi.RegistrationModes))
	for _, m := range adminapi.RegistrationModes {
		items = append(items, ui.PickItem{Value: m, Label: registrationLabels[m], Hint: m})
	}
	return app.UI.Pick("Who can create an account?", items, current)
}

func setRegistration(c *cobra.Command, app *App, mode string) error {
	mode = strings.ToLower(strings.TrimSpace(mode))
	patch, err := adminapi.RegistrationPatch(mode)
	if err != nil {
		return ui.Usage(fmt.Sprintf("Pass %s.", ui.JoinOr(adminapi.RegistrationModes)))
	}
	return adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
		if err := api.PatchConfig(ctx, patch); err != nil {
			return err
		}
		app.UI.OK("%s", registrationDone[mode])
		if mode != "invite" || !app.UI.Interactive() {
			return nil
		}
		ok, err := app.UI.ConfirmWith(ui.Question{Text: "Create an invite link now?", Detail: "It works for one person for 7 days.", Default: true})
		if err != nil || !ok {
			return err
		}
		return createInvite(ctx, app, api, adminapi.InviteOptions{Uses: 1, Expires: 7 * 24 * time.Hour})
	})
}

func newRegistrationCmd(app *App) *cobra.Command {
	c := &cobra.Command{
		Use:   "registration",
		Short: "Choose who can create an account",
		Long:  "Shows who can create an account on your instance and offers to change it.",
		Example: `rexctl registration
rexctl registration set invite
rexctl registration pending`,
		Args: noArgs,
	}
	app.jsonFlag(c)
	set := &cobra.Command{
		Use:   "set [open|approval|invite|closed]",
		Short: "Set who can create an account",
		Long:  "Set who can create an account. open lets anyone sign up, approval lets an admin approve each request, invite needs an invite link and closed stops sign-ups and invite links. Leave the mode out on a terminal to pick it from a list.",
		Example: `rexctl registration set invite
rexctl registration set approval`,
		Args:      maxArgs(1),
		ValidArgs: adminapi.RegistrationModes,
	}
	set.RunE = func(c *cobra.Command, args []string) error {
		if len(args) == 1 {
			return setRegistration(c, app, args[0])
		}
		if !app.UI.Interactive() {
			return ui.Usage(fmt.Sprintf("Pass %s.", ui.JoinOr(adminapi.RegistrationModes)))
		}
		var current string
		if err := adminRead(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			var err error
			current, err = registrationMode(ctx, api)
			return err
		}); err != nil {
			return err
		}
		mode, err := pickRegistration(app, current)
		if err != nil {
			return err
		}
		if mode == current {
			app.UI.OK("%s", registrationDone[mode])
			return nil
		}
		return setRegistration(c, app, mode)
	}
	c.RunE = func(c *cobra.Command, _ []string) error {
		var mode string
		var waiting int
		err := adminRead(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			var err error
			if mode, err = registrationMode(ctx, api); err != nil {
				return err
			}
			if mode == "approval" {
				list, err := api.PendingRegistrations(ctx)
				if err != nil {
					return err
				}
				waiting = len(list)
			}
			return nil
		})
		if err != nil {
			return err
		}
		if app.Flags.JSON {
			return app.UI.JSON(map[string]any{"mode": mode, "pending": waiting})
		}
		rows := []ui.KV{{Key: "Sign-up", Value: registrationLabels[mode], Hint: mode}}
		if mode == "approval" {
			rows = append(rows, ui.KV{Key: "Waiting", Value: plural(waiting, "person", "people")})
		}
		app.UI.Card(ui.Card{Title: "Registration", Rows: rows})
		if !app.UI.Interactive() {
			app.UI.Next("rexctl registration set <open|approval|invite|closed>")
			return nil
		}
		items := []ui.PickItem{{Value: "set", Label: "Change who can sign up"}}
		if mode == "approval" && waiting > 0 {
			items = append(items, ui.PickItem{Value: "pending", Label: "See who is waiting"})
		}
		if mode == "invite" {
			items = append(items, ui.PickItem{Value: "invite", Label: "Create an invite link"})
		}
		items = append(items, ui.PickItem{Value: "", Label: "Done"})
		choice, err := app.UI.Pick("What do you want to do?", items, "")
		if err != nil || choice == "" {
			return err
		}
		switch choice {
		case "pending":
			return listPending(c, app)
		case "invite":
			return adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
				return createInvite(ctx, app, api, adminapi.InviteOptions{Uses: 1, Expires: 7 * 24 * time.Hour})
			})
		}
		next, err := pickRegistration(app, mode)
		if err != nil || next == mode {
			return err
		}
		return setRegistration(c, app, next)
	}

	pending := &cobra.Command{
		Use:     "pending",
		Short:   "List people waiting for approval",
		Example: `rexctl registration pending`,
		Args:    noArgs,
	}
	app.jsonFlag(pending)
	pending.RunE = func(c *cobra.Command, _ []string) error {
		return listPending(c, app)
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
	approve := &cobra.Command{Use: "approve <id>", Short: "Approve a person waiting for approval", Example: "rexctl registration approve alice", Args: exactArgs("the request ID"), RunE: review(true)}
	deny := &cobra.Command{Use: "deny <id>", Short: "Turn down a person waiting for approval", Example: "rexctl registration deny 1234567890", Args: exactArgs("the request ID"), RunE: review(false)}
	c.AddCommand(set, pending, approve, deny)
	return c
}

func listPending(c *cobra.Command, app *App) error {
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
