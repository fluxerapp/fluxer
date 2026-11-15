// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const userArg = "a username, email address or user ID"

type adminFunc func(ctx context.Context, api *adminapi.Client, inst *instance.Instance) error

func adminRead(c *cobra.Command, app *App, fn adminFunc) error {
	ctx := c.Context()
	api, inst, err := app.Admin(ctx)
	if err != nil {
		return err
	}
	return adminapi.Explain(fn(ctx, api, inst))
}

func adminWrite(c *cobra.Command, app *App, fn adminFunc) error {
	ctx := c.Context()
	api, inst, err := app.Admin(ctx)
	if err != nil {
		return err
	}
	return app.Locked(ctx, inst, func() error {
		return adminapi.Explain(fn(ctx, api, inst))
	})
}

func printLink(app *App, link string) {
	app.UI.Highlight(link)
}

func yesNo(b bool) string {
	if b {
		return "yes"
	}
	return "no"
}

func plural(n int, one, many string) string {
	if n == 1 {
		return fmt.Sprintf("%d %s", n, one)
	}
	return fmt.Sprintf("%d %s", n, many)
}

func shortDate(rfc string) string {
	t, err := time.Parse(time.RFC3339, rfc)
	if err != nil {
		return rfc
	}
	return t.Local().Format("2 Jan 2006")
}

func userStatus(u adminapi.User) string {
	var parts []string
	if u.Admin {
		parts = append(parts, "admin")
	}
	if u.Banned {
		parts = append(parts, "banned")
	}
	return strings.Join(parts, ", ")
}

func newUsersCmd(app *App) *cobra.Command {
	c := &cobra.Command{Use: "users", Short: "Manage accounts on your instance", Args: noArgs}

	var search string
	var admins bool
	list := &cobra.Command{Use: "list", Short: "List accounts", Args: noArgs}
	list.Flags().StringVar(&search, "search", "", "only show accounts that match this text")
	list.Flags().BoolVar(&admins, "admins", false, "only show admins")
	app.jsonFlag(list)
	list.RunE = func(c *cobra.Command, _ []string) error {
		return adminRead(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			users, err := api.ListUsers(ctx, search, admins)
			if err != nil {
				return err
			}
			if app.Flags.JSON {
				return app.UI.JSON(users)
			}
			if len(users) == 0 {
				app.UI.Info("No accounts match.")
				return nil
			}
			rows := make([][]string, 0, len(users))
			for _, u := range users {
				rows = append(rows, []string{u.Username, u.Email, u.ID, shortDate(u.CreatedAt), userStatus(u)})
			}
			app.UI.Table([]string{"Username", "Email", "ID", "Joined", "Status"}, rows)
			return nil
		})
	}

	show := &cobra.Command{Use: "show <user>", Short: "Show one account", Args: exactArgs(userArg)}
	app.jsonFlag(show)
	show.RunE = func(c *cobra.Command, args []string) error {
		return adminRead(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			u, err := api.ShowUser(ctx, args[0])
			if err != nil {
				return err
			}
			if app.Flags.JSON {
				return app.UI.JSON(u)
			}
			lastActive := "never"
			if u.LastActiveAt != nil {
				lastActive = shortDate(*u.LastActiveAt)
			}
			email := u.Email
			if email == "" {
				email = "none"
			}
			acls := "none"
			if len(u.ACLs) > 0 {
				acls = strings.Join(u.ACLs, ", ")
			}
			app.UI.Table([]string{"Account", u.Username}, [][]string{
				{"ID", u.ID},
				{"Email", email},
				{"Email confirmed", yesNo(u.Verified)},
				{"Joined", shortDate(u.CreatedAt)},
				{"Last active", lastActive},
				{"Admin", yesNo(u.Admin)},
				{"Permissions", acls},
				{"Two-factor", yesNo(u.MFA)},
				{"Banned", yesNo(u.Banned)},
				{"Disabled", yesNo(u.Disabled)},
				{"Sessions", fmt.Sprint(u.Sessions)},
			})
			return nil
		})
	}

	var email string
	var admin bool
	create := &cobra.Command{Use: "create <username>", Short: "Create an account and print a link to set its password", Args: exactArgs("a username")}
	create.Flags().StringVar(&email, "email", "", "email address for the account")
	create.Flags().BoolVar(&admin, "admin", false, "make the account an admin")
	create.RunE = func(c *cobra.Command, args []string) error {
		return adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			created, err := api.CreateUser(ctx, args[0], email, admin)
			var apiErr *adminapi.APIError
			if email != "" && errors.As(err, &apiErr) && apiErr.Code == "USERNAME_SIGN_IN_ONLY" {
				return ui.Refused("People on your instance sign in with a username and don't have an email address on their account.", "Run the command again without --email.")
			}
			if err != nil {
				return err
			}
			if admin {
				app.UI.OK("Created %s as an admin.", created.User.Username)
			} else {
				app.UI.OK("Created %s.", created.User.Username)
			}
			if created.ResetLink != "" {
				app.UI.Info("Send them this link to set a password.")
				printLink(app, created.ResetLink)
			}
			return nil
		})
	}

	resetLink := &cobra.Command{Use: "reset-link <user>", Short: "Print a link to set a new password", Args: exactArgs(userArg)}
	resetLink.RunE = func(c *cobra.Command, args []string) error {
		return adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			u, err := api.ResolveUser(ctx, args[0])
			if err != nil {
				return err
			}
			link, expires, err := api.ResetLink(ctx, u.ID)
			var apiErr *adminapi.APIError
			if errors.As(err, &apiErr) && apiErr.Code == "USERNAME_SIGN_IN_ONLY" {
				return ui.Failed("People on your instance reset their password by email from the sign-in page.", "If email has stopped working, run rexctl email off, then try again.")
			}
			if err != nil {
				return err
			}
			if t, err := time.Parse(time.RFC3339, expires); err == nil {
				app.UI.OK("Made a link for %s to set a new password. It works until %s.", u.Username, t.Local().Format("15:04 on 2 January"))
			} else {
				app.UI.OK("Made a link for %s to set a new password.", u.Username)
			}
			printLink(app, link)
			return nil
		})
	}

	signOut := &cobra.Command{Use: "sign-out <user>", Short: "Sign an account out everywhere", Args: exactArgs(userArg)}
	signOut.RunE = func(c *cobra.Command, args []string) error {
		return adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			u, err := api.ResolveUser(ctx, args[0])
			if err != nil {
				return err
			}
			n, err := api.SignOut(ctx, u.ID)
			if err != nil {
				return err
			}
			if n == 0 {
				app.UI.OK("%s wasn't signed in anywhere.", u.Username)
				return nil
			}
			app.UI.OK("Signed %s out of %s.", u.Username, plural(n, "session", "sessions"))
			return nil
		})
	}

	disableMFA := &cobra.Command{Use: "disable-mfa <user>", Short: "Turn off two-factor authentication for an account", Args: exactArgs(userArg)}
	disableMFA.RunE = func(c *cobra.Command, args []string) error {
		return adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			u, err := api.ResolveUser(ctx, args[0])
			if err != nil {
				return err
			}
			if !u.MFA {
				app.UI.OK("%s doesn't use two-factor authentication.", u.Username)
				return nil
			}
			if err := api.DisableMFA(ctx, u.ID); err != nil {
				return err
			}
			app.UI.OK("Turned off two-factor authentication for %s.", u.Username)
			return nil
		})
	}

	var reason string
	var deleteMessages bool
	ban := &cobra.Command{Use: "ban <user>", Short: "Ban an account", Args: exactArgs(userArg)}
	ban.Flags().StringVar(&reason, "reason", "", "why you're banning them, one of "+ui.JoinOr(adminapi.BanReasonNames()))
	ban.Flags().BoolVar(&deleteMessages, "delete-messages", false, "also delete their messages")
	ban.RunE = func(c *cobra.Command, args []string) error {
		if r, ok := adminapi.FindBanReason(reason); ok {
			reason = r.Name
		}
		options := make([]ui.Option, len(adminapi.BanReasons))
		for i, r := range adminapi.BanReasons {
			options[i] = ui.Option{Label: r.Label, Value: r.Name}
		}
		err := app.UI.Ask(c, ui.Field{
			Flag:    "reason",
			Title:   "Why are you banning them?",
			Value:   &reason,
			Options: options,
			Validate: func(s string) error {
				if _, ok := adminapi.FindBanReason(s); !ok {
					return ui.Usage(fmt.Sprintf("Pass --reason with %s.", ui.JoinOr(adminapi.BanReasonNames())))
				}
				return nil
			},
		})
		if err != nil {
			return err
		}
		why, ok := adminapi.FindBanReason(reason)
		if !ok {
			return ui.Usage(fmt.Sprintf("Pass --reason with %s.", ui.JoinOr(adminapi.BanReasonNames())))
		}
		return adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			u, err := api.ResolveUser(ctx, args[0])
			if err != nil {
				return err
			}
			question := fmt.Sprintf("Ban %s?", u.Username)
			if deleteMessages {
				question = fmt.Sprintf("Ban %s and delete their messages?", u.Username)
			}
			ok, err := app.UI.Confirm(question, false)
			if err != nil {
				return err
			}
			if !ok {
				return ui.Refused(fmt.Sprintf("Didn't ban %s.", u.Username), "")
			}
			if err := api.Ban(ctx, u.ID, why, ""); err != nil {
				return err
			}
			if !deleteMessages {
				app.UI.OK("Banned %s.", u.Username)
				return nil
			}
			n, err := api.DeleteMessages(ctx, u.ID, why.Label)
			if err != nil {
				app.UI.OK("Banned %s.", u.Username)
				return err
			}
			if n == 0 {
				app.UI.OK("Banned %s. They had no messages to delete.", u.Username)
				return nil
			}
			app.UI.OK("Banned %s. Fluxer is deleting their %s in the background.", u.Username, plural(n, "message", "messages"))
			return nil
		})
	}

	unban := &cobra.Command{Use: "unban <user>", Short: "Lift a ban", Args: exactArgs(userArg)}
	unban.RunE = func(c *cobra.Command, args []string) error {
		return adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			u, err := api.ResolveUser(ctx, args[0])
			if err != nil {
				return err
			}
			if !u.Banned {
				app.UI.OK("%s isn't banned.", u.Username)
				return nil
			}
			if err := api.Unban(ctx, u.ID); err != nil {
				return err
			}
			app.UI.OK("Lifted the ban on %s.", u.Username)
			return nil
		})
	}

	var confirm string
	del := &cobra.Command{Use: "delete <user>", Short: "Delete an account and its data", Args: exactArgs(userArg)}
	confirmFlag(del, &confirm)
	del.RunE = func(c *cobra.Command, args []string) error {
		return adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, inst *instance.Instance) error {
			u, err := api.ResolveUser(ctx, args[0])
			if err != nil {
				return err
			}
			if u.DeletionFinished() {
				app.UI.OK("Account %s is already deleted.", u.ID)
				return nil
			}
			if err := app.UI.ConfirmDestructive(fmt.Sprintf("This deletes %s and everything they posted.", u.Username), inst.Domain(), confirm); err != nil {
				return err
			}
			if err := api.DeleteUser(ctx, u.ID); err != nil {
				if errors.Is(err, adminapi.ErrDeletionPending) {
					app.UI.OK("%s is already being deleted.", u.Username)
					return nil
				}
				if errors.Is(err, adminapi.ErrAlreadyDeleted) {
					app.UI.OK("Account %s is already deleted.", u.ID)
					return nil
				}
				return err
			}
			app.UI.OK("Deleted %s.", u.Username)
			return nil
		})
	}

	c.AddCommand(list, show, create, resetLink, signOut, disableMFA, ban, unban, del)
	return c
}
