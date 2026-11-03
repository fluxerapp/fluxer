// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"
	"io"
	"net/mail"
	"os"
	"slices"
	"strconv"
	"strings"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type emailFlags struct {
	Host         string
	Port         int
	TLS          string
	Username     string
	PasswordFile string
	From         string
}

func readSecretFile(app *App, path, flag string) (string, error) {
	var data []byte
	var err error
	if path == "-" {
		data, err = io.ReadAll(app.UI.Stdin())
	} else {
		data, err = os.ReadFile(path)
	}
	if err != nil {
		return "", ui.Failed(fmt.Sprintf("Couldn't read the file passed with --%s.", flag), "Check the path and try again.")
	}
	secret := strings.TrimRight(string(data), "\r\n")
	if secret == "" {
		return "", ui.Usage(fmt.Sprintf("The file passed with --%s is empty.", flag))
	}
	return secret, nil
}

func validHost(s string) error {
	s = strings.TrimSpace(s)
	if s == "" || strings.ContainsAny(s, " /:") {
		return ui.Usage("Pass --host as a host name, like smtp.example.com.")
	}
	return nil
}

func validFrom(s string) error {
	if _, err := mail.ParseAddress(s); err != nil {
		return ui.Usage("Pass --from as an email address, like Example Chat <noreply@chat.example.com>.")
	}
	return nil
}

func validPort(s string) error {
	n, err := strconv.Atoi(strings.TrimSpace(s))
	if err != nil || n < 1 || n > 65535 {
		return ui.Usage("Pass --port as a number from 1 to 65535.")
	}
	return nil
}

const tlsUsage = "how the connection is encrypted (starttls, implicit, opportunistic or none). Picked from the port when left out"

func validTLS(mode, flag string) error {
	if mode == "" || slices.Contains(adminapi.TLSModes, mode) {
		return nil
	}
	return ui.Usage(fmt.Sprintf("Pass --%s starttls, implicit, opportunistic or none.", flag))
}

func tlsFix(message, fallback string) string {
	if certificateFailed(message) {
		return "If your email server's certificate comes from your own CA, put that CA's PEM file in the ca folder of your instance, then run rexctl config set FLUXER_NODE_EXTRA_CA_CERTS " + stack.NodeCADir + "/<file>. If it doesn't use encryption, run rexctl email set --tls none."
	}
	if adminapi.StartTLSFailed(message) {
		return "If your email server doesn't use encryption, run rexctl email set --tls none."
	}
	return fallback
}

func certificateFailed(message string) bool {
	return strings.Contains(strings.ToLower(message), "certificate")
}

func smtpReason(message string) string {
	if certificateFailed(message) {
		return "Your email server's certificate isn't trusted. It may be self-signed."
	}
	return sentenceOf(message)
}

func newEmailCmd(app *App) *cobra.Command {
	c := &cobra.Command{Use: "email", Short: "Set up how your instance sends email", Args: noArgs}
	var f emailFlags
	set := &cobra.Command{Use: "set", Short: "Set the SMTP server", Args: noArgs}
	set.Flags().StringVar(&f.Host, "host", "", "the SMTP server")
	set.Flags().IntVar(&f.Port, "port", 587, "the SMTP port")
	set.Flags().StringVar(&f.TLS, "tls", "", tlsUsage)
	set.Flags().StringVar(&f.Username, "username", "", "the username for the SMTP server")
	set.Flags().StringVar(&f.PasswordFile, "password-file", "", "file with the SMTP password, or - for stdin")
	set.Flags().StringVar(&f.From, "from", "", "sender, like Example Chat <noreply@chat.example.com>")
	set.RunE = func(c *cobra.Command, _ []string) error {
		if err := validTLS(f.TLS, "tls"); err != nil {
			return err
		}
		port := strconv.Itoa(f.Port)
		fields := []ui.Field{
			{Flag: "host", Title: "SMTP server", Value: &f.Host, Validate: validHost},
			{Flag: "port", Title: "Port", Value: &port, Default: "587", Validate: validPort},
		}
		if app.UI.Interactive() || c.Flags().Changed("username") {
			fields = append(fields, ui.Field{Flag: "username", Title: "SMTP username", Value: &f.Username, Optional: true})
		}
		fields = append(fields, ui.Field{Flag: "from", Title: "Send from", Value: &f.From, Validate: validFrom})
		if err := app.UI.Ask(c, fields...); err != nil {
			return err
		}
		if err := validHost(f.Host); err != nil {
			return ui.Usage(err.Error())
		}
		if err := validPort(port); err != nil {
			return ui.Usage(err.Error())
		}
		from, err := mail.ParseAddress(f.From)
		if err != nil {
			return ui.Usage("Pass --from as an email address, like Example Chat <noreply@chat.example.com>.")
		}
		var password string
		if f.PasswordFile != "" {
			if password, err = readSecretFile(app, f.PasswordFile, "password-file"); err != nil {
				return err
			}
		} else if app.UI.Interactive() && f.Username != "" {
			if err := app.UI.Ask(c, ui.Field{Flag: "password-file", Title: "SMTP password", Value: &password, Secret: true}); err != nil {
				return err
			}
		}
		portNum, _ := strconv.Atoi(port)
		settings := adminapi.SMTPSettings{
			Host:     strings.TrimSpace(f.Host),
			Port:     portNum,
			TLS:      f.TLS,
			Username: f.Username,
			Password: password,
			FromName: from.Name,
			FromAddr: from.Address,
		}
		return adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			if settings.TLS == "" {
				saved, err := api.SavedTLSMode(ctx, settings.Host, settings.Port)
				if err != nil {
					return err
				}
				settings.TLS = saved
			}
			if err := api.SetSMTP(ctx, settings); err != nil {
				return err
			}
			app.UI.OK("Saved the email settings.")
			res, err := api.TestSMTP(ctx, "")
			if err != nil {
				return err
			}
			if !res.OK {
				return ui.Failed(fmt.Sprintf("Fluxer couldn't sign in to %s. %s", settings.Host, smtpReason(res.Error)), tlsFix(res.Error, "Check the settings, then run rexctl email set again."))
			}
			app.UI.OK("Fluxer signed in to %s.", settings.Host)
			app.UI.Info("Run rexctl email test you@example.com to send a test email.")
			return nil
		})
	}

	test := &cobra.Command{Use: "test [address]", Short: "Send a test email", Args: maxArgs(1)}
	test.RunE = func(c *cobra.Command, args []string) error {
		to := ""
		if len(args) == 1 {
			to = strings.TrimSpace(args[0])
			if _, err := mail.ParseAddress(to); err != nil {
				return ui.Usage("Pass an email address, like you@example.com.")
			}
		}
		return adminRead(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			res, err := api.TestSMTP(ctx, to)
			if err != nil {
				return err
			}
			if !res.OK {
				return ui.Failed(fmt.Sprintf("Fluxer couldn't send email. %s", smtpReason(res.Error)), tlsFix(res.Error, "Check the settings, then run rexctl email set."))
			}
			if to == "" {
				app.UI.OK("Fluxer signed in to your email server.")
				return nil
			}
			app.UI.OK("Sent a test email to %s.", to)
			return nil
		})
	}

	off := &cobra.Command{Use: "off", Short: "Stop sending email", Args: noArgs}
	off.RunE = func(c *cobra.Command, _ []string) error {
		return adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			if err := api.EmailOff(ctx); err != nil {
				return err
			}
			app.UI.OK("Turned off email. Your instance won't send any email until you run rexctl email set.")
			return nil
		})
	}

	c.AddCommand(set, test, off)
	return c
}

func sentenceOf(s string) string {
	s = strings.TrimSpace(s)
	if s == "" {
		return ""
	}
	s = strings.ToUpper(s[:1]) + s[1:]
	if !strings.HasSuffix(s, ".") {
		s += "."
	}
	return s
}
