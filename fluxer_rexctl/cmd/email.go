// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"net/mail"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"strings"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const caCertsVar = "FLUXER_NODE_EXTRA_CA_CERTS"

type emailFlags struct {
	Host         string
	Port         string
	TLS          string
	Username     string
	PasswordFile string
	From         string
	CAFile       string
}

func validHost(s string) error {
	s = strings.TrimSpace(s)
	if s == "" || strings.ContainsAny(s, " /:") {
		return errors.New("Use a host name such as smtp.example.com.")
	}
	return nil
}

func validFrom(s string) error {
	if _, err := mail.ParseAddress(s); err != nil {
		return errors.New("Use an email address such as Example Chat <noreply@chat.example.com>.")
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
		return "If your email server's certificate comes from your own CA, run rexctl email set --ca-file with that CA's PEM file. If it doesn't use encryption, run rexctl email set --tls none."
	}
	if adminapi.StartTLSFailed(message) || tlsMismatch(message) {
		return "If your email server doesn't use encryption, run rexctl email set --tls none."
	}
	return fallback
}

func certificateFailed(message string) bool {
	return strings.Contains(strings.ToLower(message), "certificate")
}

func tlsMismatch(message string) bool {
	return strings.Contains(strings.ToLower(message), "wrong version number")
}

func smtpReason(message string) string {
	if certificateFailed(message) {
		return "Your email server's certificate isn't trusted. It may be self-signed."
	}
	if tlsMismatch(message) {
		return "The server didn't start encryption on that port."
	}
	return sentence(ui.Inline(message))
}

var tlsOptions = []ui.Option{
	{Label: "Pick from the port", Value: ""},
	{Label: "STARTTLS", Value: "starttls", Hint: "usually port 587"},
	{Label: "TLS from the start", Value: "implicit", Hint: "usually port 465"},
	{Label: "STARTTLS when the server offers it", Value: "opportunistic"},
	{Label: "No encryption", Value: "none", Hint: "sends the password in clear text"},
}

func emailState(cfg *adminapi.EmailConfig) []ui.KV {
	if cfg.Delivery != "smtp" || cfg.SMTP.Host == "" {
		return []ui.KV{{Key: "Email", Value: "off", State: ui.StateWarn, Hint: "Fluxer can't send sign-up or password emails"}}
	}
	s := cfg.SMTP
	tls := s.TLS
	if tls == "" {
		tls = adminapi.TLSModeFor(s.Port)
	}
	user := s.Username
	switch {
	case user == "":
		user = "none"
	case cfg.PasswordSet:
		user += ", password saved"
	}
	from := s.FromAddr
	if s.FromName != "" {
		from = (&mail.Address{Name: s.FromName, Address: s.FromAddr}).String()
	}
	return []ui.KV{
		{Key: "Server", Value: fmt.Sprintf("%s:%d", s.Host, s.Port), State: ui.StateOK},
		{Key: "Encryption", Value: tls},
		{Key: "Username", Value: user},
		{Key: "Sender", Value: from},
	}
}

func newEmailCmd(app *App) *cobra.Command {
	c := &cobra.Command{
		Use:   "email",
		Short: "Set up how your instance sends email",
		Long:  "Shows how your instance sends email and offers to change it. Fluxer sends email to confirm addresses, reset passwords and tell people about new sign-ins on their account.",
		Example: `rexctl email
rexctl email set --host smtp.example.com --username chat --password-file pass.txt --from "Example Chat <noreply@chat.example.com>"
rexctl email test you@example.com`,
		Args: noArgs,
	}
	app.jsonFlag(c)
	var f emailFlags
	set := &cobra.Command{
		Use:   "set",
		Short: "Set the SMTP server",
		Long:  "Set the SMTP server Fluxer sends email through. Anything you leave out keeps its saved value, except that a new server or port without --tls uses the usual encryption for that port. rexctl tests them before saving, so a typo doesn't stop the email Fluxer already sends.",
		Example: `rexctl email set --host smtp.example.com --port 587 --username chat --password-file pass.txt --from noreply@chat.example.com
rexctl email set --host new.example.com
rexctl email set --ca-file my-ca.pem`,
		Args: noArgs,
	}
	set.Flags().StringVar(&f.Host, "host", "", "the SMTP server")
	set.Flags().StringVar(&f.Port, "port", "", "the SMTP port, 587 when nothing is saved")
	set.Flags().StringVar(&f.TLS, "tls", "", tlsUsage)
	set.Flags().StringVar(&f.Username, "username", "", "the username for the SMTP server, or empty for none")
	set.Flags().StringVar(&f.PasswordFile, "password-file", "", "file with the SMTP password, or - for stdin")
	set.Flags().StringVar(&f.From, "from", "", "sender, like Example Chat <noreply@chat.example.com>")
	set.Flags().StringVar(&f.CAFile, "ca-file", "", "PEM file of the CA that signed your email server's certificate")
	set.RunE = func(c *cobra.Command, _ []string) error {
		return setEmail(c.Context(), c, app, &f)
	}

	test := &cobra.Command{
		Use:   "test [address]",
		Short: "Send a test email",
		Example: `rexctl email test
rexctl email test you@example.com`,
		Args: maxArgs(1),
	}
	test.RunE = func(c *cobra.Command, args []string) error {
		to := ""
		if len(args) == 1 {
			to = strings.TrimSpace(args[0])
			if _, err := mail.ParseAddress(to); err != nil {
				return ui.Usage("Pass an email address, like you@example.com.")
			}
		}
		return adminRead(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			return sendTestEmail(ctx, app, api, to)
		})
	}

	off := &cobra.Command{
		Use:     "off",
		Short:   "Stop sending email",
		Example: `rexctl email off`,
		Args:    noArgs,
	}
	off.RunE = func(c *cobra.Command, _ []string) error {
		return emailOff(c, app)
	}

	c.RunE = func(c *cobra.Command, _ []string) error {
		var cfg *adminapi.EmailConfig
		err := adminRead(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			var err error
			cfg, err = api.Email(ctx)
			return err
		})
		if err != nil {
			return err
		}
		if app.Flags.JSON {
			return app.UI.JSON(emailJSON(cfg))
		}
		app.UI.Card(ui.Card{Title: "Email", Rows: emailState(cfg)})
		if !app.UI.Interactive() {
			app.UI.Next("rexctl email set", "rexctl email test you@example.com")
			return nil
		}
		items := []ui.PickItem{{Value: "set", Label: "Change the email server"}}
		if cfg.Delivery == "smtp" {
			items = append(items, ui.PickItem{Value: "test", Label: "Send a test email"}, ui.PickItem{Value: "off", Label: "Turn email off"})
		}
		items = append(items, ui.PickItem{Value: "", Label: "Done"})
		choice, err := app.UI.Pick("What do you want to do?", items, "")
		if err != nil || choice == "" {
			return err
		}
		switch choice {
		case "set":
			return setEmail(c.Context(), set, app, &f)
		case "test":
			var to string
			if err := app.UI.Ask(nil, ui.Field{Title: "Which address should get the test email?", Value: &to, Optional: true,
				Description: "Leave it empty to only check that Fluxer can sign in to your email server.", Validate: func(s string) error {
					if s == "" {
						return nil
					}
					return validFrom(s)
				}}); err != nil {
				return err
			}
			return adminRead(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
				return sendTestEmail(ctx, app, api, strings.TrimSpace(to))
			})
		}
		return emailOff(c, app)
	}

	c.AddCommand(set, test, off)
	return c
}

type emailJSONOut struct {
	Delivery    string `json:"delivery"`
	Host        string `json:"host,omitempty"`
	Port        int    `json:"port,omitempty"`
	TLS         string `json:"tls,omitempty"`
	Username    string `json:"username,omitempty"`
	PasswordSet bool   `json:"password_set"`
	FromName    string `json:"from_name,omitempty"`
	FromAddress string `json:"from_address,omitempty"`
}

func emailJSON(cfg *adminapi.EmailConfig) emailJSONOut {
	s := cfg.SMTP
	delivery := cfg.Delivery
	if delivery == "" {
		delivery = "off"
	}
	return emailJSONOut{Delivery: delivery, Host: s.Host, Port: s.Port, TLS: s.TLS, Username: s.Username, PasswordSet: cfg.PasswordSet, FromName: s.FromName, FromAddress: s.FromAddr}
}

func emailOff(c *cobra.Command, app *App) error {
	return adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
		if err := api.EmailOff(ctx); err != nil {
			return err
		}
		app.UI.OK("Turned off email. Your instance won't send any email until you run rexctl email set.")
		return nil
	})
}

func sendTestEmail(ctx context.Context, app *App, api *adminapi.Client, to string) error {
	res, err := api.TestSMTP(ctx, to)
	if err != nil {
		return err
	}
	if !res.OK {
		return ui.Failed(fmt.Sprintf("Fluxer couldn't send email. %s", smtpReason(res.Error)), tlsFix(res.Error, "Check the settings with rexctl email, then run rexctl email set."))
	}
	if to == "" {
		app.UI.OK("Fluxer signed in to your email server.")
		return nil
	}
	app.UI.OK("Sent a test email to %s.", to)
	return nil
}

func flagSet(c *cobra.Command, name string) bool {
	fl := c.Flags().Lookup(name)
	return fl != nil && fl.Changed
}

func setEmail(ctx context.Context, c *cobra.Command, app *App, f *emailFlags) error {
	if err := validTLS(f.TLS, "tls"); err != nil {
		return err
	}
	api, inst, err := app.Admin(ctx)
	if err != nil {
		return err
	}
	saved, err := api.Email(ctx)
	if err != nil {
		return adminapi.Explain(err)
	}
	if saved.UsernameOnly {
		return ui.Refused("People on your instance sign in with a username, so Fluxer doesn't send email.", "Email works on an instance where people sign in with an email address.")
	}
	old := saved.SMTP
	next := old
	if flagSet(c, "host") {
		next.Host = strings.TrimSpace(f.Host)
	}
	port := strconv.Itoa(cmpPort(old.Port))
	if flagSet(c, "port") {
		port = strings.TrimSpace(f.Port)
	}
	if flagSet(c, "username") {
		next.Username = strings.TrimSpace(f.Username)
	}
	from := old.FromAddr
	if old.FromName != "" {
		from = (&mail.Address{Name: old.FromName, Address: old.FromAddr}).String()
	}
	if flagSet(c, "from") {
		from = f.From
	}
	tls := old.TLS
	if flagSet(c, "tls") {
		tls = f.TLS
	}
	username := next.Username
	fields := []ui.Field{
		{Flag: "host", FlagLabel: "--host", Title: "SMTP server", Placeholder: "smtp.example.com", Value: &next.Host, Validate: validHost},
		{Flag: "port", FlagLabel: "--port", Title: "Port", Value: &port, Validate: settings.ValidPort},
	}
	if app.UI.Interactive() {
		fields = append(fields,
			ui.Field{Flag: "tls", Title: "How is the connection encrypted?", Value: &tls, Options: tlsOptions},
			ui.Field{Flag: "username", Title: "SMTP username", Value: &username, Optional: true, Description: "Leave it empty if the server takes mail without signing in."})
	}
	fields = append(fields, ui.Field{Flag: "from", FlagLabel: "--from", Title: "Send from", Value: &from, Validate: validFrom})
	if err := app.UI.Ask(c, fields...); err != nil {
		return err
	}
	next.Username = strings.TrimSpace(username)
	next.Port, _ = strconv.Atoi(strings.TrimSpace(port))
	next.Host = strings.TrimSpace(next.Host)
	addr, err := mail.ParseAddress(from)
	if err != nil {
		return ui.Usage("--from can't be \"" + from + "\". Use an email address such as Example Chat <noreply@chat.example.com>.")
	}
	next.FromName, next.FromAddr = addr.Name, addr.Address
	if !flagSet(c, "tls") && !app.UI.Interactive() && (next.Host != old.Host || next.Port != old.Port) {
		tls = ""
	}
	next.TLS = tls
	if f.PasswordFile != "" {
		if next.Password, err = readSecret(app, f.PasswordFile, "the SMTP password"); err != nil {
			return err
		}
	} else if next.Username != "" {
		keep := saved.PasswordSet && next.Username == old.Username && next.Host == old.Host
		switch {
		case app.UI.Interactive():
			desc := ""
			if keep {
				desc = "Leave it empty to keep the saved password."
			}
			if err := app.UI.Ask(nil, ui.Field{Title: "SMTP password", Value: &next.Password, Secret: true, Optional: keep, Description: desc}); err != nil {
				return err
			}
		case !keep:
			return ui.Usage("rexctl needs the SMTP password for " + next.Username + " on " + next.Host + ". Pass it with --password-file, or --password-file - to read it from stdin.")
		}
	}
	if next.TLS == "none" && next.Password != "" {
		app.UI.Warn("Without encryption, Fluxer sends the SMTP password in clear text.", "Use starttls or implicit if your email server takes them.")
	}
	caNote := ""
	if f.CAFile != "" {
		if err := installEmailCA(ctx, app, inst, f.CAFile); err != nil {
			if errors.Is(err, errKeptSettings) {
				app.UI.Info("Nothing changed.")
				return nil
			}
			return err
		}
		caNote = "Fluxer keeps trusting the CA certificate from " + f.CAFile + "."
	}
	return app.Locked(ctx, inst, func() error {
		return adminapi.Explain(saveEmail(ctx, app, api, next, caNote))
	})
}

func cmpPort(saved int) int {
	if saved > 0 {
		return saved
	}
	return 587
}

func saveEmail(ctx context.Context, app *App, api *adminapi.Client, next adminapi.SMTPSettings, caNote string) error {
	var refused *ui.ExitError
	err := app.UI.Check(ctx, "Checking the new email settings", func(ctx context.Context, s *ui.Step) error {
		res, err := api.TestSMTPWith(ctx, next, "")
		if err != nil {
			return err
		}
		if !res.OK {
			msg := fmt.Sprintf("Fluxer couldn't sign in to %s. %s", next.Host, smtpReason(res.Error))
			if !app.UI.Interactive() {
				msg += " Your old email settings are still in use."
			}
			refused = ui.Failed(msg, tlsFix(res.Error, "Check the settings, then run rexctl email set again."))
			return refused
		}
		s.Done("Fluxer signed in to %s.", next.Host)
		return nil
	})
	if err != nil && (refused == nil || !app.UI.Interactive()) {
		if refused != nil && caNote != "" {
			app.UI.Info("%s", caNote)
		}
		return err
	}
	if refused != nil {
		ok, err := app.UI.ConfirmWith(ui.Question{Text: "Save these settings anyway?", Detail: "Fluxer can't send email until they work.", Default: false})
		if err != nil {
			return err
		}
		if !ok {
			app.UI.Info("Your old email settings are still in use.")
			if caNote != "" {
				app.UI.Info("%s", caNote)
			}
			return nil
		}
	}
	if err := api.SetSMTP(ctx, next); err != nil {
		return err
	}
	app.UI.OK("Saved the email settings.")
	app.UI.Info("Run rexctl email test you@example.com to send a test email.")
	return nil
}

func installEmailCA(ctx context.Context, app *App, inst *instance.Instance, path string) error {
	name := filepath.Base(path)
	if filepath.Ext(name) != ".pem" && filepath.Ext(name) != ".crt" {
		name += ".pem"
	}
	undo, err := savePEM(path, inst.Path(instance.CADir), name, "Pass --ca-file with the path to your CA's PEM file.")
	if err != nil {
		return err
	}
	value := stack.NodeCADir + "/" + name
	if err := changeConfig(ctx, app, []configChange{{name: caCertsVar, value: &value}}, nil); err != nil {
		undo()
		return err
	}
	return nil
}

func savePEM(path, dir, name, hint string) (func(), error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, ui.Failed(fmt.Sprintf("Couldn't read %s.", path), hint)
	}
	if !bytes.Contains(data, []byte("-----BEGIN CERTIFICATE-----")) {
		return nil, ui.Usage(fmt.Sprintf("%s isn't a PEM certificate.", path))
	}
	dest := filepath.Join(dir, name)
	before, readErr := os.ReadFile(dest)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return nil, ui.Wrap(err, "Couldn't save the CA certificate.", "Check that you can write to "+dir+", then try again.")
	}
	if err := envfile.WriteFileAtomic(dest, data, false); err != nil {
		return nil, ui.Wrap(err, "Couldn't save the CA certificate.", "Check that you can write to "+dir+", then try again.")
	}
	return func() {
		if readErr != nil {
			os.Remove(dest)
		} else {
			_ = envfile.WriteFileAtomic(dest, before, false)
		}
	}, nil
}
