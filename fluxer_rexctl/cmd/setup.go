// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"io"
	"net/mail"
	"os"
	"regexp"
	"strconv"
	"strings"
	"unicode/utf8"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/setup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type setupFlags struct {
	SignIn       string
	Email        string
	Username     string
	PasswordFile string
	Name         string
	Registration string
	SkipEmail    bool
	SMTPHost     string
	SMTPPort     int
	SMTPTLS      string
	SMTPUsername string
	SMTPPassFile string
	SMTPFrom     string
}

var setupFlagNames = []string{
	"sign-in", "admin-email", "admin-username", "admin-password-file", "name", "registration",
	"skip-email", "smtp-host", "smtp-port", "smtp-tls", "smtp-username", "smtp-password-file", "smtp-from",
}

func (f *setupFlags) bind(c *cobra.Command) {
	fl := c.Flags()
	fl.StringVar(&f.SignIn, "sign-in", "", "how people sign in (email or username)")
	fl.StringVar(&f.Email, "admin-email", "", "email address for your admin account")
	fl.StringVar(&f.Username, "admin-username", "", "username for your admin account")
	fl.StringVar(&f.PasswordFile, "admin-password-file", "", "file with your admin password, or - for stdin")
	fl.StringVar(&f.Name, "name", "", "your instance's name")
	fl.StringVar(&f.Registration, "registration", "", "who can create an account (invite, approval, open or closed)")
	fl.BoolVar(&f.SkipEmail, "skip-email", false, "set up email later")
	fl.StringVar(&f.SMTPHost, "smtp-host", "", "the SMTP server")
	fl.IntVar(&f.SMTPPort, "smtp-port", 587, "the SMTP port")
	fl.StringVar(&f.SMTPTLS, "smtp-tls", "", tlsUsage)
	fl.StringVar(&f.SMTPUsername, "smtp-username", "", "the username for the SMTP server")
	fl.StringVar(&f.SMTPPassFile, "smtp-password-file", "", "file with the SMTP password, or - for stdin")
	fl.StringVar(&f.SMTPFrom, "smtp-from", "", "sender, like Example Chat <noreply@chat.example.com>")
}

func (f *setupFlags) passed(c *cobra.Command) bool {
	return len(f.passedNames(c)) > 0
}

func (f *setupFlags) passedNames(c *cobra.Command) []string {
	var names []string
	for _, name := range setupFlagNames {
		if fl := c.Flags().Lookup(name); fl != nil && fl.Changed {
			names = append(names, "--"+name)
		}
	}
	return names
}

func newSetupCmd(app *App) *cobra.Command {
	var f setupFlags
	c := &cobra.Command{
		Use:   "setup",
		Short: "Create the admin account and finish setup in the terminal",
		Args:  noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			if err := oneStdinFlag(c); err != nil {
				return err
			}
			ctx := c.Context()
			inst, err := app.Instance()
			if err != nil {
				return err
			}
			return app.Locked(ctx, inst, func() error {
				p, err := app.Named(ctx, inst)
				if err != nil {
					return err
				}
				adminID, err := runSetup(ctx, app, c, inst, p, &f, nil)
				if err != nil {
					return err
				}
				if !signInAfterSetup(ctx, app, inst, p, adminID) {
					app.UI.Info("Sign in at %s.", inst.Origin())
				}
				return nil
			})
		},
	}
	f.bind(c)
	c.AddCommand(&cobra.Command{
		Use:   "token",
		Short: "Show the setup token for the browser",
		Args:  noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runSetupToken(c.Context(), app)
		},
	})
	return c
}

func runSetupToken(ctx context.Context, app *App) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	p, err := app.Named(ctx, inst)
	if err != nil {
		return err
	}
	client := app.adminClient(inst, p)
	if done, err := setup.Done(ctx, client); err == nil && done {
		return ui.Refused("Setup is already finished.", fmt.Sprintf("Sign in at %s.", inst.Origin()))
	}
	if admin, _, err := setup.AccountsExist(ctx, client); err == nil && admin {
		return ui.Refused("Your admin account already exists, but setup isn't finished.", browserSetupFix(inst.Origin(), true))
	}
	token := setup.Token(inst.State.Secrets)
	if token == "" {
		err := app.Change(ctx, inst, func(e *engine.Engine) error {
			t, err := setup.EnsureToken(inst.State.Secrets)
			if err != nil {
				return err
			}
			if err := inst.Save(); err != nil {
				return err
			}
			return app.UI.Step(ctx, "Restarting Fluxer with a new setup token", func(ctx context.Context, s *ui.Step) error {
				if _, err := deploy.Apply(ctx, inst, e, deploy.ApplyOptions{Pull: true, OnEvent: app.progress(s, "Restarting")}); err != nil {
					return ui.Wrap(readyFailure(ctx, inst, p, err), "Couldn't restart Fluxer with a new setup token.", "Run rexctl logs api to see why.")
				}
				if err := waitForFluxer(ctx, inst, p, s, "Restarting"); err != nil {
					return err
				}
				token = t
				s.Done("Restarted Fluxer with a new setup token.")
				return nil
			})
		})
		if err != nil {
			return err
		}
	}
	app.UI.Info("Open %s and enter this setup token to create your admin account.", inst.Origin())
	app.UI.Highlight(token)
	return nil
}

func runSetup(ctx context.Context, app *App, c *cobra.Command, inst *instance.Instance, p *engine.Project, f *setupFlags, pre *setup.Answers) (string, error) {
	client := app.adminClient(inst, p)
	done, err := setup.Done(ctx, client)
	if err != nil {
		return "", ui.Wrap(err, "Couldn't connect to your instance.", "Run rexctl status to see whether it's running.")
	}
	if done {
		return "", ui.Refused("Setup is already finished.", fmt.Sprintf("Sign in at %s.", inst.Origin()))
	}
	if admin, exists, err := setup.AccountsExist(ctx, client); err == nil && exists {
		msg := "Fluxer already has accounts, but setup isn't finished."
		if admin {
			msg = "Your admin account already exists, but setup isn't finished."
		}
		return "", ui.Refused(msg, browserSetupFix(inst.Origin(), admin))
	}
	var answers setup.Answers
	if pre != nil {
		answers = *pre
	} else if answers, err = askSetup(app, c, inst.Domain(), f); err != nil {
		return "", err
	}
	if answers.SMTP != nil {
		if answers.SMTP, err = testEmail(ctx, app, client, inst.Domain(), f, answers); err != nil {
			return "", err
		}
	}

	var res *setup.Result
	err = app.UI.Step(ctx, "Creating your admin account", func(ctx context.Context, s *ui.Step) error {
		var err error
		res, err = setup.Run(ctx, client, setup.Token(inst.State.Secrets), inst.Origin(), inst.Dir, answers)
		if err == nil || errors.Is(err, setup.ErrRecoveryKit) {
			s.Done("Created your admin account.")
		}
		return err
	})
	switch {
	case err == nil:
	case errors.Is(err, setup.ErrRecoveryKit):
		app.UI.Warn("Couldn't save your recovery kit.", "Create one in your account settings.")
	case errors.Is(err, setup.ErrAlreadyDone):
		return "", ui.Refused("Setup is already finished.", fmt.Sprintf("Sign in at %s.", inst.Origin()))
	case res != nil:
		return "", ui.Wrap(err, "Couldn't finish setup.", fmt.Sprintf("Open %s and sign in to finish setup in your browser.", inst.Origin()))
	default:
		return "", setupError(err)
	}
	if res != nil && res.RecoveryKitPath != "" {
		app.UI.OK("Saved your recovery kit to %s.", res.RecoveryKitPath)
		app.UI.Info("  Keep a copy somewhere safe. You'll need it if you lose your password.")
	}
	inst.State.SetupDone = true
	if err := inst.Save(); err != nil {
		return "", err
	}
	if res == nil {
		return "", nil
	}
	return res.UserID, nil
}

func signInAfterSetup(ctx context.Context, app *App, inst *instance.Instance, p *engine.Project, adminID string) bool {
	if adminID == "" {
		return false
	}
	if err := printSignInLink(ctx, app, app.adminClient(inst, p), adminID, "you"); err != nil {
		app.UI.Detail("%v", err)
		return false
	}
	return true
}

func setupError(err error) error {
	var apiErr *adminapi.APIError
	if errors.As(err, &apiErr) && (apiErr.Message != "" || len(apiErr.Fields) > 0) {
		msg := strings.TrimSpace(apiErr.Error())
		if !strings.HasSuffix(msg, ".") {
			msg += "."
		}
		return ui.Wrap(err, msg, "Run rexctl setup to try again.")
	}
	return ui.Wrap(err, "Couldn't create your admin account.", "Run rexctl setup to try again.")
}

func askSetup(app *App, c *cobra.Command, domain string, f *setupFlags) (setup.Answers, error) {
	var a setup.Answers
	u := app.UI
	if err := u.Ask(c, ui.Field{
		Flag:  "sign-in",
		Title: "Will people sign in with an email address or a username?",
		Value: &f.SignIn,
		Options: []ui.Option{
			{Label: "Email address", Value: string(setup.SignInEmail)},
			{Label: "Username", Value: string(setup.SignInUsername)},
		},
	}); err != nil {
		return a, err
	}
	a.SignIn = setup.SignIn(f.SignIn)

	if a.SignIn == setup.SignInEmail {
		smtp, err := askEmail(app, c, domain, f)
		if err != nil {
			return a, err
		}
		a.SMTP = smtp
	} else if fl := smtpFlagPassed(c); fl != "" {
		return a, ui.Usage(fmt.Sprintf("Email only works when people sign in with an email address. Leave out --%s, or pass --sign-in email.", fl))
	}

	if a.SignIn == setup.SignInEmail {
		if err := u.Ask(c, ui.Field{Flag: "admin-email", Title: "Your admin email address", Value: &f.Email, Validate: validateSetupEmail}); err != nil {
			return a, err
		}
	}
	if err := u.Ask(c, ui.Field{Flag: "admin-username", Title: "Your admin username", Value: &f.Username, Validate: validateSetupUsername}); err != nil {
		return a, err
	}
	password, err := askSecret(app, c, "admin-password-file", "Your admin password", f.PasswordFile, validateSetupPassword)
	if err != nil {
		return a, err
	}
	if err := u.Ask(c, ui.Field{Flag: "name", Title: "What's your instance called?", Value: &f.Name, Validate: validateSetupName}); err != nil {
		return a, err
	}
	if err := u.Ask(c, ui.Field{
		Flag:    "registration",
		Title:   "Who can create an account?",
		Value:   &f.Registration,
		Default: string(setup.RegistrationInvite),
		Options: registrationOptions(f.Registration),
	}); err != nil {
		return a, err
	}
	a.Email = strings.TrimSpace(f.Email)
	a.Username = strings.TrimSpace(f.Username)
	a.Password = password
	a.Name = strings.TrimSpace(f.Name)
	a.Registration = setup.Registration(f.Registration)
	return a, nil
}

func smtpFlagPassed(c *cobra.Command) string {
	if c == nil {
		return ""
	}
	for _, name := range []string{"smtp-host", "smtp-port", "smtp-tls", "smtp-username", "smtp-password-file", "smtp-from"} {
		if fl := c.Flags().Lookup(name); fl != nil && fl.Changed {
			return name
		}
	}
	return ""
}

func registrationOptions(current string) []ui.Option {
	opts := []ui.Option{
		{Label: "People with an invite link", Value: string(setup.RegistrationInvite)},
		{Label: "Anyone, after you approve them", Value: string(setup.RegistrationApproval)},
		{Label: "Anyone", Value: string(setup.RegistrationOpen)},
	}
	if current == string(setup.RegistrationClosed) {
		opts = append(opts, ui.Option{Label: "Nobody", Value: string(setup.RegistrationClosed)})
	}
	return opts
}

func askEmail(app *App, c *cobra.Command, domain string, f *setupFlags) (*setup.SMTP, error) {
	u := app.UI
	if f.SkipEmail {
		return nil, nil
	}
	if f.SMTPHost == "" {
		if !u.Interactive() {
			return nil, ui.Usage("Pass --smtp-host, or --skip-email to set up email later.")
		}
		choice := "yes"
		if err := u.Ask(nil, ui.Field{
			Title: "Can your instance send email?",
			Value: &choice,
			Options: []ui.Option{
				{Label: "Yes", Value: "yes"},
				{Label: "Later", Value: "later"},
			},
		}); err != nil {
			return nil, err
		}
		if choice == "later" {
			return nil, nil
		}
	}
	return askSMTP(app, c, domain, f)
}

func askSMTP(app *App, c *cobra.Command, domain string, f *setupFlags) (*setup.SMTP, error) {
	u := app.UI
	port := strconv.Itoa(f.SMTPPort)
	if f.SMTPFrom == "" && domain != "" {
		f.SMTPFrom = "noreply@" + domain
	}
	fields := []ui.Field{
		{Flag: "smtp-host", Title: "SMTP server", Value: &f.SMTPHost, Validate: validateSetupHost},
		{Flag: "smtp-port", Title: "Port", Value: &port, Validate: validateSetupPort},
	}
	if u.Interactive() {
		fields = append(fields, ui.Field{Flag: "smtp-username", Title: "SMTP username", Value: &f.SMTPUsername, Optional: true})
	}
	if err := u.Ask(c, fields...); err != nil {
		return nil, err
	}
	password := ""
	if f.SMTPPassFile != "" || (u.Interactive() && strings.TrimSpace(f.SMTPUsername) != "") {
		var err error
		password, err = askSecret(app, c, "smtp-password-file", "SMTP password", f.SMTPPassFile, nil)
		if err != nil {
			return nil, err
		}
	}
	if err := u.Ask(c, ui.Field{Flag: "smtp-from", Title: "Send from", Value: &f.SMTPFrom, Validate: validateSetupFrom}); err != nil {
		return nil, err
	}
	n, _ := strconv.Atoi(port)
	f.SMTPPort = n
	if err := validTLS(f.SMTPTLS, "smtp-tls"); err != nil {
		return nil, err
	}
	return &setup.SMTP{
		Host:     strings.TrimSpace(f.SMTPHost),
		Port:     n,
		TLS:      f.SMTPTLS,
		Username: strings.TrimSpace(f.SMTPUsername),
		Password: password,
		From:     strings.TrimSpace(f.SMTPFrom),
	}, nil
}

func testEmail(ctx context.Context, app *App, client *adminapi.Client, domain string, f *setupFlags, a setup.Answers) (*setup.SMTP, error) {
	smtp := a.SMTP
	to := ""
	if a.SignIn == setup.SignInEmail {
		to = a.Email
	}
	for {
		err := app.UI.Step(ctx, "Checking your email settings", func(ctx context.Context, s *ui.Step) error {
			if err := setup.TestSMTP(ctx, client, *smtp, a.Name, to); err != nil {
				return err
			}
			if to != "" {
				s.Done("Sent a test email to %s.", to)
			} else {
				s.Done("Connected to %s.", smtp.Host)
			}
			return nil
		})
		if err == nil {
			return smtp, nil
		}
		var se *setup.SMTPError
		if !errors.As(err, &se) {
			return nil, ui.Wrap(err, "Couldn't check your email settings.", "Run rexctl setup to try again.")
		}
		msg := smtpReason(se.Message)
		plain := (adminapi.StartTLSFailed(se.Message) || certificateFailed(se.Message)) && smtp.TLS != "none"
		if !app.UI.Interactive() {
			fix := "Run rexctl email set after setup to try again."
			if plain {
				fix = strings.TrimSuffix(tlsFix(se.Message, ""), ".") + " after setup."
			}
			app.UI.Warn("Couldn't send email. "+msg, fix)
			return nil, nil
		}
		app.UI.Fail("Couldn't send email. "+msg, "")
		choice := "retry"
		opts := []ui.Option{
			{Label: "Change the email settings", Value: "retry"},
			{Label: "Set up email later", Value: "later"},
		}
		if plain {
			opts = append([]ui.Option{{Label: "Try again without encryption", Value: "plain"}}, opts...)
			choice = "plain"
		}
		if err := app.UI.Ask(nil, ui.Field{
			Title:   "What do you want to do?",
			Value:   &choice,
			Options: opts,
		}); err != nil {
			return nil, err
		}
		switch choice {
		case "later":
			return nil, nil
		case "plain":
			smtp.TLS = "none"
			f.SMTPTLS = "none"
			continue
		}
		f.SMTPPassFile = ""
		if smtp, err = askSMTP(app, nil, domain, f); err != nil {
			return nil, err
		}
	}
}

func askSecret(app *App, c *cobra.Command, flag, title, file string, validate func(string) error) (string, error) {
	if file != "" {
		v, err := readSetupSecret(app, file)
		if err != nil {
			return "", ui.Wrap(err, fmt.Sprintf("Couldn't read %s.", file), fmt.Sprintf("Check the path you passed to --%s.", flag))
		}
		if validate != nil {
			if err := validate(v); err != nil {
				return "", ui.Usage(err.Error())
			}
		}
		return v, nil
	}
	if !app.UI.Interactive() {
		return "", ui.MissingFlag(flag)
	}
	var v string
	if err := app.UI.Ask(nil, ui.Field{Title: title, Value: &v, Secret: true, Validate: validate}); err != nil {
		return "", err
	}
	return v, nil
}

func oneStdinFlag(c *cobra.Command) error {
	var names []string
	stdin := 0
	for _, name := range []string{"storage-secret-key-file", "admin-password-file", "smtp-password-file"} {
		fl := c.Flags().Lookup(name)
		if fl == nil {
			continue
		}
		names = append(names, "--"+name)
		if fl.Value.String() == "-" {
			stdin++
		}
	}
	if stdin > 1 {
		return ui.Usage(fmt.Sprintf("Only one of %s can read from stdin.", ui.JoinAnd(names)))
	}
	return nil
}

func readSetupSecret(app *App, file string) (string, error) {
	var data []byte
	var err error
	if file == "-" {
		data, err = io.ReadAll(io.LimitReader(app.UI.Stdin(), 64*1024))
	} else {
		data, err = os.ReadFile(file)
	}
	if err != nil {
		return "", err
	}
	return strings.TrimRight(string(data), "\r\n"), nil
}

var (
	setupUsernamePattern = regexp.MustCompile(`^[A-Za-z0-9_.]{1,32}$`)
	setupHostPattern     = regexp.MustCompile(`^[A-Za-z0-9]([A-Za-z0-9-]{0,61}[A-Za-z0-9])?(\.[A-Za-z0-9]([A-Za-z0-9-]{0,61}[A-Za-z0-9])?)*$`)
)

type setupInputError string

func (e setupInputError) Error() string { return string(e) }

func validateSetupEmail(v string) error {
	addr, err := mail.ParseAddress(strings.TrimSpace(v))
	if err != nil || addr.Name != "" || !strings.Contains(addr.Address[strings.LastIndex(addr.Address, "@")+1:], ".") {
		return setupInputError("Enter an email address, like you@example.com.")
	}
	return nil
}

func validateSetupUsername(v string) error {
	if !setupUsernamePattern.MatchString(strings.TrimSpace(v)) {
		return setupInputError("Use up to 32 letters, numbers, dots or underscores.")
	}
	return nil
}

func validateSetupPassword(v string) error {
	n := utf8.RuneCountInString(v)
	if n < 8 || n > 256 {
		return setupInputError("Use at least 8 characters.")
	}
	return nil
}

func validateSetupName(v string) error {
	n := utf8.RuneCountInString(strings.TrimSpace(v))
	if n < 1 || n > 100 {
		return setupInputError("Use between 1 and 100 characters.")
	}
	return nil
}

func validateSetupHost(v string) error {
	if !setupHostPattern.MatchString(strings.TrimSpace(v)) {
		return setupInputError("Enter a server name, like smtp.example.com.")
	}
	return nil
}

func validateSetupPort(v string) error {
	n, err := strconv.Atoi(strings.TrimSpace(v))
	if err != nil || n < 1 || n > 65535 {
		return setupInputError("Enter a port number, like 587.")
	}
	return nil
}

func validateSetupFrom(v string) error {
	if _, err := mail.ParseAddress(strings.TrimSpace(v)); err != nil {
		return setupInputError("Enter an address, like Example Chat <noreply@chat.example.com>.")
	}
	return nil
}

func browserSetupFix(origin string, admin bool) string {
	if admin {
		return fmt.Sprintf("Open %s and sign in as your admin to finish setup in your browser.", origin)
	}
	return fmt.Sprintf("Run rexctl setup token and enter the token at %s to finish setup in your browser.", origin)
}
