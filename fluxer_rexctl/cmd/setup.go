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
	"time"
	"unicode/utf8"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
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
	SMTPPort     int
	SMTP         smtpForm
}

type smtpForm struct {
	prefix   string
	Host     string
	Port     string
	TLS      string
	Username string
	Password string
	PassFile string
	From     string
}

func (m *smtpForm) flag(name string) string { return m.prefix + name }

func (m *smtpForm) fields(hide func() bool) []ui.Field {
	return []ui.Field{
		{Flag: m.flag("host"), FlagLabel: "--" + m.flag("host"), Title: "SMTP server", Placeholder: "smtp.example.com", Value: &m.Host, Validate: validateSetupHost, Hide: hide},
		{Flag: m.flag("port"), FlagLabel: "--" + m.flag("port"), Title: "Port", Value: &m.Port, Default: "587", Validate: validateSetupPort, Hide: hide},
		{Flag: m.flag("username"), Title: "SMTP username", Value: &m.Username, Optional: true, Hide: hide},
		{Title: "SMTP password", Value: &m.Password, Secret: true, Optional: true, Hide: func() bool {
			return hide() || strings.TrimSpace(m.Username) == "" || m.PassFile != ""
		}},
		{Flag: m.flag("from"), FlagLabel: "--" + m.flag("from"), Title: "Send from", Value: &m.From, Validate: validateSetupFrom, Hide: hide},
	}
}

func (m *smtpForm) load(app *App, port int, domain string) error {
	m.Port = strconv.Itoa(port)
	if _, ip := settings.DomainIP(domain); m.From == "" && domain != "" && !ip {
		m.From = "noreply@" + settings.MailDomain(domain)
	}
	if err := validTLS(m.TLS, m.flag("tls")); err != nil {
		return err
	}
	if m.PassFile == "" {
		return nil
	}
	v, err := askSecret(app, nil, m.flag("password-file"), "SMTP password", m.PassFile, nil)
	if err != nil {
		return err
	}
	m.Password = v
	return nil
}

func (m *smtpForm) value() *setup.SMTP {
	n, _ := strconv.Atoi(strings.TrimSpace(m.Port))
	return &setup.SMTP{
		Host:     strings.TrimSpace(m.Host),
		Port:     n,
		TLS:      m.TLS,
		Username: strings.TrimSpace(m.Username),
		Password: m.Password,
		From:     strings.TrimSpace(m.From),
	}
}

var setupFlagNames = []string{
	"sign-in", "admin-email", "admin-username", "admin-password-file", "name", "registration",
	"skip-email", "smtp-host", "smtp-port", "smtp-tls", "smtp-username", "smtp-password-file", "smtp-from",
}

func (f *setupFlags) bind(c *cobra.Command) {
	fl := c.Flags()
	fl.StringVar(&f.SignIn, "sign-in", "", "how people sign in (email or username), username by default on an IP address or private name")
	fl.StringVar(&f.Email, "admin-email", "", "email address for your admin account")
	fl.StringVar(&f.Username, "admin-username", "", "username for your admin account")
	fl.StringVar(&f.PasswordFile, "admin-password-file", "", "file with your admin password, or - for stdin")
	fl.StringVar(&f.Name, "name", "", "your instance's name")
	fl.StringVar(&f.Registration, "registration", "", "who can create an account (invite, approval, open or closed)")
	fl.BoolVar(&f.SkipEmail, "skip-email", false, "set up email later")
	f.SMTP.prefix = "smtp-"
	fl.StringVar(&f.SMTP.Host, "smtp-host", "", "the SMTP server")
	fl.IntVar(&f.SMTPPort, "smtp-port", 587, "the SMTP port")
	fl.StringVar(&f.SMTP.TLS, "smtp-tls", "", tlsUsage)
	fl.StringVar(&f.SMTP.Username, "smtp-username", "", "the username for the SMTP server")
	fl.StringVar(&f.SMTP.PassFile, "smtp-password-file", "", "file with the SMTP password, or - for stdin")
	fl.StringVar(&f.SMTP.From, "smtp-from", "", "sender, like Example Chat <noreply@chat.example.com>")
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
		Short: "Create the admin and finish setup in the terminal",
		Example: "rexctl setup\n" +
			"rexctl setup --sign-in username --admin-username admin --admin-password-file pw.txt --name \"Example Chat\"\n" +
			"rexctl setup token",
		Args: noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			if err := oneStdinFlag(c); err != nil {
				return err
			}
			return runSetupCommand(c.Context(), app, c, &f)
		},
	}
	f.bind(c)
	c.AddCommand(&cobra.Command{
		Use:     "token",
		Short:   "Show the setup token for the browser",
		Example: "rexctl setup token",
		Args:    noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runSetupToken(c.Context(), app)
		},
	})
	return c
}

func setupClient(ctx context.Context, app *App, inst *instance.Instance) (*adminapi.Client, rt.Runtime, error) {
	r, err := app.Runtime(ctx, inst)
	if err != nil {
		return nil, nil, err
	}
	t, err := r.Admin(ctx)
	if err != nil {
		return nil, nil, err
	}
	return adminapi.New(t, app.Version), r, nil
}

func runSetupCommand(ctx context.Context, app *App, c *cobra.Command, f *setupFlags) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	return app.Locked(ctx, inst, func() error {
		client, _, err := setupClient(ctx, app, inst)
		if err != nil {
			return err
		}
		if err := setupReady(ctx, client, inst); err != nil {
			return err
		}
		var st setupState
		if err := st.load(app, f, inst.Domain()); err != nil {
			return err
		}
		if !app.UI.Interactive() {
			if err := st.missing(c); err != nil {
				return err
			}
		}
		stage := &ui.Stage{ID: "admin", Title: "Admin", Key: 'u', Fields: st.fields(c, func() bool { return false })}
		stage.Summary = func() string { return st.summary() }
		if err := app.UI.RunFlow(ctx, c, &ui.Flow{Title: "rexctl setup", Subtitle: inst.Domain(), Stages: []*ui.Stage{stage}}); err != nil {
			return err
		}
		answers := st.answers()
		adminID, err := runSetup(ctx, app, client, inst, &st, &answers)
		if err != nil {
			return err
		}
		if !signInAfterSetup(ctx, app, client, adminID) {
			app.UI.Info("Sign in at %s.", inst.Origin())
		}
		firstInvite(ctx, app, client, answers)
		return nil
	})
}

func setupReady(ctx context.Context, client *adminapi.Client, inst *instance.Instance) error {
	done, err := setup.Done(ctx, client)
	if err != nil {
		return ui.Wrap(err, "Couldn't connect to your instance.", "Run rexctl status to see whether it's running.")
	}
	if done {
		return ui.Refused("Setup is already finished.", fmt.Sprintf("Sign in at %s.", inst.Origin()))
	}
	if exists, err := setup.AccountsExist(ctx, client); err == nil && exists {
		return ui.Refused("Fluxer already has accounts, but setup isn't finished.", browserSetupFix(inst.Origin()))
	}
	return nil
}

func runSetupToken(ctx context.Context, app *App) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	client, r, err := setupClient(ctx, app, inst)
	if err != nil {
		return err
	}
	if done, err := setup.Done(ctx, client); err == nil && done {
		return ui.Refused("Setup is already finished.", fmt.Sprintf("Sign in at %s.", inst.Origin()))
	}
	token := setup.Token(inst.State.Secrets)
	if token == "" {
		if token, err = newSetupToken(ctx, app, inst, r); err != nil {
			return err
		}
	}
	app.UI.Info("Open %s and enter this setup token to create your admin account.", inst.Origin())
	app.UI.Highlight(token)
	return nil
}

func newSetupToken(ctx context.Context, app *App, inst *instance.Instance, r rt.Runtime) (string, error) {
	if r.Info(ctx).Root && euid() != 0 {
		return "", ui.Refused("A new setup token needs a restart, and this instance is managed as root.", "Run sudo rexctl setup token.")
	}
	app.UI.Info("A new setup token needs a restart. %s", restartLine(r.Kind(), []string{"all"}))
	ok, err := app.UI.Confirm("Go ahead?", true)
	if err != nil {
		return "", err
	}
	if !ok {
		app.UI.Info("Nothing changed.")
		return "", errDeclined
	}
	var token string
	err = app.Locked(ctx, inst, func() error {
		t, err := setup.EnsureToken(inst.State.Secrets)
		if err != nil {
			return err
		}
		if err := inst.Save(); err != nil {
			return ui.Wrap(err, fmt.Sprintf("Couldn't save your settings in %s.", inst.Dir), "Check that you can write to it, then try again.")
		}
		return app.UI.Step(ctx, "Restarting Fluxer with a new setup token", func(ctx context.Context, s *ui.Step) error {
			if err := r.Restart(ctx, s, nil); err != nil {
				return ui.Wrap(err, "Couldn't restart Fluxer with a new setup token.", "Run rexctl logs api to see why.")
			}
			token = t
			s.Done("Restarted Fluxer with a new setup token.")
			return nil
		})
	})
	return token, err
}

func runSetup(ctx context.Context, app *App, client *adminapi.Client, inst *instance.Instance, st *setupState, pre *setup.Answers) (string, error) {
	if err := setupReady(ctx, client, inst); err != nil {
		return "", err
	}
	var err error
	if pre == nil {
		return "", ui.Failed("rexctl has no setup answers to use.", "Run rexctl setup.")
	}
	answers := *pre
	if answers.SMTP != nil {
		if answers.SMTP, err = testEmail(ctx, app, client, st, answers); err != nil {
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
		app.UI.OK("Saved your recovery kit to %s. Keep a copy somewhere safe. You'll need it if you lose your password.", res.RecoveryKitPath)
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

func signInAfterSetup(ctx context.Context, app *App, client *adminapi.Client, adminID string) bool {
	if adminID == "" {
		return false
	}
	if err := printSignInLink(ctx, app, client, adminID, "you"); err != nil {
		app.UI.Detail("%v", err)
		return false
	}
	return true
}

func firstInvite(ctx context.Context, app *App, client *adminapi.Client, answers setup.Answers) {
	if answers.Registration != setup.RegistrationInvite {
		return
	}
	if err := createInvite(ctx, app, client, adminapi.InviteOptions{Expires: 7 * 24 * time.Hour}); err != nil {
		app.UI.Warn("Couldn't create an invite link.", "Run rexctl invites create to make one.")
	}
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

func testEmail(ctx context.Context, app *App, client *adminapi.Client, st *setupState, a setup.Answers) (*setup.SMTP, error) {
	smtp := a.SMTP
	to := ""
	if a.SignIn == setup.SignInEmail {
		to = a.Email
	}
	for {
		err := app.UI.Check(ctx, "Checking your email settings", func(ctx context.Context, s *ui.Step) error {
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
			st.smtp.TLS = "none"
			continue
		}
		st.smtp.PassFile = ""
		if err := app.UI.Ask(nil, st.smtp.fields(func() bool { return false })...); err != nil {
			return nil, err
		}
		smtp = st.smtp.value()
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
		return setupInputError("Enter a username of up to 32 letters, numbers, dots or underscores.")
	}
	return nil
}

func validateSetupPassword(v string) error {
	n := utf8.RuneCountInString(v)
	if n < 8 || n > 256 {
		return setupInputError("Enter a password of at least 8 characters.")
	}
	return nil
}

func validateSetupName(v string) error {
	switch n := utf8.RuneCountInString(strings.TrimSpace(v)); {
	case n == 0:
		return setupInputError("Enter the name people see in the app.")
	case n > 100:
		return setupInputError("Use 100 characters or fewer.")
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

func browserSetupFix(origin string) string {
	return fmt.Sprintf("Run rexctl setup token and enter the token at %s to finish setup in your browser.", origin)
}
