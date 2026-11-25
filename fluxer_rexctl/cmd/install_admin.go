// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"
	"strings"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/setup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	emailNow   = "yes"
	emailLater = "later"
)

type setupState struct {
	f             *setupFlags
	smtp          *smtpForm
	email         string
	password      string
	usernameFirst bool
}

func usernameDomain(domain string) bool {
	_, ip := settings.DomainIP(domain)
	return ip || domain != "" && checks.PrivateDomain(domain)
}

func (s *setupState) load(app *App, f *setupFlags, domain string) error {
	s.f = f
	s.smtp = &f.SMTP
	s.usernameFirst = usernameDomain(domain)
	s.email = emailNow
	if f.SkipEmail {
		s.email = emailLater
	}
	if f.PasswordFile != "" {
		v, err := askSecret(app, nil, "admin-password-file", "What password do you want for your admin account?", f.PasswordFile, validateSetupPassword)
		if err != nil {
			return err
		}
		s.password = v
	}
	return s.smtp.load(app, f.SMTPPort, domain)
}

func (s *setupState) missing(c *cobra.Command) error {
	f := s.f
	var missing []string
	if f.SignIn == "" && s.usernameFirst {
		f.SignIn = string(setup.SignInUsername)
	}
	if f.SignIn == "" {
		missing = append(missing, "--sign-in email or --sign-in username")
	}
	if f.SignIn == string(setup.SignInEmail) && f.Email == "" {
		missing = append(missing, "--admin-email")
	}
	if f.Username == "" {
		missing = append(missing, "--admin-username")
	}
	if f.PasswordFile == "" {
		missing = append(missing, "--admin-password-file")
	}
	if f.Name == "" {
		missing = append(missing, "--name")
	}
	if f.SignIn == string(setup.SignInEmail) && !f.SkipEmail && s.smtp.Host == "" {
		missing = append(missing, "--smtp-host or --skip-email")
	}
	if f.SignIn == string(setup.SignInUsername) {
		if fl := smtpFlagPassed(c); fl != "" {
			return ui.Usage(fmt.Sprintf("Email only works when people sign in with an email address. Leave out --%s, or pass --sign-in email.", fl))
		}
	}
	if len(missing) == 0 {
		return nil
	}
	return ui.Usage("Pass " + ui.JoinAnd(missing) + ".")
}

func (s *setupState) signInDefault() string {
	if s.usernameFirst {
		return string(setup.SignInUsername)
	}
	return ""
}

func (s *setupState) wantsEmail() bool {
	return s.f.SignIn == string(setup.SignInEmail) && !s.f.SkipEmail && s.email == emailNow
}

func (s *setupState) fields(c *cobra.Command, hide func() bool) []ui.Field {
	f := s.f
	byEmail := func() bool { return hide() || f.SignIn != string(setup.SignInEmail) }
	noSMTP := func() bool { return hide() || !s.wantsEmail() }
	fields := []ui.Field{
		{
			Flag:      "sign-in",
			FlagLabel: "--sign-in",
			Title:     "Will people sign in with an email address or a username?",
			DescriptionFunc: func() string {
				if s.usernameFirst {
					return "This can't change later. Mail sent from an IP address or a private name rarely gets delivered, so Username is likely."
				}
				return "This can't change later."
			},
			Value:   &f.SignIn,
			Default: s.signInDefault(),
			Options: []ui.Option{
				{Label: "Email address", Value: string(setup.SignInEmail)},
				{Label: "Username", Value: string(setup.SignInUsername)},
			},
			Hide: hide,
		},
		{
			Title: "Can your instance send email?",
			Value: &s.email,
			Options: []ui.Option{
				{Label: "Yes", Value: emailNow},
				{Label: "Later", Value: emailLater, Hint: "set it up with rexctl email set"},
			},
			Hide: func() bool { return byEmail() || f.SkipEmail || flagPassed(c, "smtp-host") },
		},
	}
	fields = append(fields, s.smtp.fields(noSMTP)...)
	return append(fields, []ui.Field{
		{Flag: "admin-email", FlagLabel: "--admin-email", Title: "What email address will you sign in with?", Description: "This becomes your admin account.", Value: &f.Email, Validate: validateSetupEmail, Hide: byEmail},
		{Flag: "admin-username", FlagLabel: "--admin-username", Title: "What username do you want?", DescriptionFunc: func() string {
			if f.SignIn == string(setup.SignInUsername) {
				return "You sign in with it, and it becomes your admin account. Letters, numbers, dots and underscores."
			}
			return "This becomes your admin account. Letters, numbers, dots and underscores."
		}, Value: &f.Username, Validate: validateSetupUsername, Hide: hide},
		{Title: "What password do you want for your admin account?", DescriptionFunc: func() string {
			if f.SignIn == string(setup.SignInUsername) {
				return "At least 8 characters. rexctl saves a recovery kit in case you lose it."
			}
			return "At least 8 characters."
		}, Value: &s.password, Secret: true, Validate: validateSetupPassword, Hide: func() bool { return hide() || f.PasswordFile != "" }},
		{Flag: "name", FlagLabel: "--name", Title: "What's your instance called?", Description: "People see this name in the app.", Value: &f.Name, Validate: validateSetupName, Hide: hide},
		{
			Flag:      "registration",
			FlagLabel: "--registration",
			Title:     "Who can create an account?",
			Value:     &f.Registration,
			Default:   string(setup.RegistrationInvite),
			Options:   registrationOptions(f.Registration),
			Hide:      hide,
		},
	}...)
}

func flagPassed(c *cobra.Command, name string) bool {
	if c == nil {
		return false
	}
	fl := c.Flags().Lookup(name)
	return fl != nil && fl.Changed
}

func (s *setupState) answers() setup.Answers {
	f := s.f
	a := setup.Answers{
		SignIn:       setup.SignIn(f.SignIn),
		Email:        strings.TrimSpace(f.Email),
		Username:     strings.TrimSpace(f.Username),
		Password:     s.password,
		Name:         strings.TrimSpace(f.Name),
		Registration: setup.Registration(f.Registration),
	}
	if a.SignIn != setup.SignInEmail {
		a.Email = ""
	}
	if s.wantsEmail() {
		a.SMTP = s.smtp.value()
	}
	return a
}

func (s *setupState) summary() string {
	f := s.f
	who := strings.TrimSpace(f.Username)
	if f.SignIn == string(setup.SignInEmail) && f.Email != "" {
		who = strings.TrimSpace(f.Email)
	}
	if s.wantsEmail() {
		return who + ", email through " + strings.TrimSpace(s.smtp.Host)
	}
	if f.SignIn == string(setup.SignInEmail) {
		return who + ", email later"
	}
	return who
}

func (in *installer) adminFields() []ui.Field {
	notHere := func() bool { return in.f.SetupWhere != "terminal" }
	setupWhere := ui.Field{
		Flag:      "setup",
		FlagLabel: "--setup",
		Title:     "Where do you want to create your admin account?",
		Value:     &in.f.SetupWhere,
		Options: []ui.Option{
			{Label: "Here in the terminal", Value: "terminal"},
			{Label: "In my browser", Value: "browser", Hint: "with a setup token rexctl shows at the end"},
		},
	}
	if in.admin.f == nil {
		return []ui.Field{setupWhere}
	}
	in.admin.usernameFirst = usernameDomain(in.f.Domain)
	if _, ip := settings.DomainIP(in.f.Domain); in.admin.smtp.From == "" && in.f.Domain != "" && !ip {
		in.admin.smtp.From = "noreply@" + settings.MailDomain(in.f.Domain)
	}
	return append([]ui.Field{setupWhere}, in.admin.fields(in.c, notHere)...)
}

func (in *installer) adminChecks(ctx context.Context, c *ui.Checklist) error {
	in.planFor("admin")
	if in.adminStage != nil {
		in.adminStage.Fields = in.adminFields()
	}
	return in.checkStorage(ctx, c)
}

func (in *installer) adminSummary() string {
	if in.f.SetupWhere != "terminal" {
		in.answers = nil
		return "In your browser with a setup token"
	}
	a := in.admin.answers()
	in.answers = &a
	return in.admin.summary() + ", set up here in the terminal"
}
