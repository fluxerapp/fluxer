// SPDX-License-Identifier: AGPL-3.0-or-later

package setup

import (
	"context"
	"errors"
	"fmt"
	"net/http"
	"net/mail"
	"path/filepath"
	"regexp"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
)

const TokenKey = "FLUXER_SETUP_TOKEN"

type SignIn string

const (
	SignInEmail    SignIn = "email"
	SignInUsername SignIn = "username"
)

type Registration string

const (
	RegistrationOpen     Registration = "open"
	RegistrationApproval Registration = "approval"
	RegistrationInvite   Registration = "invite"
	RegistrationClosed   Registration = "closed"
)

type SMTP struct {
	Host     string
	Port     int
	TLS      string
	Username string
	Password string
	From     string
}

type Answers struct {
	SignIn       SignIn
	Email        string
	Username     string
	Password     string
	Name         string
	Registration Registration
	SMTP         *SMTP
}

type Result struct {
	UserID          string
	RecoveryKitPath string
	TestEmailSentTo string
}

var (
	ErrNoToken       = errors.New("setup: no setup token")
	ErrAlreadyDone   = errors.New("setup: already finished")
	ErrPendingSignup = errors.New("setup: the account is waiting for approval")
	ErrRecoveryKit   = errors.New("setup: couldn't save the recovery kit")
)

type SMTPError struct {
	Message string
}

func (e *SMTPError) Error() string { return e.Message }

func Token(values map[string]string) string {
	if t := strings.TrimSpace(values[TokenKey]); t != "CHANGE_ME" {
		return t
	}
	return ""
}

func EnsureToken(values map[string]string) (string, error) {
	if t := Token(values); t != "" {
		return t, nil
	}
	t, err := secrets.Hex(16)
	if err != nil {
		return "", err
	}
	values[TokenKey] = t
	return t, nil
}

type discovery struct {
	AppPublic struct {
		Setup struct {
			Configured bool   `json:"configured"`
			LockReason string `json:"account_identity_lock_reason"`
		} `json:"setup"`
	} `json:"app_public"`
}

func Done(ctx context.Context, c *adminapi.Client) (bool, error) {
	var d discovery
	if err := c.Loopback(ctx, http.MethodGet, "/.well-known/fluxer", nil, nil, &d); err != nil {
		return false, err
	}
	return d.AppPublic.Setup.Configured, nil
}

func AccountsExist(ctx context.Context, c *adminapi.Client) (admin bool, exists bool, err error) {
	var d discovery
	if err := c.Loopback(ctx, http.MethodGet, "/.well-known/fluxer", nil, nil, &d); err != nil {
		return false, false, err
	}
	switch d.AppPublic.Setup.LockReason {
	case "admin_exists":
		return true, true, nil
	case "accounts_exist", "existing_instance":
		return false, true, nil
	}
	return false, false, nil
}

type registerResponse struct {
	Token  string `json:"token"`
	UserID string `json:"user_id"`
}

type recoveryKitResponse struct {
	RecoveryKey string `json:"recovery_key"`
	CreatedAt   string `json:"created_at"`
}

func Run(ctx context.Context, c *adminapi.Client, token, origin, dir string, a Answers) (*Result, error) {
	token = strings.TrimSpace(token)
	if token == "" {
		return nil, ErrNoToken
	}
	patch := map[string]any{
		"app_public":   map[string]any{"branding": map[string]any{"product_name": a.Name}},
		"registration": registrationPatch(a.Registration),
	}
	if a.SMTP != nil {
		email, err := emailPatch(*a.SMTP, a.Name)
		if err != nil {
			return nil, err
		}
		patch["integrations"] = map[string]any{"email": email}
	}
	done, err := Done(ctx, c)
	if err != nil {
		return nil, err
	}
	if done {
		return nil, ErrAlreadyDone
	}
	if err := c.Loopback(ctx, http.MethodPost, "/instance/setup/token-checks", nil, map[string]any{"setup_token": token}, nil); err != nil {
		return nil, err
	}
	identity := map[string]any{"mode": string(a.SignIn), "setup_token": token}
	if err := c.Loopback(ctx, http.MethodPut, "/instance/setup/account-identity", nil, identity, nil); err != nil {
		return nil, err
	}
	account := map[string]any{"username": a.Username, "password": a.Password, "setup_token": token}
	if a.SignIn == SignInEmail {
		account["email"] = a.Email
	}
	var reg registerResponse
	if err := register(ctx, c, account, &reg); err != nil {
		return nil, err
	}
	if reg.Token == "" {
		return nil, ErrPendingSignup
	}
	auth := map[string]string{"Authorization": reg.Token}
	res := &Result{UserID: reg.UserID}

	if err := c.Call(ctx, http.MethodPatch, "/admin/instance/config", nil, patch, nil); err != nil {
		return res, err
	}

	var kitErr error
	if a.SignIn == SignInUsername {
		res.RecoveryKitPath, kitErr = saveRecoveryKit(ctx, c, auth, origin, dir, a)
	}

	finish := map[string]any{"app_public": map[string]any{"setup": map[string]any{"configured": true}}}
	if err := c.Call(ctx, http.MethodPatch, "/admin/instance/config", nil, finish, nil); err != nil {
		return res, err
	}
	if kitErr != nil {
		return res, fmt.Errorf("%w: %w", ErrRecoveryKit, kitErr)
	}
	return res, nil
}

func registrationPatch(r Registration) map[string]any {
	switch r {
	case RegistrationOpen:
		return map[string]any{"mode": "open", "admin_registration_urls_enabled": true}
	case RegistrationApproval:
		return map[string]any{"mode": "approval", "admin_registration_urls_enabled": true}
	case RegistrationClosed:
		return map[string]any{"mode": "closed", "admin_registration_urls_enabled": false}
	default:
		return map[string]any{"mode": "closed", "admin_registration_urls_enabled": true}
	}
}

func tlsMode(s SMTP) string {
	if s.TLS != "" {
		return s.TLS
	}
	return adminapi.TLSModeFor(s.Port)
}

func emailPatch(s SMTP, name string) (map[string]any, error) {
	addr, err := mail.ParseAddress(s.From)
	if err != nil {
		return nil, err
	}
	fromName := addr.Name
	if fromName == "" {
		fromName = name
	}
	return map[string]any{
		"enabled":    true,
		"provider":   "smtp",
		"from_email": addr.Address,
		"from_name":  fromName,
		"smtp": map[string]any{
			"host":     s.Host,
			"port":     s.Port,
			"username": s.Username,
			"password": s.Password,
			"tls_mode": tlsMode(s),
		},
	}, nil
}

type smtpTestResponse struct {
	OK    bool    `json:"ok"`
	Error *string `json:"error"`
}

func TestSMTP(ctx context.Context, c *adminapi.Client, s SMTP, name, to string) error {
	patch, err := emailPatch(s, name)
	if err != nil {
		return err
	}
	body := map[string]any{
		"host":       s.Host,
		"port":       s.Port,
		"tls_mode":   tlsMode(s),
		"username":   nil,
		"password":   nil,
		"from_email": patch["from_email"],
		"from_name":  nil,
	}
	if fromName, _ := patch["from_name"].(string); strings.TrimSpace(fromName) != "" {
		body["from_name"] = fromName
	}
	if s.Username != "" {
		body["username"] = s.Username
	}
	if s.Password != "" {
		body["password"] = s.Password
	}
	if to != "" {
		body["send_to"] = to
	}
	var out smtpTestResponse
	if err := c.Call(ctx, http.MethodPost, "/admin/instance/config/smtp-tests", nil, body, &out); err != nil {
		return err
	}
	if !out.OK {
		msg := "The SMTP server refused the connection."
		if out.Error != nil && strings.TrimSpace(*out.Error) != "" {
			msg = strings.TrimSpace(*out.Error)
		}
		return &SMTPError{Message: msg}
	}
	return nil
}

func saveRecoveryKit(ctx context.Context, c *adminapi.Client, auth map[string]string, origin, dir string, a Answers) (string, error) {
	var kit recoveryKitResponse
	if err := c.Loopback(ctx, http.MethodPost, "/users/@me/recovery-kit", auth, map[string]any{"password": a.Password}, &kit); err != nil {
		return "", err
	}
	path := filepath.Join(dir, "recovery-kit-"+slug(a.Username)+".txt")
	data := recoveryKitText(strings.TrimRight(origin, "/"), a.Name, a.Username, kit)
	if err := envfile.WriteFileAtomic(path, []byte(data), true); err != nil {
		return "", err
	}
	return path, nil
}

func recoveryKitText(origin, name, username string, kit recoveryKitResponse) string {
	created := kit.CreatedAt
	if t, err := time.Parse(time.RFC3339Nano, kit.CreatedAt); err == nil {
		created = t.UTC().Format("2 January 2006 15:04 UTC")
	}
	title := "Fluxer recovery kit"
	if name != "" {
		title = name + " recovery kit"
	}
	var b strings.Builder
	b.WriteString(title + "\n\n")
	b.WriteString("Keep this file somewhere safe. If you forget your password, you can use it to get back into your account. Anyone with this recovery key can reset your password. Never share it.\n\n")
	fmt.Fprintf(&b, "Instance       %s\n", origin)
	fmt.Fprintf(&b, "Username       %s\n", username)
	fmt.Fprintf(&b, "Recovery key   %s\n", kit.RecoveryKey)
	fmt.Fprintf(&b, "Created        %s\n\n", created)
	b.WriteString("How to recover your account\n\n")
	fmt.Fprintf(&b, "1. Go to %s/recover.\n", origin)
	b.WriteString("2. Enter your username and this recovery key.\n")
	b.WriteString("3. Choose a new password. You get a new recovery kit and this one stops working.\n")
	return b.String()
}

var slugPattern = regexp.MustCompile(`[^a-z0-9]+`)

func slug(s string) string {
	out := strings.Trim(slugPattern.ReplaceAllString(strings.ToLower(s), "-"), "-")
	if out == "" {
		return "admin"
	}
	return out
}
