// SPDX-License-Identifier: AGPL-3.0-or-later

package adminapi

import (
	"context"
	"net/http"
	"strings"
)

type SMTPSettings struct {
	Host     string
	Port     int
	TLS      string
	Username string
	Password string
	FromName string
	FromAddr string
}

type EmailConfig struct {
	Delivery     string
	SMTP         SMTPSettings
	PasswordSet  bool
	UsernameOnly bool
}

var TLSModes = []string{"starttls", "implicit", "opportunistic", "none"}

func TLSModeFor(port int) string {
	if port == 465 {
		return "implicit"
	}
	return "starttls"
}

func StartTLSFailed(message string) bool {
	return strings.Contains(strings.ToLower(message), "starttls")
}

func (c *Client) Email(ctx context.Context) (*EmailConfig, error) {
	cfg, err := c.RawConfig(ctx)
	if err != nil {
		return nil, err
	}
	str := func(path string) string {
		v, _ := lookup(cfg, path)
		s, _ := v.(string)
		return s
	}
	out := &EmailConfig{Delivery: str("integrations.email.delivery"), UsernameOnly: str("account_identity.mode") == "username"}
	out.SMTP = SMTPSettings{
		Host:     str("integrations.email.smtp.host"),
		TLS:      str("integrations.email.smtp.tls_mode"),
		Username: str("integrations.email.smtp.username"),
		FromName: str("integrations.email.from_name"),
		FromAddr: str("integrations.email.from_email"),
	}
	if v, ok := lookup(cfg, "integrations.email.smtp.port"); ok {
		if n, ok := v.(float64); ok {
			out.SMTP.Port = int(n)
		}
	}
	if v, ok := lookup(cfg, "integrations.email.smtp.password_set"); ok {
		out.PasswordSet, _ = v.(bool)
	}
	return out, nil
}

func nullable(s string) any {
	if s == "" {
		return nil
	}
	return s
}

func (c *Client) SetSMTP(ctx context.Context, s SMTPSettings) error {
	tls := s.TLS
	if tls == "" {
		tls = TLSModeFor(s.Port)
	}
	smtp := map[string]any{
		"host":     s.Host,
		"port":     s.Port,
		"tls_mode": tls,
		"username": nullable(s.Username),
	}
	if s.Password != "" {
		smtp["password"] = s.Password
	}
	email := map[string]any{
		"delivery":   "smtp",
		"from_email": s.FromAddr,
		"from_name":  nullable(s.FromName),
		"smtp":       smtp,
	}
	return c.PatchConfig(ctx, map[string]any{"integrations": map[string]any{"email": email}})
}

func (c *Client) EmailOff(ctx context.Context) error {
	return c.PatchConfig(ctx, map[string]any{"integrations": map[string]any{"email": map[string]any{"delivery": "off"}}})
}

type SMTPTestResult struct {
	OK             bool
	Error          string
	ServerResponse string
}

func (c *Client) TestSMTP(ctx context.Context, sendTo string) (*SMTPTestResult, error) {
	return c.testSMTP(ctx, InstanceEmailSmtpTestRequest{}, sendTo)
}

func (c *Client) TestSMTPWith(ctx context.Context, s SMTPSettings, sendTo string) (*SMTPTestResult, error) {
	tls := InstanceEmailSmtpTestRequestTlsMode(s.TLS)
	if s.TLS == "" {
		tls = InstanceEmailSmtpTestRequestTlsMode(TLSModeFor(s.Port))
	}
	req := InstanceEmailSmtpTestRequest{Host: &s.Host, Port: &s.Port, TlsMode: &tls, Username: &s.Username}
	if s.Password != "" {
		req.Password = &s.Password
	}
	if s.FromAddr != "" {
		from := EmailType(s.FromAddr)
		req.FromEmail = &from
	}
	if s.FromName != "" {
		req.FromName = &s.FromName
	}
	return c.testSMTP(ctx, req, sendTo)
}

func (c *Client) testSMTP(ctx context.Context, req InstanceEmailSmtpTestRequest, sendTo string) (*SMTPTestResult, error) {
	if sendTo != "" {
		to := EmailType(sendTo)
		req.SendTo = &to
	}
	var resp InstanceEmailSmtpTestResponse
	if err := c.Call(ctx, http.MethodPost, "/admin/instance/config/smtp-tests", nil, req, &resp); err != nil {
		return nil, err
	}
	out := &SMTPTestResult{OK: resp.Ok}
	if resp.Error != nil {
		out.Error = *resp.Error
	}
	if resp.ServerResponse != nil {
		out.ServerResponse = *resp.ServerResponse
	}
	return out, nil
}
