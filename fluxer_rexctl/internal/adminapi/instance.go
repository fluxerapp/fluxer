// SPDX-License-Identifier: AGPL-3.0-or-later

package adminapi

import (
	"context"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"net/http"
	"net/url"
	"strings"
	"time"
)

const configPath = "/admin/instance/config"

func (c *Client) Config(ctx context.Context) (*InstanceConfigResponse, error) {
	var resp InstanceConfigResponse
	if err := c.Call(ctx, http.MethodGet, configPath, nil, nil, &resp); err != nil {
		return nil, err
	}
	return &resp, nil
}

func (c *Client) RawConfig(ctx context.Context) (map[string]any, error) {
	var resp map[string]any
	if err := c.Call(ctx, http.MethodGet, configPath, nil, nil, &resp); err != nil {
		return nil, err
	}
	return resp, nil
}

func (c *Client) PatchConfig(ctx context.Context, patch any) error {
	return c.Call(ctx, http.MethodPatch, configPath, nil, patch, nil)
}

func (c *Client) PatchConfigWithWarnings(ctx context.Context, patch any) ([]string, error) {
	var resp struct {
		Warnings []string `json:"warnings"`
	}
	if err := c.Call(ctx, http.MethodPatch, configPath, nil, patch, &resp); err != nil {
		return nil, err
	}
	return resp.Warnings, nil
}

var RegistrationModes = []string{"open", "approval", "invite", "closed"}

func RegistrationPatch(mode string) (map[string]any, error) {
	var apiMode string
	links := true
	switch mode {
	case "open", "approval":
		apiMode = mode
	case "invite":
		apiMode = "closed"
	case "closed":
		apiMode = "closed"
		links = false
	default:
		return nil, fmt.Errorf("adminapi: unknown registration mode %q", mode)
	}
	return map[string]any{"registration": map[string]any{
		"mode":                            apiMode,
		"admin_registration_urls_enabled": links,
	}}, nil
}

type Invite struct {
	ID        string  `json:"id"`
	URL       string  `json:"url"`
	Label     string  `json:"label,omitempty"`
	Uses      int     `json:"uses"`
	MaxUses   *int    `json:"max_uses"`
	ExpiresAt *string `json:"expires_at"`
	CreatedAt string  `json:"created_at"`
}

func InviteURL(origin, id string) string {
	return strings.TrimRight(origin, "/") + "/register?registration_url=" + url.QueryEscape(id)
}

func rfc3339(t *time.Time) *string {
	if t == nil {
		return nil
	}
	s := t.UTC().Format(time.RFC3339)
	return &s
}

func (c *Client) ListInvites(ctx context.Context, origin string) ([]Invite, error) {
	cfg, err := c.Config(ctx)
	if err != nil {
		return nil, err
	}
	out := []Invite{}
	now := time.Now()
	for _, u := range cfg.Registration.Urls {
		if u.RevokedAt != nil {
			continue
		}
		if u.ExpiresAt != nil && u.ExpiresAt.Before(now) {
			continue
		}
		inv := Invite{
			ID:        u.Id,
			URL:       InviteURL(origin, u.Id),
			Uses:      u.UseCount,
			MaxUses:   u.MaxUses,
			ExpiresAt: rfc3339(u.ExpiresAt),
			CreatedAt: u.CreatedAt.UTC().Format(time.RFC3339),
		}
		if u.Label != nil {
			inv.Label = *u.Label
		}
		out = append(out, inv)
	}
	return out, nil
}

type InviteOptions struct {
	Uses    int
	Expires time.Duration
	Label   string
}

func (c *Client) CreateInvite(ctx context.Context, opts InviteOptions) (*Invite, error) {
	req := CreateRegistrationUrlRequest{}
	if opts.Uses > 0 {
		uses := opts.Uses
		req.MaxUses = &uses
	}
	if opts.Expires > 0 {
		at := time.Now().Add(opts.Expires).UTC().Truncate(time.Second)
		req.ExpiresAt = &at
	}
	if opts.Label != "" {
		label := opts.Label
		req.Label = &label
	}
	var resp CreateRegistrationUrlResponse
	if err := c.Call(ctx, http.MethodPost, "/admin/instance/registration-urls", nil, req, &resp); err != nil {
		return nil, err
	}
	u := resp.RegistrationUrl
	inv := &Invite{
		ID:        u.Id,
		URL:       resp.Url,
		Uses:      u.UseCount,
		MaxUses:   u.MaxUses,
		ExpiresAt: rfc3339(u.ExpiresAt),
		CreatedAt: u.CreatedAt.UTC().Format(time.RFC3339),
	}
	if u.Label != nil {
		inv.Label = *u.Label
	}
	return inv, nil
}

type InviteNotFoundError struct{ ID string }

func (e *InviteNotFoundError) Error() string { return fmt.Sprintf("adminapi: no invite %q", e.ID) }

func (c *Client) RevokeInvite(ctx context.Context, id string) error {
	cfg, err := c.Config(ctx)
	if err != nil {
		return err
	}
	for _, u := range cfg.Registration.Urls {
		if u.Id != id {
			continue
		}
		if u.RevokedAt != nil {
			return nil
		}
		return c.Call(ctx, http.MethodDelete, "/admin/instance/registration-urls/"+url.PathEscape(id), nil, nil, nil)
	}
	return &InviteNotFoundError{ID: id}
}

type PendingRegistration struct {
	ID          string `json:"id"`
	Username    string `json:"username"`
	Email       string `json:"email,omitempty"`
	RequestedAt string `json:"requested_at"`
}

func (c *Client) PendingRegistrations(ctx context.Context) ([]PendingRegistration, error) {
	var resp PendingRegistrationListResponse
	if err := c.Call(ctx, http.MethodGet, "/admin/instance/pending-registrations", nil, nil, &resp); err != nil {
		return nil, err
	}
	out := []PendingRegistration{}
	for _, p := range resp.PendingRegistrations {
		pr := PendingRegistration{ID: p.UserId, Username: p.Username, RequestedAt: p.RequestedAt.UTC().Format(time.RFC3339)}
		if p.Email != nil {
			pr.Email = *p.Email
		}
		out = append(out, pr)
	}
	return out, nil
}

type PendingNotFoundError struct{ ID string }

func (e *PendingNotFoundError) Error() string {
	return fmt.Sprintf("adminapi: no pending registration %q", e.ID)
}

func (c *Client) ReviewRegistration(ctx context.Context, id string, approve bool) (*PendingRegistration, error) {
	pending, err := c.PendingRegistrations(ctx)
	if err != nil {
		return nil, err
	}
	var found *PendingRegistration
	for i := range pending {
		if pending[i].ID == id || strings.EqualFold(pending[i].Username, id) {
			found = &pending[i]
			break
		}
	}
	if found == nil {
		return nil, &PendingNotFoundError{ID: id}
	}
	status := Rejected
	if approve {
		status = Approved
	}
	req := PendingRegistrationActionRequest{Status: status}
	if err := c.Call(ctx, http.MethodPatch, "/admin/instance/pending-registrations/"+url.PathEscape(found.ID), nil, req, nil); err != nil {
		return nil, err
	}
	return found, nil
}

type SMTPSettings struct {
	Host     string
	Port     int
	TLS      string
	Username string
	Password string
	FromName string
	FromAddr string
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

func (c *Client) SavedTLSMode(ctx context.Context, host string, port int) (string, error) {
	cfg, err := c.RawConfig(ctx)
	if err != nil {
		return "", err
	}
	savedHost, _ := lookup(cfg, "integrations.email.smtp.host")
	savedPort, _ := lookup(cfg, "integrations.email.smtp.port")
	mode, _ := lookup(cfg, "integrations.email.smtp.tls_mode")
	h, _ := savedHost.(string)
	n, _ := savedPort.(float64)
	m, _ := mode.(string)
	if !strings.EqualFold(h, host) || int(n) != port {
		return "", nil
	}
	return m, nil
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
		"username": s.Username,
	}
	if s.Password != "" {
		smtp["password"] = s.Password
	}
	email := map[string]any{
		"delivery":   "smtp",
		"from_email": s.FromAddr,
		"smtp":       smtp,
	}
	if s.FromName != "" {
		email["from_name"] = s.FromName
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
	req := InstanceEmailSmtpTestRequest{}
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

func (c *Client) UploadBranding(ctx context.Context, kind BrandingAssetUploadRequestKind, image []byte) error {
	data := base64.StdEncoding.EncodeToString(image)
	req := BrandingAssetUploadRequest{Kind: kind, Image: &data}
	var resp json.RawMessage
	return c.Call(ctx, http.MethodPost, "/admin/instance/config/branding-assets", nil, req, &resp)
}

func (c *Client) SetBranding(ctx context.Context, name, colour string) error {
	branding := map[string]any{}
	if name != "" {
		branding["product_name"] = name
	}
	if colour != "" {
		branding["theme_color"] = colour
	}
	if len(branding) == 0 {
		return nil
	}
	return c.PatchConfig(ctx, map[string]any{"app_public": map[string]any{"branding": branding}})
}
