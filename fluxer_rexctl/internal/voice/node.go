// SPDX-License-Identifier: AGPL-3.0-or-later

package voice

import (
	"bytes"
	"context"
	"crypto/hmac"
	"crypto/sha256"
	"encoding/base64"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"strconv"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
)

const (
	TokenPrefix   = "fvj_"
	NodeAudience  = "fluxer-voice-node"
	NodePath      = "/voice/node"
	nodeTokenLife = 2 * time.Minute

	FeatureServerAddresses = "server_addresses"
)

const (
	CodeTokenInvalid        = "VOICE_JOIN_TOKEN_INVALID"
	CodeTokenBusy           = "VOICE_JOIN_TOKEN_BUSY"
	CodeEndpointInvalid     = "VOICE_JOIN_ENDPOINT_INVALID"
	CodeEndpointUnreachable = "VOICE_JOIN_ENDPOINT_UNREACHABLE"
	CodeKeyMismatch         = "VOICE_JOIN_KEY_MISMATCH"
	CodeIDInUse             = "VOICE_SERVER_ID_IN_USE"
	CodeKeyInUse            = "VOICE_SERVER_API_KEY_IN_USE"
	CodeNodeUnknown         = "VOICE_NODE_UNKNOWN"
	CodeSignatureInvalid    = "VOICE_NODE_SIGNATURE_INVALID"
	CodeTokenExpired        = "VOICE_NODE_TOKEN_EXPIRED"
	CodeUnknownRegion       = "UNKNOWN_VOICE_REGION"
)

func ValidToken(token string) bool {
	rest, ok := strings.CutPrefix(token, TokenPrefix)
	if !ok {
		return false
	}
	id, secret, ok := strings.Cut(rest, "_")
	return ok && id != "" && strings.Trim(id, "0123456789") == "" && len(secret) == 43
}

type APIError struct {
	Status  int
	Code    string
	Message string
	Detail  string
	Date    time.Time
}

func (e *APIError) Error() string {
	msg := e.Message
	if e.Detail != "" {
		msg += " (" + e.Detail + ")"
	}
	if e.Code != "" {
		return fmt.Sprintf("%s: %s", e.Code, msg)
	}
	return fmt.Sprintf("HTTP %d: %s", e.Status, msg)
}

func CodeOf(err error) string {
	var e *APIError
	if errors.As(err, &e) {
		return e.Code
	}
	return ""
}

const ClockFix = "Set the clock on this machine. sudo timedatectl set-ntp true turns on time sync on most Linux machines."

const clockLeeway = time.Minute

func ClockProblem(err error, instance string, now time.Time) string {
	var e *APIError
	if !errors.As(err, &e) || e.Code != CodeTokenExpired {
		return ""
	}
	if text := ClockOff(e.Date, instance, now); text != "" {
		return text
	}
	return fmt.Sprintf("%s rejects the time on requests from this machine", instance)
}

func ClockOff(date time.Time, instance string, now time.Time) string {
	off := now.Sub(date)
	switch {
	case date.IsZero() || off.Abs() <= clockLeeway:
		return ""
	case off < 0:
		return fmt.Sprintf("The clock on this machine is about %s behind %s", about(-off), instance)
	}
	return fmt.Sprintf("The clock on this machine is about %s ahead of %s", about(off), instance)
}

func about(d time.Duration) string {
	unit, name := time.Second, "second"
	switch {
	case d >= 90*time.Minute:
		unit, name = time.Hour, "hour"
	case d >= 90*time.Second:
		unit, name = time.Minute, "minute"
	}
	n := int(d.Round(unit) / unit)
	if n == 1 {
		return "1 " + name
	}
	return fmt.Sprintf("%d %ss", n, name)
}

type Discovery struct {
	APIPublic      string
	APICodeVersion int
	VoiceEnabled   bool
	VoiceOrigins   []string
	ListsOrigins   bool
}

type Client struct {
	HTTP *http.Client
}

func (c *Client) http() *http.Client {
	if c.HTTP != nil {
		return c.HTTP
	}
	return &http.Client{Timeout: 30 * time.Second}
}

func (c *Client) Discover(ctx context.Context, domain string) (*Discovery, error) {
	target := domain
	if !strings.Contains(target, "://") {
		target = "https://" + target
	}
	u, err := url.Parse(target)
	if err != nil || u.Host == "" {
		return nil, fmt.Errorf("voice: %s isn't a domain", domain)
	}
	u.Path = "/.well-known/fluxer"
	var doc struct {
		APICodeVersion int `json:"api_code_version"`
		Endpoints      struct {
			APIPublic string `json:"api_public"`
		} `json:"endpoints"`
		Features struct {
			VoiceEnabled bool `json:"voice_enabled"`
		} `json:"features"`
		VoiceOrigins *[]string `json:"voice_origins"`
	}
	if err := c.do(ctx, http.MethodGet, u.String(), nil, nil, &doc); err != nil {
		return nil, err
	}
	if doc.Endpoints.APIPublic == "" {
		return nil, fmt.Errorf("voice: %s has no public API endpoint in its discovery document", domain)
	}
	return &Discovery{
		APIPublic:      strings.TrimRight(doc.Endpoints.APIPublic, "/"),
		APICodeVersion: doc.APICodeVersion,
		VoiceEnabled:   doc.Features.VoiceEnabled,
		VoiceOrigins:   deref(doc.VoiceOrigins),
		ListsOrigins:   doc.VoiceOrigins != nil,
	}, nil
}

func deref(list *[]string) []string {
	if list == nil {
		return nil
	}
	return *list
}

type PreviewRequest struct {
	PublicIPv4    string `json:"public_ipv4,omitempty"`
	PublicIPv6    string `json:"public_ipv6,omitempty"`
	RegionID      string `json:"region_id,omitempty"`
	CPUCount      int    `json:"cpu_count,omitempty"`
	MemoryBytes   uint64 `json:"memory_bytes,omitempty"`
	RexctlVersion string `json:"rexctl_version,omitempty"`
}

type Preview struct {
	ServerID       string `json:"server_id"`
	ServerIDPinned bool   `json:"server_id_pinned"`
	Region         struct {
		ID         string   `json:"id"`
		Name       string   `json:"name"`
		DistanceKm *float64 `json:"distance_km"`
		New        bool     `json:"new"`
		Source     string   `json:"source"`
	} `json:"region"`
	Latitude          *float64              `json:"latitude"`
	Longitude         *float64              `json:"longitude"`
	Hostname          *string               `json:"hostname"`
	SuggestedHostname string                `json:"suggested_hostname"`
	SoftLimit         *int                  `json:"soft_connection_limit"`
	WebhookURL        string                `json:"webhook_url"`
	APICodeVersion    int                   `json:"api_code_version"`
	Release           string                `json:"release"`
	ExpiresAt         string                `json:"expires_at"`
	TurnServers       []settings.TurnServer `json:"turn_servers"`
}

type JoinRequest struct {
	ServerID   string   `json:"server_id,omitempty"`
	RegionID   string   `json:"region_id,omitempty"`
	Endpoint   string   `json:"endpoint"`
	APIKey     string   `json:"api_key"`
	APISecret  string   `json:"api_secret"`
	Latitude   *float64 `json:"latitude,omitempty"`
	Longitude  *float64 `json:"longitude,omitempty"`
	SoftLimit  int      `json:"soft_connection_limit,omitempty"`
	PublicIPv4 string   `json:"public_ipv4,omitempty"`
	PublicIPv6 string   `json:"public_ipv6,omitempty"`
}

type Joined struct {
	ServerID   string `json:"server_id"`
	RegionID   string `json:"region_id"`
	Endpoint   string `json:"endpoint"`
	WebhookURL string `json:"webhook_url"`
	NodeURL    string `json:"node_url"`
}

type Node struct {
	RegionID   string                 `json:"region_id"`
	ServerID   string                 `json:"server_id"`
	Endpoint   string                 `json:"endpoint"`
	Latitude   *float64               `json:"latitude"`
	Longitude  *float64               `json:"longitude"`
	IsActive   bool                   `json:"is_active"`
	Draining   bool                   `json:"draining"`
	SoftLimit  *int                   `json:"soft_connection_limit"`
	Turn       *[]settings.TurnServer `json:"turn_servers"`
	PublicIPv4 *string                `json:"public_ipv4"`
	PublicIPv6 *string                `json:"public_ipv6"`
	Features   []string               `json:"features"`
	Health     struct {
		Status    string  `json:"status"`
		CheckedAt *string `json:"checked_at"`
		Error     *string `json:"error"`
	} `json:"health"`
}

func (c *Client) Preview(ctx context.Context, api, token string, req PreviewRequest) (*Preview, error) {
	var out Preview
	err := c.do(ctx, http.MethodPost, api+"/voice/join/preview", map[string]string{"Authorization": "VoiceJoin " + token}, req, &out)
	return &out, err
}

func (c *Client) Join(ctx context.Context, api, token string, req JoinRequest) (*Joined, error) {
	var out Joined
	err := c.do(ctx, http.MethodPost, api+"/voice/join", map[string]string{"Authorization": "VoiceJoin " + token}, req, &out)
	return &out, err
}

type NodeUpdate struct {
	Endpoint   *string  `json:"endpoint,omitempty"`
	PublicIPv4 *string  `json:"public_ipv4,omitempty"`
	PublicIPv6 *string  `json:"public_ipv6,omitempty"`
	Draining   *bool    `json:"draining,omitempty"`
	Latitude   *float64 `json:"latitude,omitempty"`
	Longitude  *float64 `json:"longitude,omitempty"`
}

func (c *Client) GetNode(ctx context.Context, nodeURL, key, secret string) (*Node, error) {
	var out Node
	err := c.node(ctx, http.MethodGet, nodeURL, key, secret, nil, &out)
	return &out, err
}

func (c *Client) UpdateNode(ctx context.Context, nodeURL, key, secret string, update NodeUpdate) (*Node, error) {
	var out Node
	err := c.node(ctx, http.MethodPatch, nodeURL, key, secret, update, &out)
	return &out, err
}

func (c *Client) DeleteNode(ctx context.Context, nodeURL, key, secret string) error {
	return c.node(ctx, http.MethodDelete, nodeURL, key, secret, nil, nil)
}

func (c *Client) node(ctx context.Context, method, nodeURL, key, secret string, in, out any) error {
	var body []byte
	if in != nil {
		var err error
		if body, err = json.Marshal(in); err != nil {
			return err
		}
	}
	token := NodeToken(key, secret, method, body, time.Now())
	return c.doRaw(ctx, method, nodeURL, map[string]string{"Authorization": "Bearer " + token}, body, out)
}

func NodeToken(key, secret, method string, body []byte, now time.Time) string {
	digest := ""
	if len(body) > 0 {
		sum := sha256.Sum256(body)
		digest = base64.StdEncoding.EncodeToString(sum[:])
	}
	return SignHS256(secret, map[string]any{
		"iss":    key,
		"aud":    NodeAudience,
		"method": method,
		"path":   NodePath,
		"sha256": digest,
		"iat":    now.Unix(),
		"exp":    now.Add(nodeTokenLife).Unix(),
	})
}

func SignHS256(secret string, claims map[string]any) string {
	enc := base64.RawURLEncoding
	header, _ := json.Marshal(map[string]string{"alg": "HS256", "typ": "JWT"})
	payload, _ := json.Marshal(claims)
	input := enc.EncodeToString(header) + "." + enc.EncodeToString(payload)
	mac := hmac.New(sha256.New, []byte(secret))
	mac.Write([]byte(input))
	return input + "." + enc.EncodeToString(mac.Sum(nil))
}

func (c *Client) do(ctx context.Context, method, target string, headers map[string]string, in, out any) error {
	var body []byte
	if in != nil {
		var err error
		if body, err = json.Marshal(in); err != nil {
			return err
		}
	}
	return c.doRaw(ctx, method, target, headers, body, out)
}

func (c *Client) doRaw(ctx context.Context, method, target string, headers map[string]string, body []byte, out any) error {
	var reader io.Reader
	if body != nil {
		reader = bytes.NewReader(body)
	}
	req, err := http.NewRequestWithContext(ctx, method, target, reader)
	if err != nil {
		return err
	}
	if body != nil {
		req.Header.Set("Content-Type", "application/json")
	}
	req.Header.Set("Accept", "application/json")
	for k, v := range headers {
		req.Header.Set(k, v)
	}
	resp, err := c.http().Do(req)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	data, err := io.ReadAll(io.LimitReader(resp.Body, 1<<20))
	if err != nil {
		return err
	}
	if resp.StatusCode >= 400 {
		e := &APIError{Status: resp.StatusCode}
		if d, err := http.ParseTime(resp.Header.Get("Date")); err == nil {
			e.Date = d
		}
		var parsed struct {
			Code    string `json:"code"`
			Message string `json:"message"`
			Detail  string `json:"detail"`
		}
		if json.Unmarshal(data, &parsed) == nil {
			e.Code, e.Message, e.Detail = parsed.Code, parsed.Message, parsed.Detail
		}
		if e.Message == "" {
			e.Message = strings.TrimSpace(string(data))
		}
		return e
	}
	if out == nil || len(bytes.TrimSpace(data)) == 0 {
		return nil
	}
	return json.Unmarshal(data, out)
}

func (c *Client) TestWebhook(ctx context.Context, target, key, secret string, now time.Time) (int, time.Time, error) {
	body, err := json.Marshal(map[string]any{
		"event":     "room_started",
		"id":        "EV_" + strings.TrimPrefix(TestRoom(), TestRoomPrefix),
		"createdAt": strconv.FormatInt(now.Unix(), 10),
		"room":      map[string]any{"name": TestRoom()},
	})
	if err != nil {
		return 0, time.Time{}, err
	}
	sum := sha256.Sum256(body)
	token := SignHS256(secret, map[string]any{
		"iss":    key,
		"nbf":    now.Add(-10 * time.Second).Unix(),
		"exp":    now.Add(time.Minute).Unix(),
		"sha256": base64.StdEncoding.EncodeToString(sum[:]),
	})
	req, err := http.NewRequestWithContext(ctx, http.MethodPost, target, bytes.NewReader(body))
	if err != nil {
		return 0, time.Time{}, err
	}
	req.Header.Set("Content-Type", "application/webhook+json")
	req.Header.Set("Authorization", token)
	resp, err := c.HTTP.Do(req)
	if err != nil {
		return 0, time.Time{}, err
	}
	resp.Body.Close()
	date, _ := http.ParseTime(resp.Header.Get("Date"))
	return resp.StatusCode, date, nil
}

func (c *Client) AllowsOrigin(ctx context.Context, domain, origin string) (bool, error) {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, "https://"+domain+"/", nil)
	if err != nil {
		return false, err
	}
	resp, err := c.HTTP.Do(req)
	if err != nil {
		return false, err
	}
	resp.Body.Close()
	return ConnectSrcAllows(resp.Header.Values("Content-Security-Policy"), origin), nil
}

func ConnectSrcAllows(policies []string, origin string) bool {
	for _, policy := range policies {
		for _, directive := range strings.Split(policy, ";") {
			fields := strings.Fields(directive)
			if len(fields) == 0 || fields[0] != "connect-src" {
				continue
			}
			for _, source := range fields[1:] {
				if strings.TrimRight(source, "/") == origin {
					return true
				}
			}
		}
	}
	return false
}
