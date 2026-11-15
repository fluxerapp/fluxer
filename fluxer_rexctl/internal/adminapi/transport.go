// SPDX-License-Identifier: AGPL-3.0-or-later

package adminapi

import (
	"bytes"
	"context"
	"encoding/base64"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/url"
	"os"
	osuser "os/user"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
)

const (
	RelayService = "api"
	RelayPath    = "/usr/src/app/fluxer_api/bin/operator-relay.mjs"
	RelayNode    = "node"
)

type Target string

const (
	TargetOperator Target = "operator"
	TargetLoopback Target = "loopback"
)

type Request struct {
	Target     Target            `json:"target"`
	Method     string            `json:"method"`
	Path       string            `json:"path"`
	Query      map[string]string `json:"query,omitempty"`
	Headers    map[string]string `json:"headers,omitempty"`
	Body       json.RawMessage   `json:"body,omitempty"`
	BodyBase64 string            `json:"body_base64,omitempty"`
	HostUser   string            `json:"host_user,omitempty"`
}

type Response struct {
	Status      int               `json:"status"`
	Headers     map[string]string `json:"headers,omitempty"`
	Body        json.RawMessage   `json:"body,omitempty"`
	APIVersion  string            `json:"api_version"`
	SchemaMajor int               `json:"schema_major"`
	RelayError  string            `json:"relay_error,omitempty"`
}

type Transport interface {
	Do(ctx context.Context, req Request) (*Response, error)
}

type ExecTransport struct {
	Project  *engine.Project
	HostUser string
}

func NewExec(p *engine.Project) *ExecTransport {
	return &ExecTransport{Project: p, HostUser: HostUser()}
}

func (t *ExecTransport) Do(ctx context.Context, req Request) (*Response, error) {
	if req.HostUser == "" {
		req.HostUser = t.HostUser
	}
	payload, err := json.Marshal(req)
	if err != nil {
		return nil, err
	}
	var stdout, stderr bytes.Buffer
	err = t.Project.Exec(ctx, RelayService, engine.ExecOptions{
		Stdin:  bytes.NewReader(payload),
		Stdout: &stdout,
		Stderr: &stderr,
	}, RelayNode, RelayPath)
	if ctx.Err() != nil {
		return nil, ctx.Err()
	}
	if err != nil {
		if missing := stderr.String(); strings.Contains(missing, "Cannot find module") && strings.Contains(missing, RelayPath) {
			return nil, &RelayError{Detail: "this Fluxer release has no " + RelayPath, Err: ErrNoRelay}
		}
		return nil, &RelayError{Detail: firstLine(stderr.String(), err.Error()), Err: err}
	}
	var resp Response
	if err := json.Unmarshal(bytes.TrimSpace(stdout.Bytes()), &resp); err != nil {
		return nil, &RelayError{Detail: firstLine(stderr.String(), "the relay wrote no response"), Err: err}
	}
	return &resp, nil
}

const (
	hostUserHeader = "X-Fluxer-Operator-Host-User"
	hostTimeout    = 120 * time.Second
)

type HostTransport struct {
	Socket         string
	APIAddr        string
	ClientIPHeader string
	HostUser       string
}

func NewHost(socket, apiAddr, clientIPHeader string) *HostTransport {
	return &HostTransport{Socket: socket, APIAddr: apiAddr, ClientIPHeader: clientIPHeader, HostUser: HostUser()}
}

func (t *HostTransport) Do(ctx context.Context, req Request) (*Response, error) {
	if req.HostUser == "" {
		req.HostUser = t.HostUser
	}
	if !strings.HasPrefix(req.Path, "/") {
		return nil, &RelayError{Detail: "path must start with /", Err: ErrBadRequest}
	}
	headers := http.Header{}
	for k, v := range req.Headers {
		headers.Set(k, v)
	}
	client := &http.Client{Timeout: hostTimeout}
	base := "http://" + t.APIAddr
	switch req.Target {
	case TargetOperator:
		if req.Path != "/admin" && !strings.HasPrefix(req.Path, "/admin/") {
			return nil, &RelayError{Detail: "the operator target serves /admin paths only", Err: ErrBadRequest}
		}
		if req.HostUser != "" {
			headers.Set(hostUserHeader, req.HostUser)
		}
		socket := t.Socket
		client.Transport = &http.Transport{DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) {
			var d net.Dialer
			return d.DialContext(ctx, "unix", socket)
		}}
		base = "http://operator"
	case TargetLoopback:
		name := t.ClientIPHeader
		if name == "" {
			name = "x-forwarded-for"
		}
		if headers.Get(name) == "" {
			headers.Set(name, "127.0.0.1")
		}
	default:
		return nil, &RelayError{Detail: fmt.Sprintf("unknown target %s", req.Target), Err: ErrBadRequest}
	}
	var body io.Reader
	switch {
	case req.BodyBase64 != "":
		data, err := base64.StdEncoding.DecodeString(req.BodyBase64)
		if err != nil {
			return nil, &RelayError{Detail: "the body is not base64", Err: err}
		}
		body = bytes.NewReader(data)
	case len(req.Body) > 0 && string(req.Body) != "null":
		if headers.Get("Content-Type") == "" {
			headers.Set("Content-Type", "application/json")
		}
		body = bytes.NewReader(req.Body)
	}
	u := base + req.Path
	if len(req.Query) > 0 {
		q := url.Values{}
		for k, v := range req.Query {
			q.Set(k, v)
		}
		u += "?" + q.Encode()
	}
	method := req.Method
	if method == "" {
		method = http.MethodGet
	}
	hreq, err := http.NewRequestWithContext(ctx, method, u, body)
	if err != nil {
		return nil, &RelayError{Detail: err.Error(), Err: err}
	}
	hreq.Header = headers
	resp, err := client.Do(hreq)
	if ctx.Err() != nil {
		return nil, ctx.Err()
	}
	if err != nil {
		return nil, &RelayError{Detail: err.Error(), Err: err}
	}
	defer resp.Body.Close()
	data, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, &RelayError{Detail: err.Error(), Err: err}
	}
	out := &Response{Status: resp.StatusCode, Headers: map[string]string{}, APIVersion: resp.Header.Get("X-Fluxer-Version"), SchemaMajor: SchemaMajor}
	for k, v := range resp.Header {
		out.Headers[strings.ToLower(k)] = strings.Join(v, ", ")
	}
	out.Body = hostBody(data, resp.Header.Get("Content-Type"))
	return out, nil
}

func hostBody(data []byte, contentType string) json.RawMessage {
	if len(data) == 0 {
		return nil
	}
	if strings.Contains(contentType, "json") && json.Valid(data) {
		return data
	}
	text, _ := json.Marshal(string(data))
	return text
}

func firstLine(s, fallback string) string {
	s = strings.TrimSpace(s)
	if s == "" {
		return fallback
	}
	if i := strings.IndexByte(s, '\n'); i >= 0 {
		return strings.TrimSpace(s[:i])
	}
	return s
}

func HostUser() string {
	for _, key := range []string{"SUDO_USER", "USER", "USERNAME"} {
		if v := strings.TrimSpace(os.Getenv(key)); v != "" {
			return v
		}
	}
	if u, err := osuser.Current(); err == nil {
		return u.Username
	}
	return ""
}

type RelayError struct {
	Detail string
	Err    error
}

func (e *RelayError) Error() string { return "adminapi: relay: " + e.Detail }

func (e *RelayError) Unwrap() error { return e.Err }

type FieldError struct {
	Path    string `json:"path"`
	Code    string `json:"code,omitempty"`
	Message string `json:"message"`
}

type APIError struct {
	Status  int
	Code    string
	Message string
	Fields  []FieldError
	Body    json.RawMessage
}

func (e *APIError) Error() string {
	msg := e.Message
	if len(e.Fields) > 0 {
		f := e.Fields[0]
		if f.Path != "" {
			msg = fmt.Sprintf("Fluxer didn't accept the value for %s. %s", fieldName(f.Path), f.Message)
		} else {
			msg = f.Message
		}
	}
	if msg != "" {
		return msg
	}
	return fmt.Sprintf("admin API returned %d", e.Status)
}

func fieldName(path string) string {
	parts := strings.FieldsFunc(path, func(r rune) bool { return r == '.' || r == '[' || r == ']' })
	for i := len(parts) - 1; i >= 0; i-- {
		if strings.Trim(parts[i], "0123456789") != "" {
			return strings.ReplaceAll(parts[i], "_", " ")
		}
	}
	return path
}

var ErrNoRelay = errors.New("adminapi: this Fluxer release has no operator relay")

var ErrBadRequest = errors.New("adminapi: the request can't be sent")

var ErrSchemaMismatch = errors.New("adminapi: rexctl and Fluxer versions differ")
