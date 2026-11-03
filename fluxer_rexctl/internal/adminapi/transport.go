// SPDX-License-Identifier: AGPL-3.0-or-later

package adminapi

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	osuser "os/user"
	"strings"

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

var ErrSchemaMismatch = errors.New("adminapi: rexctl and Fluxer versions differ")
