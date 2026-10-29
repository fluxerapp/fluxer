// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"encoding/json"
	"errors"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
)

type fakeAdmin struct {
	sso map[string]any
	err error
}

func (f fakeAdmin) Do(_ context.Context, req adminapi.Request) (*adminapi.Response, error) {
	if f.err != nil {
		return nil, f.err
	}
	if req.Path != "/admin/instance/config" {
		return &adminapi.Response{Status: 404, Body: json.RawMessage(`{}`)}, nil
	}
	body, err := json.Marshal(map[string]any{"sso": f.sso})
	if err != nil {
		return nil, err
	}
	return &adminapi.Response{Status: 200, Body: body}, nil
}

type fakeExec struct {
	out     string
	err     error
	called  bool
	service string
	stdin   ssoProbeInput
	command []string
}

func (f *fakeExec) run(_ context.Context, service, stdin string, command ...string) (string, error) {
	f.called, f.service, f.command = true, service, command
	if err := json.Unmarshal([]byte(stdin), &f.stdin); err != nil {
		return "", err
	}
	return f.out, f.err
}

func ssoEnv(sso map[string]any, apiState string) *Env {
	e := &Env{prepared: true, psDone: true}
	if sso != nil {
		e.Admin = adminapi.New(fakeAdmin{sso: sso}, "")
	}
	if apiState != "" {
		e.ps = []engine.Container{{Service: "api", State: apiState}}
	}
	return e
}

func ssoOn() map[string]any {
	return map[string]any{"enabled": true, "issuer": "https://id.example.com/"}
}

func TestSSOProviderSkips(t *testing.T) {
	cases := map[string]struct {
		env  *Env
		exec fakeExec
	}{
		"no admin API":     {env: ssoEnv(nil, "running")},
		"api not running":  {env: ssoEnv(ssoOn(), "exited")},
		"SSO off":          {env: ssoEnv(map[string]any{"enabled": false, "issuer": "https://id.example.com"}, "running")},
		"no issuer":        {env: ssoEnv(map[string]any{"enabled": true, "issuer": nil}, "running")},
		"blank issuer":     {env: ssoEnv(map[string]any{"enabled": true, "issuer": "  "}, "running")},
		"exec fails":       {env: ssoEnv(ssoOn(), "running"), exec: fakeExec{err: errors.New("container gone")}},
		"no probe output":  {env: ssoEnv(ssoOn(), "running"), exec: fakeExec{out: "node: not found"}},
		"probe threw":      {env: ssoEnv(ssoOn(), "running"), exec: fakeExec{out: `{"ok":false,"stage":"probe","code":"TypeError"}`}},
		"admin API failed": {env: &Env{prepared: true, psDone: true, ps: []engine.Container{{Service: "api", State: "running"}}, Admin: adminapi.New(fakeAdmin{err: errors.New("relay down")}, "")}},
	}
	for name, c := range cases {
		t.Run(name, func(t *testing.T) {
			exec := c.exec
			r := ssoProvider(context.Background(), c.env, exec.run)
			if r.Status != Skip || r.Message != "" {
				t.Fatalf("got %+v, want a quiet skip", r)
			}
		})
	}
}

func TestSSOProviderPass(t *testing.T) {
	exec := &fakeExec{out: `{"ok":true,"host":"id.example.com","token_host":"auth.example.com"}`}
	r := ssoProvider(context.Background(), ssoEnv(ssoOn(), "running"), exec.run)
	if r.Status != Pass {
		t.Fatalf("got %+v, want pass", r)
	}
	if want := "The API connects to your SSO provider at id.example.com and its token endpoint at auth.example.com."; r.Message != want {
		t.Fatalf("message %q, want %q", r.Message, want)
	}
	if exec.service != "api" || len(exec.command) != 3 || exec.command[0] != "node" || exec.command[1] != "-e" {
		t.Fatalf("ran %s %v, want node -e in api", exec.service, exec.command)
	}
	if want := "https://id.example.com/.well-known/openid-configuration"; exec.stdin.Discovery != want {
		t.Fatalf("discovery %q, want %q", exec.stdin.Discovery, want)
	}
	if exec.stdin.Token != "" || exec.stdin.Timeout != ssoProbeSeconds*1000 {
		t.Fatalf("got input %+v", exec.stdin)
	}
}

func TestSSOProviderPassOneHost(t *testing.T) {
	exec := &fakeExec{out: `{"ok":true,"host":"id.example.com","token_host":"id.example.com"}`}
	r := ssoProvider(context.Background(), ssoEnv(ssoOn(), "running"), exec.run)
	if want := "The API connects to your SSO provider at id.example.com."; r.Status != Pass || r.Message != want {
		t.Fatalf("got %+v, want pass with %q", r, want)
	}
}

func TestSSOProviderUsesSetEndpoints(t *testing.T) {
	sso := ssoOn()
	sso["token_url"] = "https://auth.example.com/token"
	exec := &fakeExec{out: `{"ok":true,"host":"id.example.com","token_host":"auth.example.com"}`}
	ssoProvider(context.Background(), ssoEnv(sso, "running"), exec.run)
	if exec.stdin.Token != "https://auth.example.com/token" || exec.stdin.Discovery == "" {
		t.Fatalf("got input %+v, want discovery and the set token URL", exec.stdin)
	}
	sso["authorization_url"] = "https://id.example.com/authorize"
	sso["jwks_url"] = "https://id.example.com/jwks"
	sso["userinfo_url"] = "https://id.example.com/userinfo"
	exec = &fakeExec{out: `{"ok":true,"host":"","token_host":"auth.example.com"}`}
	r := ssoProvider(context.Background(), ssoEnv(sso, "running"), exec.run)
	if exec.stdin.Discovery != "" {
		t.Fatalf("got input %+v, want no discovery when every endpoint is set", exec.stdin)
	}
	if want := "The API connects to your SSO provider at auth.example.com."; r.Status != Pass || r.Message != want {
		t.Fatalf("got %+v, want pass with %q", r, want)
	}
}

func TestSSOProviderFailures(t *testing.T) {
	network := `Make id.example.com resolve to an address the API container can connect to. Add "id.example.com:host-gateway" to extra_hosts for the api service with rexctl config edit, and make your reverse proxy accept connections from Docker.`
	cases := []struct {
		name    string
		out     string
		message string
		fix     string
	}{
		{
			name:    "not found",
			out:     `{"ok":false,"stage":"discovery","host":"id.example.com","code":"ENOTFOUND","message":"getaddrinfo ENOTFOUND id.example.com"}`,
			message: "The API can't look up the address of your SSO provider at id.example.com (ENOTFOUND).",
			fix:     network,
		},
		{
			name:    "refused",
			out:     `{"ok":false,"stage":"discovery","host":"id.example.com","code":"ECONNREFUSED","message":"connect ECONNREFUSED 203.0.113.7:443"}`,
			message: "The API can't connect to your SSO provider at id.example.com because the connection was refused (ECONNREFUSED).",
			fix:     network,
		},
		{
			name:    "fetch timeout",
			out:     `{"ok":false,"stage":"discovery","host":"id.example.com","code":"TimeoutError","message":"The operation was aborted due to timeout"}`,
			message: "The API can't connect to your SSO provider at id.example.com because the connection timed out after 8 seconds.",
			fix:     network,
		},
		{
			name:    "no route",
			out:     `{"ok":false,"stage":"discovery","host":"id.example.com","code":"ENETUNREACH","message":"connect ENETUNREACH 2001:db8::7:443"}`,
			message: "The API can't connect to your SSO provider at id.example.com because there is no network route to it (ENETUNREACH).",
			fix:     network,
		},
		{
			name:    "certificate",
			out:     `{"ok":false,"stage":"discovery","host":"id.example.com","code":"CERT_HAS_EXPIRED","message":"certificate has expired"}`,
			message: "The API doesn't trust the certificate of your SSO provider at id.example.com (CERT_HAS_EXPIRED).",
			fix:     "Give id.example.com a certificate for that name from a public certificate authority, or check that extra_hosts for the api service sends id.example.com to the proxy that serves it.",
		},
		{
			name:    "HTTP status",
			out:     `{"ok":false,"stage":"discovery","host":"id.example.com","status":404}`,
			message: "Your SSO provider at id.example.com answered the API with HTTP 404.",
			fix:     "Check sso.issuer with rexctl config get sso.issuer. Your provider has to serve its discovery document at https://id.example.com/.well-known/openid-configuration.",
		},
		{
			name:    "not a discovery document",
			out:     `{"ok":false,"stage":"discovery","host":"id.example.com","code":"NOT_DISCOVERY"}`,
			message: "Your SSO provider at id.example.com didn't send the API a discovery document.",
			fix:     "Check sso.issuer with rexctl config get sso.issuer. Your provider has to serve its discovery document at https://id.example.com/.well-known/openid-configuration.",
		},
		{
			name:    "token endpoint timeout",
			out:     `{"ok":false,"stage":"token","host":"auth.example.com","code":"ETIMEDOUT","message":"connection timed out"}`,
			message: "The API can't connect to your SSO provider's token endpoint at auth.example.com because the connection timed out after 8 seconds.",
			fix:     `Make auth.example.com resolve to an address the API container can connect to. Add "auth.example.com:host-gateway" to extra_hosts for the api service with rexctl config edit, and make your reverse proxy accept connections from Docker.`,
		},
		{
			name:    "other cause",
			out:     `{"ok":false,"stage":"discovery","host":"id.example.com","code":"EPIPE","message":"write EPIPE"}`,
			message: "The API can't connect to your SSO provider at id.example.com (EPIPE).",
			fix:     network,
		},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			exec := &fakeExec{out: "(node:1) ExperimentalWarning\n" + c.out + "\n"}
			r := ssoProvider(context.Background(), ssoEnv(ssoOn(), "running"), exec.run)
			if r.Status != Fail || r.Fixable {
				t.Fatalf("got %+v, want a failure rexctl can't fix", r)
			}
			if r.Message != c.message {
				t.Errorf("message %q, want %q", r.Message, c.message)
			}
			if r.Fix != c.fix {
				t.Errorf("fix %q, want %q", r.Fix, c.fix)
			}
			if strings.Contains(c.out, `"message"`) && len(r.Details) != 1 {
				t.Errorf("details %v, want the probe's error", r.Details)
			}
		})
	}
}
