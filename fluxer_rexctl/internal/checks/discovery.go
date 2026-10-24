// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"encoding/json"
	"fmt"
	"net/url"
	"strings"
	"time"
)

const (
	discoveryAttempts = 6
	discoveryRetry    = 2 * time.Second
)

func init() {
	Register(Check{ID: "discovery", Phases: Installed, Run: runDiscovery, Fix: fixDiscovery, SafeFix: true})
}

type discoveryDoc struct {
	Endpoints struct {
		API    string `json:"api"`
		WebApp string `json:"webapp"`
	} `json:"endpoints"`
}

func runDiscovery(ctx context.Context, e *Env) Result {
	if e.Instance == nil || e.Domain == "" || !e.running(ctx, "edge") || !e.running(ctx, "api") {
		return skip("")
	}
	out, err := e.discoveryDoc(ctx)
	for attempt := 1; err != nil && attempt < discoveryAttempts; attempt++ {
		select {
		case <-ctx.Done():
			return skip("")
		case <-time.After(discoveryRetry):
		}
		out, err = e.discoveryDoc(ctx)
	}
	if err != nil {
		r := fail("Apps can't load your instance's details.", "Run rexctl logs api to see why.")
		r.Details = []string{out}
		return r
	}
	var doc discoveryDoc
	start := strings.Index(out, "{")
	if start < 0 || json.Unmarshal([]byte(out[start:]), &doc) != nil {
		return fail("Apps can't load your instance's details.", "Run rexctl logs api to see why.")
	}
	got := endpointHost(doc.Endpoints.API)
	if got == "" {
		got = endpointHost(doc.Endpoints.WebApp)
	}
	if got != "" && !strings.EqualFold(got, e.Domain) {
		return fixable(fail(fmt.Sprintf("Apps are told to connect to %s and your instance is %s.", got, e.Domain),
			"Run rexctl doctor --fix to apply your instance's address."))
	}
	return pass(fmt.Sprintf("Apps are told to connect to %s.", e.Domain))
}

func (e *Env) discoveryDoc(ctx context.Context) (string, error) {
	return e.exec(ctx, "edge", "", "wget", "-q", "-O", "-", "http://127.0.0.1:8088/.well-known/fluxer")
}

func endpointHost(s string) string {
	u, err := url.Parse(strings.TrimSpace(s))
	if err != nil {
		return ""
	}
	return u.Hostname()
}

func fixDiscovery(ctx context.Context, e *Env) error { return e.up(ctx) }
