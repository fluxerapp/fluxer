// SPDX-License-Identifier: AGPL-3.0-or-later

package deploy

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
)

const nestedSiteStderr = `{"level":"info","ts":1793328334.3,"msg":"using config from file","file":"/etc/caddy/Caddyfile"}
{"level":"error","ts":1793328334.371964,"msg":"/etc/caddy/conf.d/Mastodon.caddy:1: unrecognized directive: mastodon.tails.ch"}
`

func writeDropIn(t *testing.T, dir, file, data string) {
	t.Helper()
	path := filepath.Join(dir, instance.CaddyDropInDir, file)
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(data), 0o644); err != nil {
		t.Fatal(err)
	}
}

func TestCaddyMessage(t *testing.T) {
	for _, tc := range []struct {
		stderr, want string
	}{
		{nestedSiteStderr, "/etc/caddy/conf.d/Mastodon.caddy:1: unrecognized directive: mastodon.tails.ch"},
		{"Error: adapting config using caddyfile: /etc/caddy/Caddyfile:3: unknown directive\n", "/etc/caddy/Caddyfile:3: unknown directive"},
		{"", ""},
	} {
		if got := caddyMessage(tc.stderr); got != tc.want {
			t.Errorf("caddyMessage(%q) = %q, want %q", tc.stderr, got, tc.want)
		}
	}
}

func TestEdgeProblemPointsAtSiteFile(t *testing.T) {
	dir := t.TempDir()
	writeDropIn(t, dir, "Mastodon.caddy", "mastodon.tails.ch {\n\treverse_proxy 10.0.4.22:3000\n}\n")
	e := edgeProblem(dir, nestedSiteStderr)
	if e.Caddy != "caddy.d/Mastodon.caddy:1: unrecognized directive: mastodon.tails.ch" {
		t.Fatalf("caddy error %q", e.Caddy)
	}
	if e.Site != "caddy.d/Mastodon.caddy" {
		t.Fatalf("site file %q", e.Site)
	}
	if fix := e.Fix("try again"); fix != "caddy.d/Mastodon.caddy defines its own site. Move it to caddy.d/sites, then try again." {
		t.Fatalf("fix %q", fix)
	}
}

func TestEdgeProblemLooksForSitesWhenCaddyBlamesTheCaddyfile(t *testing.T) {
	dir := t.TempDir()
	writeDropIn(t, dir, "headers.caddy", "header X-Mine yes\n")
	writeDropIn(t, dir, "tails.caddy", "header X-Mine yes\ntails.ch {\n\trespond ok\n}\n")
	e := edgeProblem(dir, "Error: adapting config using caddyfile: /etc/caddy/Caddyfile:20: unrecognized directive: tails.ch\n")
	if e.Site != "caddy.d/tails.caddy" {
		t.Fatalf("site file %q", e.Site)
	}
	if !strings.HasPrefix(e.Caddy, "Caddyfile:20:") {
		t.Fatalf("caddy error %q", e.Caddy)
	}
}

func TestEdgeProblemWithoutSiteFile(t *testing.T) {
	dir := t.TempDir()
	writeDropIn(t, dir, "headers.caddy", "header X-Mine yes\nbogus\n")
	writeDropIn(t, dir, "sites/tails.caddy", "tails.ch {\n\tbogus\n}\n")
	e := edgeProblem(dir, `{"level":"error","msg":"/etc/caddy/conf.d/sites/tails.caddy:2: unrecognized directive: bogus"}`)
	if e.Site != "" {
		t.Fatalf("site file %q", e.Site)
	}
	if fix := e.Fix("try again"); fix != "Fix the file Caddy reports, then try again." {
		t.Fatalf("fix %q", fix)
	}
}
