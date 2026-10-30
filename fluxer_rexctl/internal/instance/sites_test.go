// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"context"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const mastodonSites = `mastodon.tails.ch {
	tls internal
	reverse_proxy 10.0.4.22:3000
}

tails.ch {
	tls internal
	respond "hello {http.request.host}"
}
`

func TestDefinesSite(t *testing.T) {
	for _, tc := range []struct {
		label string
		file  string
		want  bool
	}{
		{"two site blocks", mastodonSites, true},
		{"site list with commas", "a.example.com, b.example.com {\n\trespond ok\n}\n", true},
		{"site list over two lines", "a.example.com,\nb.example.com {\n\trespond ok\n}\n", true},
		{"port only", ":8443 {\n\trespond ok\n}\n", true},
		{"scheme and localhost", "http://localhost {\n\trespond ok\n}\n", true},
		{"single label with port", "intranet:8080 {\n\trespond ok\n}\n", true},
		{"ipv6 address", "[::1]:8080 {\n\trespond ok\n}\n", true},
		{"wildcard host", "*.example.com {\n\trespond ok\n}\n", true},
		{"snippet", "(cors) {\n\theader Access-Control-Allow-Origin *\n}\n", false},
		{"global options", "{\n\temail admin@example.com\n}\n", false},
		{"plain directives", "header X-Mine yes\nencode gzip\n", false},
		{"directive blocks", "handle /extra/* {\n\trespond ok\n}\nreverse_proxy 10.0.4.22:3000 {\n\theader_up Host {host}\n}\nroute {\n\trespond ok\n}\n", false},
		{"matcher block", "@blocked {\n\tpath /secret/*\n}\nrespond @blocked 403\n", false},
		{"comment with a site", "# mastodon.tails.ch {\n# }\nheader X-Mine yes\n", false},
		{"brace inside a string", "respond \"example.com {\"\nheader X-Mine yes\n", false},
		{"backtick string", "respond `a.example.com {\n}`\n", false},
		{"import line", "import /etc/caddy/extra/*.caddy\n", false},
		{"env placeholder key", "{$MASTODON_HOST} {\n\treverse_proxy 10.0.4.22:3000\n}\n", false},
		{"env placeholder in host", "{$SUB}.example.com {\n\trespond ok\n}\n", false},
		{"placeholder inside a directive", "respond \"{system.hostname}\" 200\n", false},
		{"heredoc", "respond <<EOF\n\texample.com {\n\tEOF\n", false},
		{"unbalanced braces", "example.com {\n\trespond ok\n", false},
	} {
		if got := DefinesSite([]byte(tc.file)); got != tc.want {
			t.Errorf("%s: DefinesSite = %v, want %v", tc.label, got, tc.want)
		}
	}
}

func TestOnlySites(t *testing.T) {
	for _, tc := range []struct {
		label string
		file  string
		want  bool
	}{
		{"two site blocks", mastodonSites, true},
		{"sites and a snippet", "(common) {\n\tencode gzip\n}\n" + mastodonSites, true},
		{"sites with comments", "# my sites\n" + mastodonSites + "# end\n", true},
		{"site and global options", "{\n\temail admin@example.com\n}\n" + mastodonSites, false},
		{"site and import line", "import common.caddy\n" + mastodonSites, false},
		{"site and directive", "header X-Mine yes\n" + mastodonSites, false},
		{"snippet only", "(cors) {\n\theader Access-Control-Allow-Origin *\n}\n", false},
		{"env placeholder key", "{$MASTODON_HOST} {\n\treverse_proxy 10.0.4.22:3000\n}\n", false},
		{"empty file", "", false},
	} {
		if got := onlySites([]byte(tc.file)); got != tc.want {
			t.Errorf("%s: onlySites = %v, want %v", tc.label, got, tc.want)
		}
	}
}

func TestMoveSitesMovesOnlySiteFiles(t *testing.T) {
	inst, err := Create(t.TempDir(), "fluxer")
	if err != nil {
		t.Fatal(err)
	}
	files := map[string]string{
		"Mastodon.caddy": mastodonSites,
		"headers.caddy":  "header X-Mine yes\n",
		"mixed.caddy":    "header X-Mine yes\n" + mastodonSites,
		"env.caddy":      "{$MASTODON_HOST} {\n\treverse_proxy 10.0.4.22:3000\n}\n",
		"notes.txt":      mastodonSites,
		"taken.caddy":    mastodonSites,
	}
	for f, data := range files {
		writeTestFile(t, inst.Path(CaddyDropInDir, f), []byte(data))
	}
	writeTestFile(t, inst.Path(CaddySitesDir, "taken.caddy"), []byte("other.example.com {\n}\n"))
	moved, err := inst.MoveSites()
	if err != nil {
		t.Fatal(err)
	}
	if !slices.Equal(moved, []string{"Mastodon.caddy"}) {
		t.Fatalf("moved %v, want [Mastodon.caddy]", moved)
	}
	if got := string(readTestFile(t, inst.Path(CaddySitesDir, "Mastodon.caddy"))); got != mastodonSites {
		t.Fatalf("moved file %q", got)
	}
	for f := range files {
		_, err := os.Stat(inst.Path(CaddyDropInDir, f))
		if gone := os.IsNotExist(err); gone != (f == "Mastodon.caddy") {
			t.Errorf("%s left in caddy.d = %v", f, !gone)
		}
	}
	if again, err := inst.MoveSites(); err != nil || len(again) > 0 {
		t.Fatalf("second run moved %v, %v", again, err)
	}
	if err := inst.PutBackSites(moved); err != nil {
		t.Fatal(err)
	}
	if got := string(readTestFile(t, inst.Path(CaddyDropInDir, "Mastodon.caddy"))); got != mastodonSites {
		t.Fatalf("put back %q", got)
	}
	if _, err := os.Stat(inst.Path(CaddySitesDir, "Mastodon.caddy")); !os.IsNotExist(err) {
		t.Fatal("the site file is still in caddy.d/sites")
	}
}

func TestMoveSitesWithoutDropInFolder(t *testing.T) {
	inst := &Instance{Dir: filepath.Join(t.TempDir(), "missing")}
	moved, err := inst.MoveSites()
	if err != nil || moved != nil {
		t.Fatalf("got %v, %v", moved, err)
	}
}

func TestShippedCaddyfileImportsSitesAtTopLevel(t *testing.T) {
	data, err := stack.ReadFile(stack.Embedded(), stack.Caddyfile)
	if err != nil {
		t.Fatal(err)
	}
	entries, ok := caddyEntries(data)
	if !ok {
		t.Fatal("the shipped Caddyfile doesn't parse")
	}
	var top []string
	for _, e := range entries {
		if !e.block {
			var keys []string
			for _, k := range e.keys {
				keys = append(keys, k.text)
			}
			top = append(top, strings.Join(keys, " "))
		}
	}
	if !slices.Contains(top, "import /etc/caddy/conf.d/sites/*.caddy") {
		t.Fatalf("top level lines %q, want the sites import", top)
	}
	if slices.Contains(top, "import /etc/caddy/conf.d/*.caddy") {
		t.Fatal("caddy.d directives load at the top level")
	}
	if !strings.Contains(string(data), "\timport /etc/caddy/conf.d/*.caddy\n") {
		t.Fatal("caddy.d directives no longer load inside the site")
	}
}

func TestEdgeMountsCoverSitesFolder(t *testing.T) {
	for _, mode := range []string{"", "proxy", "tunnel"} {
		inst, err := Create(t.TempDir(), "fluxer")
		if err != nil {
			t.Fatal(err)
		}
		if _, err := os.Stat(inst.Path(CaddySitesDir)); err != nil {
			t.Fatalf("Create made no %s: %v", CaddySitesDir, err)
		}
		set := stack.Embedded()
		inst.State.Release = set.Release
		inst.State.Images = Images{Release: set.Release, Refs: fluxerRefs("a")}
		inst.State.Settings[settings.Domain] = "chat.example.com"
		if mode != "" {
			inst.State.Settings[settings.EdgeMode] = mode
		}
		if _, err := secrets.Mint(inst.State.Secrets, false); err != nil {
			t.Fatal(err)
		}
		if _, err := inst.Render(set, stack.OverlaysFor(inst.State.Settings), inst.State.Images.Refs); err != nil {
			t.Fatal(err)
		}
		env, err := inst.Environment()
		if err != nil {
			t.Fatal(err)
		}
		var none engine.Engine
		p, err := none.Load(context.Background(), engine.Spec{Name: "fluxer", Dir: inst.Dir, Files: inst.ComposeFiles(), Env: env})
		if err != nil {
			t.Fatalf("%q: %v", mode, err)
		}
		o := p.HelperFor("edge")
		if o.Image == "" {
			t.Fatalf("%q: no edge image", mode)
		}
		want := map[string]string{"/etc/caddy/Caddyfile": inst.Generated(stack.Caddyfile), "/etc/caddy/conf.d": inst.Path(CaddyDropInDir)}
		for _, m := range o.Mounts {
			if src, ok := want[m.Target]; ok && m.Bind == src {
				delete(want, m.Target)
			}
		}
		if len(want) > 0 {
			t.Fatalf("%q: edge mounts %v, missing %v", mode, o.Mounts, want)
		}
		if !slices.ContainsFunc(o.Env, func(kv string) bool { return strings.HasPrefix(kv, "FLUXER_EDGE_SITE_ADDRESS=") }) {
			t.Fatalf("%q: edge env %v", mode, o.Env)
		}
	}
}

func TestTopLevelImports(t *testing.T) {
	shipped, err := stack.ReadFile(stack.Embedded(), stack.Caddyfile)
	if err != nil {
		t.Fatal(err)
	}
	for _, tc := range []struct {
		label          string
		file           string
		sites, dropIns bool
	}{
		{"shipped Caddyfile", string(shipped), true, false},
		{"legacy Caddyfile", "{\n\temail a@tails.ch\n}\nimport /etc/caddy/conf.d/*.caddy\n{$FLUXER_DOMAIN} {\n\treverse_proxy app:8080\n}\n", false, true},
		{"nested drop-ins only", "example.com {\n\timport /etc/caddy/conf.d/*.caddy\n}\n", false, false},
		{"commented sites import", "# import /etc/caddy/conf.d/sites/*.caddy\nexample.com {\n}\n", false, false},
		{"snippet import", "(x) {\n}\nimport x\n", false, false},
	} {
		sites, dropIns := TopLevelImports([]byte(tc.file))
		if sites != tc.sites || dropIns != tc.dropIns {
			t.Errorf("%s: TopLevelImports = %v, %v, want %v, %v", tc.label, sites, dropIns, tc.sites, tc.dropIns)
		}
	}
}

func TestUnloadAndRestoreSites(t *testing.T) {
	inst, err := Create(t.TempDir(), "fluxer")
	if err != nil {
		t.Fatal(err)
	}
	writeTestFile(t, inst.Path(CaddySitesDir, "a.caddy"), []byte(mastodonSites))
	writeTestFile(t, inst.Path(CaddySitesDir, "b.caddy"), []byte(mastodonSites))
	writeTestFile(t, inst.Path(CaddySitesDir, "notes.txt"), []byte("x\n"))
	writeTestFile(t, inst.Path(CaddyDropInDir, "b.caddy"), []byte("header X-Mine yes\n"))
	moved, err := inst.UnloadSites()
	if err != nil {
		t.Fatal(err)
	}
	if !slices.Equal(moved, []string{"a.caddy"}) {
		t.Fatalf("unloaded %v, want [a.caddy]", moved)
	}
	back, err := inst.RestoreSites([]string{"a.caddy", "missing.caddy"})
	if err != nil {
		t.Fatal(err)
	}
	if !slices.Equal(back, []string{"a.caddy"}) {
		t.Fatalf("restored %v, want [a.caddy]", back)
	}
	if got := string(readTestFile(t, inst.Path(CaddySitesDir, "a.caddy"))); got != mastodonSites {
		t.Fatalf("restored file %q", got)
	}
}
