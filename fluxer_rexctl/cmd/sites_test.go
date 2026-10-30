// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	legacyCaddyfile = "{\n\temail admin@tails.ch\n}\n\nimport /etc/caddy/conf.d/*.caddy\n\n{$FLUXER_DOMAIN} {\n\treverse_proxy app:8080\n}\n"
	mastodonSite    = "mastodon.tails.ch {\n\treverse_proxy 10.0.4.22:3000\n}\n"
	importingSite   = "import common\nwiki.tails.ch {\n\trespond ok\n}\n"
)

func sitesInstance(t *testing.T, release string, caddyfile []byte) (*App, *instance.Instance, *bytes.Buffer) {
	t.Helper()
	inst, err := instance.Create(t.TempDir(), "fluxer")
	if err != nil {
		t.Fatal(err)
	}
	inst.State.Release = release
	if caddyfile != nil {
		compose, err := stack.ReadFile(stack.Embedded(), stack.ComposeFile)
		if err != nil {
			t.Fatal(err)
		}
		writeFile(t, filepath.Join(inst.SetDir(release), stack.ComposeFile), compose)
		writeFile(t, filepath.Join(inst.SetDir(release), stack.Caddyfile), caddyfile)
	}
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	out := &bytes.Buffer{}
	app := &App{UI: ui.New(ui.Options{Stdin: strings.NewReader(""), Stdout: out, Stderr: out})}
	return app, inst, out
}

func writeFile(t *testing.T, path string, data []byte) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, data, 0o644); err != nil {
		t.Fatal(err)
	}
}

func caddyFilesIn(t *testing.T, dir string) []string {
	t.Helper()
	matches, err := filepath.Glob(filepath.Join(dir, "*.caddy"))
	if err != nil {
		t.Fatal(err)
	}
	var out []string
	for _, m := range matches {
		out = append(out, filepath.Base(m))
	}
	return out
}

func TestRollbackToLegacyMovesSitesBackAndUpgradeMovesThemAgain(t *testing.T) {
	app, inst, out := sitesInstance(t, stack.LegacyRelease, []byte(legacyCaddyfile))
	writeFile(t, inst.Path(instance.CaddySitesDir, "Mastodon.caddy"), []byte(mastodonSite))
	writeFile(t, inst.Path(instance.CaddySitesDir, "wiki.caddy"), []byte(importingSite))
	if err := matchSites(app, inst); err != nil {
		t.Fatal(err)
	}
	want := []string{"Mastodon.caddy", "wiki.caddy"}
	if got := caddyFilesIn(t, inst.Path(instance.CaddyDropInDir)); !slices.Equal(got, want) {
		t.Fatalf("caddy.d has %v, want %v", got, want)
	}
	if got := caddyFilesIn(t, inst.Path(instance.CaddySitesDir)); len(got) > 0 {
		t.Fatalf("caddy.d/sites still has %v", got)
	}
	if !slices.Equal(inst.State.MovedSites, want) {
		t.Fatalf("recorded %v, want %v", inst.State.MovedSites, want)
	}
	if !strings.Contains(out.String(), "Moved caddy.d/sites/Mastodon.caddy to caddy.d/Mastodon.caddy.") {
		t.Fatalf("output %q", out.String())
	}
	saved, err := instance.Find(inst.Dir)
	if err != nil {
		t.Fatal(err)
	}
	if !slices.Equal(saved.State.MovedSites, want) {
		t.Fatalf("saved record %v, want %v", saved.State.MovedSites, want)
	}

	inst.State.Release = stack.Embedded().Release
	inst.State.Upgrade = &instance.Upgrade{From: stack.LegacyRelease, To: inst.State.Release}
	if err := moveSites(app, inst); err != nil {
		t.Fatal(err)
	}
	if got := caddyFilesIn(t, inst.Path(instance.CaddySitesDir)); !slices.Equal(got, want) {
		t.Fatalf("caddy.d/sites has %v after the upgrade, want %v", got, want)
	}
	if !slices.Equal(inst.State.Upgrade.Sites, want) {
		t.Fatalf("upgrade record %v, want %v", inst.State.Upgrade.Sites, want)
	}

	clearUpgrade(inst)
	if got := caddyFilesIn(t, inst.Path(instance.CaddyDropInDir)); !slices.Equal(got, want) {
		t.Fatalf("caddy.d has %v after a stopped upgrade, want %v", got, want)
	}
	if !slices.Equal(inst.State.MovedSites, want) {
		t.Fatalf("record after a stopped upgrade %v, want %v", inst.State.MovedSites, want)
	}
}

func TestRollbackToReleaseWithSitesLeavesThemPut(t *testing.T) {
	app, inst, out := sitesInstance(t, stack.Embedded().Release, nil)
	writeFile(t, inst.Path(instance.CaddySitesDir, "Mastodon.caddy"), []byte(mastodonSite))
	if err := matchSites(app, inst); err != nil {
		t.Fatal(err)
	}
	if got := caddyFilesIn(t, inst.Path(instance.CaddySitesDir)); !slices.Equal(got, []string{"Mastodon.caddy"}) {
		t.Fatalf("caddy.d/sites has %v", got)
	}
	if got := caddyFilesIn(t, inst.Path(instance.CaddyDropInDir)); len(got) > 0 {
		t.Fatalf("caddy.d has %v", got)
	}
	if len(inst.State.MovedSites) > 0 || out.Len() > 0 {
		t.Fatalf("recorded %v, output %q", inst.State.MovedSites, out.String())
	}
}

func TestGoingForwardToReleaseWithSitesRestoresRecordedFiles(t *testing.T) {
	app, inst, _ := sitesInstance(t, stack.Embedded().Release, nil)
	writeFile(t, inst.Path(instance.CaddyDropInDir, "wiki.caddy"), []byte(importingSite))
	inst.State.MovedSites = []string{"wiki.caddy"}
	if err := matchSites(app, inst); err != nil {
		t.Fatal(err)
	}
	if got := caddyFilesIn(t, inst.Path(instance.CaddySitesDir)); !slices.Equal(got, []string{"wiki.caddy"}) {
		t.Fatalf("caddy.d/sites has %v", got)
	}
	if inst.State.MovedSites != nil {
		t.Fatalf("record %v left", inst.State.MovedSites)
	}
}

func TestRollbackToReleaseWithoutSitesImportLeavesThemPut(t *testing.T) {
	data, err := stack.ReadFile(stack.Embedded(), stack.Caddyfile)
	if err != nil {
		t.Fatal(err)
	}
	older := bytes.Replace(data, []byte("\nimport /etc/caddy/conf.d/sites/*.caddy\n"), nil, 1)
	if bytes.Equal(older, data) {
		t.Fatal("the shipped Caddyfile has no sites import to take out")
	}
	app, inst, out := sitesInstance(t, "2026.1029.100000", older)
	writeFile(t, inst.Path(instance.CaddySitesDir, "Mastodon.caddy"), []byte(mastodonSite))
	if err := matchSites(app, inst); err != nil {
		t.Fatal(err)
	}
	if got := caddyFilesIn(t, inst.Path(instance.CaddySitesDir)); !slices.Equal(got, []string{"Mastodon.caddy"}) {
		t.Fatalf("caddy.d/sites has %v", got)
	}
	if len(inst.State.MovedSites) > 0 {
		t.Fatalf("recorded %v", inst.State.MovedSites)
	}
	if !strings.Contains(out.String(), "doesn't load caddy.d/sites") {
		t.Fatalf("output %q", out.String())
	}
}

func TestRestoredLegacyBackupLoadsSitesFromCaddyDropIns(t *testing.T) {
	app, inst, _ := sitesInstance(t, stack.LegacyRelease, []byte(legacyCaddyfile))
	writeFile(t, inst.Path(instance.CaddySitesDir, "Mastodon.caddy"), []byte(mastodonSite))
	matchRestoredSites(app)(inst)
	if got := caddyFilesIn(t, inst.Path(instance.CaddyDropInDir)); !slices.Equal(got, []string{"Mastodon.caddy"}) {
		t.Fatalf("caddy.d has %v", got)
	}
	if got := caddyFilesIn(t, inst.Path(instance.CaddySitesDir)); len(got) > 0 {
		t.Fatalf("caddy.d/sites still has %v", got)
	}
	if !slices.Equal(inst.State.MovedSites, []string{"Mastodon.caddy"}) {
		t.Fatalf("recorded %v", inst.State.MovedSites)
	}
}
