// SPDX-License-Identifier: AGPL-3.0-or-later

package engine

import (
	"context"
	"os"
	"path/filepath"
	"testing"
)

func TestLoadResolvesPWDToTheInstanceFolder(t *testing.T) {
	dir := t.TempDir()
	compose := "services:\n  edge:\n    image: caddy\n    volumes:\n      - ${PWD}/Caddyfile:/etc/caddy/Caddyfile\n"
	if err := os.WriteFile(filepath.Join(dir, "compose.yml"), []byte(compose), 0o600); err != nil {
		t.Fatal(err)
	}
	p, err := (&Engine{}).Load(context.Background(), Spec{Name: "fluxer", Dir: dir, Files: []string{"compose.yml"}})
	if err != nil {
		t.Fatal(err)
	}
	want := filepath.Join(dir, "Caddyfile")
	if got := p.Model.Services["edge"].Volumes[0].Source; got != want {
		t.Errorf("bind source = %q, want %q", got, want)
	}
}
