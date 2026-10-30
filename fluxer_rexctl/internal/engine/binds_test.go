// SPDX-License-Identifier: AGPL-3.0-or-later

package engine

import (
	"context"
	"os"
	"path/filepath"
	"slices"
	"testing"
)

func writeCompose(t *testing.T, dir, name, data string) {
	t.Helper()
	if err := os.WriteFile(filepath.Join(dir, name), []byte(data), 0o600); err != nil {
		t.Fatal(err)
	}
}

func TestBindSourcesMergesAServiceWithNoBodyLikeLoad(t *testing.T) {
	dir := t.TempDir()
	writeCompose(t, dir, "compose.yml", "services:\n  livekit:\n    image: livekit\n  edge:\n    image: caddy\n")
	writeCompose(t, dir, "custom.compose.yml", "services:\n  livekit:\n  edge:\n    volumes:\n      - ${PWD}/Caddyfile:/etc/caddy/Caddyfile\n")
	s := Spec{Name: "fluxer", Dir: dir, Files: []string{"compose.yml", "custom.compose.yml"}}
	if _, err := (&Engine{}).Load(context.Background(), s); err != nil {
		t.Fatalf("Load: %v", err)
	}
	got, err := BindSources(context.Background(), s)
	if err != nil {
		t.Fatal(err)
	}
	if want := filepath.Join(dir, "Caddyfile"); !slices.Equal(got, []string{want}) {
		t.Fatalf("BindSources = %q, want %q", got, want)
	}
}

func TestBindSourcesReturnsAnErrorForAServiceWithNoBodyOnItsOwn(t *testing.T) {
	dir := t.TempDir()
	writeCompose(t, dir, "custom.compose.yml", "services:\n  livekit:\n")
	s := Spec{Name: "fluxer", Dir: dir, Files: []string{"custom.compose.yml"}}
	if _, err := BindSources(context.Background(), s); err == nil {
		t.Fatal("BindSources read a service with no body and nothing to merge it with")
	}
	if _, err := (&Engine{}).Load(context.Background(), s); err == nil {
		t.Fatal("Load read a service with no body and nothing to merge it with")
	}
}
