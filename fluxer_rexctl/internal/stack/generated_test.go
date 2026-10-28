// SPDX-License-Identifier: AGPL-3.0-or-later

package stack

import (
	"bytes"
	"path/filepath"
	"testing"
)

func TestManagedFilesRoundTrip(t *testing.T) {
	set := Embedded()
	for _, name := range FileNames() {
		plain, err := ReadFile(set, name)
		if err != nil {
			t.Fatal(err)
		}
		managed := Managed(name, plain)
		if !bytes.HasPrefix(managed, header(name)) {
			t.Fatalf("%s has no header", name)
		}
		if !bytes.Equal(Plain(name, managed), plain) {
			t.Fatalf("%s doesn't come back the same after eject", name)
		}
	}
	compose, err := ReadFile(set, ComposeFile)
	if err != nil {
		t.Fatal(err)
	}
	plain, managed := mountRefs(Caddyfile)
	if !bytes.Contains(compose, plain) || bytes.Contains(Managed(ComposeFile, compose), plain) || !bytes.Contains(Managed(ComposeFile, compose), managed) {
		t.Fatal("the edge service doesn't mount the generated Caddyfile")
	}
}

func TestProjectDirOfGeneratedComposeFile(t *testing.T) {
	dir := filepath.Join(t.TempDir(), "fluxer")
	for _, f := range []string{filepath.Join(dir, ComposeFile), GeneratedPath(dir, ComposeFile)} {
		if got := ProjectDir(f); got != dir {
			t.Fatalf("ProjectDir(%s) = %s, want %s", f, got, dir)
		}
	}
}

func TestHeaderSaysHowToChangeTheFile(t *testing.T) {
	for name, want := range map[string]string{
		ComposeFile: "# rexctl writes this file and puts its own version back when it changes Fluxer.\n# To change it, run rexctl config edit.\n\n",
		Caddyfile:   "# rexctl writes this file and puts its own version back when it changes Fluxer.\n# To add Caddy directives, put a .caddy file in caddy.d in your instance folder.\n\n",
	} {
		if got := string(header(name)); got != want {
			t.Errorf("%s header %q, want %q", name, got, want)
		}
	}
}
