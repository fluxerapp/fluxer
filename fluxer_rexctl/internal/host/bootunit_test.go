// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"context"
	"os"
	"path/filepath"
	"runtime"
	"testing"
)

func TestUpgradeAsDockerGroupUserRunsNothingAsRoot(t *testing.T) {
	if runtime.GOOS == "windows" || os.Geteuid() == 0 {
		t.Skip("needs a Unix account that isn't root")
	}
	tmp := t.TempDir()
	fake := filepath.Join(tmp, "fake")
	sudoLog := filepath.Join(tmp, "sudo.log")
	writeScript(t, filepath.Join(fake, "sudo"), "echo \"$@\" >> '"+sudoLog+"'\nexit 1\n")
	writeScript(t, filepath.Join(fake, "systemctl"), "for a in \"$@\"; do case $a in is-enabled) echo enabled;; is-active) echo active;; esac; done\n")
	t.Setenv("PATH", fake+string(os.PathListSeparator)+os.Getenv("PATH"))

	oldRoot, oldDir, oldSupported := asRoot, systemUnitDir, bootUnitSupported
	t.Cleanup(func() { asRoot, systemUnitDir, bootUnitSupported = oldRoot, oldDir, oldSupported })
	asRoot = func() bool { return false }
	systemUnitDir = filepath.Join(tmp, "etc", "systemd", "system")
	bootUnitSupported = func() bool { return true }

	bin := filepath.Join(tmp, "home", ".local", "bin", "rexctl")
	writeScript(t, bin, "echo rexctl 2026.1106\n")
	dir := filepath.Join(tmp, "home", "fluxer")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	ctx := context.Background()
	u := BootUnit{Project: "fluxer", Dir: dir, Binary: bin}
	path, err := u.Path()
	if err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(u.content(ctx)), 0o644); err != nil {
		t.Fatal(err)
	}

	writeScript(t, bin, "echo rexctl 2026.1107\n")
	steps, err := u.RefreshSteps(ctx)
	if err != nil {
		t.Fatal(err)
	}
	if err := RunSteps(ctx, steps); err != nil {
		t.Errorf("refreshing the boot unit after the upgrade: %v", err)
	}
	s, err := CheckBootUnit(ctx, u)
	if err != nil {
		t.Fatal(err)
	}
	if s.Needed() {
		t.Errorf("the boot unit wants a fix after the upgrade: %+v", s)
	}
	if got, err := os.ReadFile(sudoLog); err == nil {
		t.Fatalf("the upgrade ran sudo:\n%s", got)
	}
}

func writeScript(t *testing.T, path, body string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte("#!/bin/sh\n"+body), 0o755); err != nil {
		t.Fatal(err)
	}
}
