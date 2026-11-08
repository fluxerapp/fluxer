// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestUnitBinaryStepsListsEveryCopyOnce(t *testing.T) {
	dir := t.TempDir()
	bin := filepath.Join(dir, "rexctl-new")
	if err := os.WriteFile(bin, []byte("new"), 0o755); err != nil {
		t.Fatal(err)
	}
	old := filepath.Join(dir, "libexec", "rexctl")
	boot := filepath.Join(dir, "fluxer.service")
	upgrade := filepath.Join(dir, "fluxer-upgrade.service")
	for path, body := range map[string]string{
		boot:    "ExecStart=" + old + " start --boot\n",
		upgrade: "ExecStart=" + old + " upgrade --yes\n",
	} {
		if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	units := map[string]bool{boot: true, upgrade: true, filepath.Join(dir, "missing.service"): true}
	steps, err := unitBinarySteps(units, bin)
	if err != nil {
		t.Fatal(err)
	}
	if len(steps) != 1 {
		t.Fatalf("got %d steps, want 1 for the shared copy", len(steps))
	}
	if !strings.Contains(steps[0].Command, old) || !strings.Contains(steps[0].Do, old) {
		t.Fatalf("step does not name the copy it replaces: %+v", steps[0])
	}
	if _, err := os.Stat(old); !os.IsNotExist(err) {
		t.Fatal("planning wrote the copy before anyone confirmed")
	}
}

func TestUnitBinaryStepsSkipsCopiesAlreadyCurrent(t *testing.T) {
	dir := t.TempDir()
	bin := filepath.Join(dir, "rexctl")
	if err := os.WriteFile(bin, []byte("same"), 0o755); err != nil {
		t.Fatal(err)
	}
	unit := filepath.Join(dir, "fluxer.service")
	if err := os.WriteFile(unit, []byte("ExecStart="+bin+" start --boot\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	steps, err := unitBinarySteps(map[string]bool{unit: false}, bin)
	if err != nil {
		t.Fatal(err)
	}
	if len(steps) != 0 {
		t.Fatalf("got %d steps for a copy that is already current", len(steps))
	}
}
