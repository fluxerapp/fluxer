// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"os"
	"path/filepath"
	"testing"
)

func TestOvercommitAtReadsTheSetting(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "overcommit_memory")
	for value, want := range map[string]OvercommitState{"1\n": OvercommitOn, "0\n": OvercommitOff, "2\n": OvercommitOff} {
		if err := os.WriteFile(path, []byte(value), 0o644); err != nil {
			t.Fatal(err)
		}
		if got := overcommitAt(path, false); got != want {
			t.Errorf("overcommitAt(%q) = %v, want %v", value, got, want)
		}
	}
	if got := overcommitAt(filepath.Join(dir, "missing"), false); got != OvercommitOn {
		t.Errorf("a missing file = %v, want %v", got, OvercommitOn)
	}
}

func TestOvercommitAtSeesASettingRootCantWrite(t *testing.T) {
	if os.Geteuid() == 0 {
		t.Skip("root can write a read-only file")
	}
	path := filepath.Join(t.TempDir(), "overcommit_memory")
	if err := os.WriteFile(path, []byte("0\n"), 0o444); err != nil {
		t.Fatal(err)
	}
	if got := overcommitAt(path, true); got != OvercommitLocked {
		t.Errorf("as root = %v, want %v", got, OvercommitLocked)
	}
	if got := overcommitAt(path, false); got != OvercommitOff {
		t.Errorf("as a user who can use sudo = %v, want %v", got, OvercommitOff)
	}
}
