// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"os"
	"path/filepath"
	"testing"
)

func TestLibexecReferenced(t *testing.T) {
	dir := t.TempDir()
	own := filepath.Join(dir, "fluxer.service")
	write := func(name, body string) {
		if err := os.WriteFile(filepath.Join(dir, name), []byte(body), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	write("fluxer.service", "ExecStart="+LibexecBinary+" start --boot\n")
	write("other.service", "ExecStart=/usr/bin/true\n")
	if libexecReferenced(dir, own) {
		t.Fatal("its own unit counted as another user")
	}
	write("fluxer-upgrade.service", "ExecStopPost=-+/usr/bin/install -D -m 0755 /x "+LibexecBinary+"\n")
	if !libexecReferenced(dir, own) {
		t.Fatal("missed the upgrade unit that uses the copy")
	}
}
