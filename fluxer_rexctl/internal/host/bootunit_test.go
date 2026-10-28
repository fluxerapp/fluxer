// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"strconv"
	"strings"
	"testing"
)

func TestUnitBinariesReadWhatRexctlWrites(t *testing.T) {
	const bin = `/home/a b/it's 100%/$rexctl`
	a := AutoUpgrade{Project: "fluxer", Dir: "/srv/fluxer", Binary: bin, At: "04:00", Log: "/srv/fluxer/.rexctl/upgrade.log"}
	for name, c := range map[string]struct {
		unit string
		want string
	}{
		"rootful boot unit":  {BootUnit{Project: "fluxer", Dir: "/srv/fluxer", Binary: bin}.render(false, "fluxer"), LibexecBinary},
		"rootless boot unit": {BootUnit{Project: "fluxer", Dir: "/srv/fluxer", Binary: bin, Rootless: true}.render(false, ""), bin},
		"upgrade service":    {a.service("fluxer"), bin},
		"cron job":           {a.cron(4, 0, "fluxer"), bin},
	} {
		if got := unitBinaries([]byte(c.unit)); !slices.Equal(got, []string{c.want}) {
			t.Errorf("%s runs %q, want %q", name, got, c.want)
		}
	}
}

func TestRootRefreshesOnlyUnitsRootOwns(t *testing.T) {
	home := t.TempDir()
	t.Setenv("HOME", home)
	t.Setenv("XDG_CONFIG_HOME", "")
	prev := asRoot
	t.Cleanup(func() { asRoot = prev })
	for _, root := range []bool{false, true} {
		asRoot = func() bool { return root }
		units, err := unitFiles("fluxer")
		if err != nil {
			t.Fatal(err)
		}
		inHome := 0
		for path := range units {
			if strings.HasPrefix(path, home) {
				inHome++
			}
		}
		if root && inHome > 0 {
			t.Errorf("as root, rexctl reads units from $HOME, which sudo may keep: %v", units)
		}
		if !root && inHome != 2 {
			t.Errorf("without root, rexctl skips your own units: %v", units)
		}
	}
}

func TestInstallKeepsTheBinarysOwner(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("no owners on Windows")
	}
	dir := t.TempDir()
	dst := filepath.Join(dir, "rexctl")
	if got := installArgs("/tmp/new", dst); slices.Contains(got, "-o") {
		t.Fatalf("a new binary got an owner: %q", got)
	}
	if err := os.WriteFile(dst, []byte("old"), 0o755); err != nil {
		t.Fatal(err)
	}
	uid, gid := strconv.Itoa(os.Getuid()), strconv.Itoa(os.Getgid())
	got := installArgs("/tmp/new", dst)
	want := []string{"-D", "-m", "0755", "-o", uid, "-g", gid, "/tmp/new", dst}
	if !slices.Equal(got, want) {
		t.Fatalf("install %q, want %q", got, want)
	}
}
