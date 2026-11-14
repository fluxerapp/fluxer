// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build !windows

package instance

import (
	"errors"
	"os"
	"path/filepath"
	"testing"
)

func TestResolveUnderSudo(t *testing.T) {
	cases := []struct {
		name    string
		sudo    bool
		mode    os.FileMode
		record  string
		want    string
		from    From
		refused bool
	}{
		{name: "finds the folder of the account that ran sudo", sudo: true, mode: 0o755, want: "fluxer", from: FromSudoer},
		{name: "uses the record of that account", sudo: true, mode: 0o755, record: "chat", want: "chat", from: FromSudoer},
		{name: "refuses a folder other accounts can change", sudo: true, mode: 0o777, want: "fluxer", refused: true},
		{name: "without sudo keeps the standard folder", sudo: false, mode: 0o755, want: "/opt/fluxer", from: FromFallback},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			if present("/opt/fluxer") {
				t.Skip("this machine has an instance in /opt/fluxer")
			}
			home := resolved(t.TempDir())
			if err := os.Chmod(home, 0o755); err != nil {
				t.Fatal(err)
			}
			for _, name := range []string{"fluxer", "chat"} {
				dir := filepath.Join(home, name)
				if err := os.MkdirAll(filepath.Join(dir, StateDirName), 0o755); err != nil {
					t.Fatal(err)
				}
				if err := os.WriteFile(filepath.Join(dir, StateDirName, StateFileName), []byte("{}"), 0o600); err != nil {
					t.Fatal(err)
				}
				if err := os.Chmod(dir, c.mode); err != nil {
					t.Fatal(err)
				}
			}
			if c.record != "" {
				path := filepath.Join(home, ".config", "rexctl", RecordFileName)
				if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
					t.Fatal(err)
				}
				if err := os.WriteFile(path, Record{Default: filepath.Join(home, c.record), Instances: []string{filepath.Join(home, c.record)}}.Encode(), 0o600); err != nil {
					t.Fatal(err)
				}
			}
			t.Chdir(t.TempDir())
			oldRoot, oldSudoer, oldRecord, oldHome := asRoot, sudoer, MachineRecordPath, rootHome
			t.Cleanup(func() { asRoot, sudoer, MachineRecordPath, rootHome = oldRoot, oldSudoer, oldRecord, oldHome })
			asRoot = func() bool { return true }
			rootHome = func() (string, error) { return t.TempDir(), nil }
			MachineRecordPath = filepath.Join(t.TempDir(), "etc", RecordFileName)
			sudoer = func() (int, string, bool) { return os.Getuid(), home, c.sudo }

			l, err := Resolve("")
			want := c.want
			if !filepath.IsAbs(want) {
				want = filepath.Join(home, want)
			}
			var untrusted *UntrustedError
			switch {
			case c.refused:
				if !errors.As(err, &untrusted) || untrusted.Path != want {
					t.Fatalf("got %+v, %v, want a refusal of %s", l, err, want)
				}
			case err != nil:
				t.Fatal(err)
			case l.Dir != want || l.From != c.from:
				t.Fatalf("got %s from %d, want %s from %d", l.Dir, l.From, want, c.from)
			}
		})
	}
}

func TestRootPath(t *testing.T) {
	if os.Geteuid() == 0 {
		t.Skip("root owns the test folders")
	}
	if got := RootPath("/", true); got != "" {
		t.Fatalf("got %s for /, want nothing", got)
	}
	dir := resolved(t.TempDir())
	if got := RootPath(filepath.Join(dir, "fluxer"), true); got != dir {
		t.Fatalf("got %q, want %s, which this account owns", got, dir)
	}
}

func plant(t *testing.T, dir string, mode os.FileMode) {
	t.Helper()
	if err := os.MkdirAll(filepath.Join(dir, StateDirName), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, StateDirName, StateFileName), []byte("{}"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.Chmod(dir, mode); err != nil {
		t.Fatal(err)
	}
}

func asRootFor(t *testing.T, uid int) {
	t.Helper()
	oldRoot, oldSudoer, oldRecord, oldHome := asRoot, sudoer, MachineRecordPath, rootHome
	t.Cleanup(func() { asRoot, sudoer, MachineRecordPath, rootHome = oldRoot, oldSudoer, oldRecord, oldHome })
	asRoot = func() bool { return true }
	rootHome = func() (string, error) { return t.TempDir(), nil }
	MachineRecordPath = filepath.Join(t.TempDir(), "etc", RecordFileName)
	sudoer = func() (int, string, bool) { return uid, t.TempDir(), true }
}

func TestRootTrustsOnlyFoldersRootAndTheSudoAccountControl(t *testing.T) {
	if os.Geteuid() == 0 {
		t.Skip("root owns the test folders")
	}
	if present("/opt/fluxer") {
		t.Skip("this machine has an instance in /opt/fluxer")
	}
	me, stranger := os.Getuid(), os.Getuid()+4242
	cases := []struct {
		name   string
		sudo   int
		mode   os.FileMode
		how    string
		shared bool
		ok     bool
	}{
		{name: "current folder of the sudo account", sudo: me, mode: 0o755, how: "workdir", ok: true},
		{name: "current folder of another account", sudo: stranger, mode: 0o755, how: "workdir", ok: true},
		{name: "current folder anyone can change", sudo: me, mode: 0o777, how: "workdir", shared: true},
		{name: "--dir of another account", sudo: stranger, mode: 0o755, how: "flag", ok: true},
		{name: "--dir anyone can change", sudo: me, mode: 0o777, how: "flag", shared: true},
		{name: "remembered folder of another account", sudo: stranger, mode: 0o755, how: "remembered"},
		{name: "remembered folder of the sudo account", sudo: me, mode: 0o755, how: "remembered", ok: true},
		{name: "link in a folder anyone can change", sudo: me, mode: 0o755, how: "link", shared: true},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			base := resolved(t.TempDir())
			if err := os.Chmod(base, 0o755); err != nil {
				t.Fatal(err)
			}
			dir := filepath.Join(base, "fluxer")
			plant(t, dir, c.mode)
			asRootFor(t, c.sudo)
			t.Chdir(t.TempDir())
			flag, want := "", dir
			switch c.how {
			case "workdir":
				t.Chdir(dir)
			case "flag":
				flag = dir
			case "link":
				shared := filepath.Join(base, "shared")
				if err := os.Mkdir(shared, 0o755); err != nil {
					t.Fatal(err)
				}
				if err := os.Chmod(shared, 0o777); err != nil {
					t.Fatal(err)
				}
				want = filepath.Join(shared, "fluxer")
				if err := os.Symlink(dir, want); err != nil {
					t.Fatal(err)
				}
			}
			var l Located
			var err error
			if c.how == "workdir" || c.how == "flag" {
				l, err = Resolve(flag)
			} else {
				l, err = Located{Dir: want}, rootMayUse(want)
			}
			var untrusted *UntrustedError
			switch {
			case c.ok && err != nil:
				t.Fatalf("got %v, want %s", err, want)
			case c.ok:
			case !errors.As(err, &untrusted):
				t.Fatalf("root uses %s (from %d), want a refusal", l.Dir, l.From)
			case untrusted.Shared != c.shared:
				t.Fatalf("got %+v, want shared %v", untrusted, c.shared)
			}
		})
	}
}

func TestUntrustedPartChecksTheLinkAndItsTarget(t *testing.T) {
	base := resolved(t.TempDir())
	target, shared := filepath.Join(base, "target"), filepath.Join(base, "shared")
	for _, d := range []string{base, target, shared} {
		if err := os.MkdirAll(d, 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.Chmod(d, 0o755); err != nil {
			t.Fatal(err)
		}
	}
	if err := os.Chmod(shared, 0o777); err != nil {
		t.Fatal(err)
	}
	link := filepath.Join(shared, "native")
	if err := os.Symlink(target, link); err != nil {
		t.Fatal(err)
	}
	anyOwner := func(os.FileInfo, int) bool { return true }
	if p, isShared := untrustedPart(filepath.Join(link, "fluxer"), anyOwner); p != shared || !isShared {
		t.Fatalf("got %q, %v, want %s, which every account can change", p, isShared, shared)
	}
	if p, _ := untrustedPart(filepath.Join(target, "fluxer"), anyOwner); p != "" {
		t.Fatalf("got %q for the target itself, want nothing", p)
	}
	if got := RealPath(filepath.Join(link, "fluxer")); got != filepath.Join(target, "fluxer") {
		t.Fatalf("got %s, want the folder the link leads to", got)
	}
}

func TestMovedInstance(t *testing.T) {
	base := t.TempDir()
	to := filepath.Join(base, "native")
	inst := &Instance{Dir: filepath.Join(base, "docker"), State: &State{MovedTo: to}}
	if inst.Moved() != nil {
		t.Fatal("an instance whose new folder is gone counts as moved")
	}
	plant(t, to, 0o755)
	var moved *MovedError
	if !errors.As(inst.Managed(), &moved) || moved.To != to {
		t.Fatalf("got %v, want a refusal that names %s", inst.Managed(), to)
	}
}
