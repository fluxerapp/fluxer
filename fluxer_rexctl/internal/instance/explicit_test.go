// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build !windows

package instance

import (
	"errors"
	"os"
	"path/filepath"
	"syscall"
	"testing"
)

const nobodyUID = 99

type nasLayout struct {
	base, share, appdata, dir string
	nobody                    map[uint64]bool
}

func (n *nasLayout) giveToNobody(t *testing.T, path string, link bool) {
	t.Helper()
	stat := os.Stat
	if link {
		stat = os.Lstat
	}
	info, err := stat(path)
	if err != nil {
		t.Fatal(err)
	}
	n.nobody[info.Sys().(*syscall.Stat_t).Ino] = true
}

func mkdirMode(t *testing.T, path string, mode os.FileMode) {
	t.Helper()
	if err := os.MkdirAll(path, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.Chmod(path, mode); err != nil {
		t.Fatal(err)
	}
}

func unraid(t *testing.T) *nasLayout {
	t.Helper()
	if present("/opt/fluxer") {
		t.Skip("this machine has an instance in /opt/fluxer")
	}
	base := resolved(t.TempDir())
	mkdirMode(t, base, 0o755)
	n := &nasLayout{base: base, share: filepath.Join(base, "mnt", "user"), nobody: map[uint64]bool{}}
	n.appdata = filepath.Join(n.share, "appdata")
	n.dir = filepath.Join(n.appdata, "fluxer")
	mkdirMode(t, filepath.Join(base, "mnt"), 0o755)
	mkdirMode(t, n.share, 0o777)
	mkdirMode(t, n.appdata, 0o777)
	n.giveToNobody(t, n.share, false)
	n.giveToNobody(t, n.appdata, false)

	oldRoot, oldSudoer, oldRecord, oldHome, oldUID, oldOwner := asRoot, sudoer, MachineRecordPath, rootHome, rootUID, ownerOf
	t.Cleanup(func() {
		asRoot, sudoer, MachineRecordPath, rootHome, rootUID, ownerOf = oldRoot, oldSudoer, oldRecord, oldHome, oldUID, oldOwner
	})
	asRoot = func() bool { return true }
	rootHome = func() (string, error) { return t.TempDir(), nil }
	MachineRecordPath = filepath.Join(t.TempDir(), "etc", RecordFileName)
	rootUID = os.Getuid()
	sudoer = func() (int, string, bool) { return 0, "", false }
	ownerOf = func(info os.FileInfo) (int, bool) {
		uid, ok := oldOwner(info)
		if st, isUnix := info.Sys().(*syscall.Stat_t); isUnix && n.nobody[st.Ino] {
			return nobodyUID, true
		}
		if ok && uid == 0 {
			return rootUID, true
		}
		return uid, ok
	}
	t.Chdir(t.TempDir())
	return n
}

func resolveAs(t *testing.T, how, dir string) (Located, error) {
	t.Helper()
	switch how {
	case "workdir":
		t.Chdir(dir)
		return Resolve("")
	case "flag":
		return Resolve(dir)
	case "record":
		if err := SaveRecord(ScopeMachine, Record{Default: clean(dir), Used: true, Instances: []string{clean(dir)}}); err != nil {
			t.Fatal(err)
		}
		return Resolve("")
	}
	t.Fatalf("unknown way %s", how)
	return Located{}, nil
}

func TestRootUsesAFolderItPickedOnASharedMount(t *testing.T) {
	cases := []struct {
		name    string
		mode    os.FileMode
		nobody  bool
		state   os.FileMode
		file    os.FileMode
		refused string
		shared  string
	}{
		{name: "root owns the instance folder", mode: 0o755, shared: "appdata"},
		{name: "nobody owns the instance folder", mode: 0o755, nobody: true, shared: "dir"},
		{name: "only the owner can open the instance folder", mode: 0o700, nobody: true, shared: "dir"},
		{name: "everyone can write to the instance folder", mode: 0o777, refused: "dir"},
		{name: "the users group can write to the instance folder", mode: 0o775, nobody: true, refused: "dir"},
		{name: "everyone can write to the state folder", mode: 0o755, state: 0o777, refused: "state"},
		{name: "everyone can write to the state file", mode: 0o755, file: 0o666, refused: "file"},
	}
	for _, c := range cases {
		for _, how := range []string{"workdir", "flag", "record"} {
			t.Run(c.name+" via "+how, func(t *testing.T) {
				n := unraid(t)
				plant(t, n.dir, c.mode)
				if c.state != 0 {
					mkdirMode(t, filepath.Join(n.dir, StateDirName), c.state)
				}
				if c.file != 0 {
					if err := os.Chmod(filepath.Join(n.dir, StateDirName, StateFileName), c.file); err != nil {
						t.Fatal(err)
					}
				}
				if c.nobody {
					n.giveToNobody(t, n.dir, false)
				}
				paths := map[string]string{
					"appdata": n.appdata,
					"dir":     n.dir,
					"state":   filepath.Join(n.dir, StateDirName),
					"file":    filepath.Join(n.dir, StateDirName, StateFileName),
				}
				_, useErr := RootNamed(RealPath(n.dir))
				l, err := resolveAs(t, how, n.dir)
				if (useErr == nil) != (err == nil) {
					t.Fatalf("rexctl use got %v, but resolving got %v", useErr, err)
				}
				var untrusted *UntrustedError
				switch {
				case c.refused != "":
					if !errors.As(err, &untrusted) || untrusted.Path != paths[c.refused] || !untrusted.Shared || !untrusted.Inside {
						t.Fatalf("got %+v, %v, want a refusal of %s", l, err, paths[c.refused])
					}
				case err != nil:
					t.Fatalf("got %v, want root to use %s", err, n.dir)
				case !same(l.Dir, n.dir):
					t.Fatalf("got %s, want %s", l.Dir, n.dir)
				case l.Shared != paths[c.shared]:
					t.Fatalf("got a warning about %q, want one about %s", l.Shared, paths[c.shared])
				}
			})
		}
	}
}

func TestRootDoesNotFindAnInstanceOnASharedMountOnItsOwn(t *testing.T) {
	for _, nobody := range []bool{false, true} {
		n := unraid(t)
		plant(t, n.dir, 0o755)
		if nobody {
			n.giveToNobody(t, n.dir, false)
		}
		sudoer = func() (int, string, bool) { return 0, n.appdata, true }
		l, err := Resolve("")
		var untrusted *UntrustedError
		if !errors.As(err, &untrusted) || untrusted.Dir != n.dir || untrusted.Shared == nobody {
			t.Fatalf("nobody %v: got %+v, %v, want a refusal of %s", nobody, l, err, n.dir)
		}
		want := n.appdata
		if nobody {
			want = n.dir
		}
		if untrusted.Path != want {
			t.Fatalf("nobody %v: got a refusal about %s, want %s", nobody, untrusted.Path, want)
		}
	}
}

func TestRootRefusesALinkOthersCanSwap(t *testing.T) {
	cases := []struct {
		name   string
		sticky bool
		nobody bool
		ok     bool
	}{
		{name: "link in a folder everyone can write to"},
		{name: "link of another account in a sticky folder", sticky: true, nobody: true},
		{name: "link of root in a sticky folder", sticky: true, ok: true},
	}
	for _, c := range cases {
		for _, how := range []string{"workdir", "flag"} {
			t.Run(c.name+" via "+how, func(t *testing.T) {
				n := unraid(t)
				real := filepath.Join(n.base, "real", "fluxer")
				mkdirMode(t, filepath.Dir(real), 0o755)
				plant(t, real, 0o755)
				holder := n.appdata
				if c.sticky {
					holder = filepath.Join(n.base, "tmp")
					mkdirMode(t, holder, 0o777|os.ModeSticky)
				}
				link := filepath.Join(holder, "fluxer")
				if err := os.Symlink(real, link); err != nil {
					t.Fatal(err)
				}
				if c.nobody {
					n.giveToNobody(t, link, true)
				}
				_, err := resolveAs(t, how, link)
				var untrusted *UntrustedError
				switch {
				case c.ok && err != nil:
					t.Fatalf("got %v, want root to use %s", err, link)
				case c.ok:
				case !errors.As(err, &untrusted) || untrusted.Link != link || untrusted.Path != holder:
					t.Fatalf("got %v, want a refusal of the link %s in %s", err, link, holder)
				}
			})
		}
	}
}

func TestRootUseRecordsTheFolderALinkLeadsTo(t *testing.T) {
	n := unraid(t)
	real := filepath.Join(n.base, "real", "fluxer")
	mkdirMode(t, filepath.Dir(real), 0o755)
	plant(t, real, 0o755)
	link := filepath.Join(n.appdata, "fluxer")
	if err := os.Symlink(real, link); err != nil {
		t.Fatal(err)
	}
	if shared, err := RootNamed(RealPath(link)); err != nil || shared != "" {
		t.Fatalf("got %q, %v, want rexctl use to accept %s", shared, err, real)
	}
	l, err := resolveAs(t, "record", link)
	if err != nil || l.Dir != real || l.Shared != "" {
		t.Fatalf("got %+v, %v, want %s with no warning", l, err, real)
	}
}
