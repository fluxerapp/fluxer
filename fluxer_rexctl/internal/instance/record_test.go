// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"errors"
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"strings"
	"sync"
	"testing"
)

func isolateRecords(t *testing.T) string {
	t.Helper()
	home := t.TempDir()
	t.Setenv("HOME", home)
	t.Setenv("XDG_CONFIG_HOME", filepath.Join(home, ".config"))
	t.Setenv("AppData", filepath.Join(home, "AppData"))
	old := MachineRecordPath
	MachineRecordPath = filepath.Join(t.TempDir(), "etc", "rexctl", RecordFileName)
	t.Cleanup(func() { MachineRecordPath = old })
	oldRoot := asRoot
	asRoot = func() bool { return false }
	t.Cleanup(func() { asRoot = oldRoot })
	t.Chdir(t.TempDir())
	return home
}

func actAsRoot(t *testing.T) {
	t.Helper()
	if runtime.GOOS == "windows" {
		t.Skip("there's no machine record on Windows")
	}
	oldRoot, oldUID := asRoot, rootUID
	asRoot, rootUID = func() bool { return true }, os.Getuid()
	t.Cleanup(func() { asRoot, rootUID = oldRoot, oldUID })
}

func managedAt(t *testing.T, dir string) string {
	t.Helper()
	inst, err := Create(dir, "fluxer")
	if err != nil {
		t.Fatal(err)
	}
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	if err := os.Chmod(inst.Dir, 0o755); err != nil {
		t.Fatal(err)
	}
	return clean(inst.Dir)
}

func record(t *testing.T, s Scope, r Record) {
	t.Helper()
	if err := SaveRecord(s, r); err != nil {
		t.Fatal(err)
	}
}

func remember(t *testing.T, s Scope, dir string, makeDefault bool) {
	t.Helper()
	if err := Remember(s, dir, makeDefault); err != nil {
		t.Fatal(err)
	}
}

func TestResolveOrder(t *testing.T) {
	isolateRecords(t)
	here := managedAt(t, t.TempDir())
	saved := managedAt(t, t.TempDir())
	record(t, ScopeUser, Record{Default: saved, Instances: []string{saved}})
	elsewhere, _ := os.Getwd()

	t.Chdir(here)
	if l, _ := Resolve("/srv/other"); l.From != FromFlag || l.Dir != "/srv/other" {
		t.Errorf("with --dir = %+v", l)
	}
	if l, _ := Resolve(""); l.From != FromWorkdir || clean(l.Dir) != here {
		t.Errorf("inside an instance = %+v", l)
	}
	t.Chdir(elsewhere)
	if l, _ := Resolve(""); l.From != FromRecord || l.Dir != saved {
		t.Errorf("with a record = %+v", l)
	}
	record(t, ScopeUser, Record{})
	fallback, _ := Fallback()
	if l, _ := Resolve(""); l.From != FromFallback || l.Dir != fallback {
		t.Errorf("with nothing recorded = %+v, want %s", l, fallback)
	}
}

func TestResolveSeveralAsks(t *testing.T) {
	isolateRecords(t)
	a, b := managedAt(t, t.TempDir()), managedAt(t, t.TempDir())
	record(t, ScopeUser, Record{Instances: []string{a, b}})
	_, err := Resolve("")
	var several *SeveralError
	if !errors.As(err, &several) || !slices.Equal(several.Dirs, []string{a, b}) {
		t.Errorf("err = %v", err)
	}
}

func TestOnlyRecordSaysSo(t *testing.T) {
	isolateRecords(t)
	dir := managedAt(t, t.TempDir())
	record(t, ScopeUser, Record{Instances: []string{dir}})
	l, err := Resolve("")
	if err != nil || l.Dir != dir || l.Why() != "the only instance rexctl remembers" {
		t.Errorf("Resolve = %+v (%s), %v", l, l.Why(), err)
	}
}

func TestStaleDefaultUsesTheLiveOne(t *testing.T) {
	isolateRecords(t)
	old, live := managedAt(t, t.TempDir()), managedAt(t, t.TempDir())
	record(t, ScopeUser, Record{Default: old, Instances: []string{old, live}})
	if err := os.RemoveAll(old); err != nil {
		t.Fatal(err)
	}
	l, err := Resolve("")
	if err != nil || l.Dir != live || l.Stale != old {
		t.Errorf("Resolve = %+v, %v", l, err)
	}
}

func TestStaleDefaultWithSeveralListsThem(t *testing.T) {
	isolateRecords(t)
	old, a, b := managedAt(t, t.TempDir()), managedAt(t, t.TempDir()), managedAt(t, t.TempDir())
	record(t, ScopeUser, Record{Default: old, Instances: []string{old, a, b}})
	if err := os.RemoveAll(old); err != nil {
		t.Fatal(err)
	}
	_, err := Resolve("")
	var several *SeveralError
	if !errors.As(err, &several) || !slices.Equal(several.Dirs, []string{a, b}) || several.Stale != old {
		t.Errorf("err = %+v", err)
	}
}

func TestAllGoneFallsBackAndForgets(t *testing.T) {
	isolateRecords(t)
	dir := managedAt(t, t.TempDir())
	remember(t, ScopeUser, dir, false)
	if err := os.RemoveAll(dir); err != nil {
		t.Fatal(err)
	}
	l, err := Resolve("")
	if fb, _ := Fallback(); err != nil || l.From != FromFallback || l.Dir != fb || l.Stale != "" {
		t.Errorf("Resolve = %+v, %v", l, err)
	}
	if r := ReadRecord(ScopeUser); !r.Empty() {
		t.Errorf("record kept %+v", r)
	}
}

func TestOnlyAChosenStaleDefaultIsKept(t *testing.T) {
	isolateRecords(t)
	chosen := managedAt(t, t.TempDir())
	remember(t, ScopeUser, chosen, true)
	if err := os.RemoveAll(chosen); err != nil {
		t.Fatal(err)
	}
	l, err := Resolve("")
	if err != nil || l.From != FromFallback || l.Stale != chosen || !l.Chosen {
		t.Errorf("chosen: Resolve = %+v, %v", l, err)
	}
	if l, _ := Resolve(""); l.Stale != chosen {
		t.Errorf("the chosen default was forgotten: %+v", l)
	}

	auto, other := managedAt(t, t.TempDir()), managedAt(t, t.TempDir())
	record(t, ScopeUser, Record{})
	remember(t, ScopeUser, auto, false)
	remember(t, ScopeUser, other, false)
	if r := ReadRecord(ScopeUser); r.Default != auto || r.Used {
		t.Fatalf("record %+v", r)
	}
	if err := os.RemoveAll(auto); err != nil {
		t.Fatal(err)
	}
	if l, err := Resolve(""); err != nil || l.Chosen || l.Dir != other {
		t.Errorf("automatic: Resolve = %+v, %v", l, err)
	}
}

func TestRememberKeepsTheFirstAsDefault(t *testing.T) {
	isolateRecords(t)
	first, second := managedAt(t, t.TempDir()), managedAt(t, t.TempDir())
	remember(t, ScopeUser, first, false)
	remember(t, ScopeUser, second, false)
	if l, err := Resolve(""); err != nil || l.Dir != first {
		t.Errorf("Resolve = %+v, %v", l, err)
	}
}

func TestRememberTakesTheDefaultFromItsOwnScope(t *testing.T) {
	isolateRecords(t)
	machine := managedAt(t, t.TempDir())
	record(t, ScopeMachine, Record{Instances: []string{machine}})
	first, second := managedAt(t, t.TempDir()), managedAt(t, t.TempDir())
	remember(t, ScopeUser, first, false)
	remember(t, ScopeUser, second, false)
	r := ReadRecord(ScopeUser)
	if r.Default != first || !slices.Equal(r.Instances, []string{first, second}) {
		t.Errorf("user record = %+v", r)
	}
}

func TestRememberFirstKeepsStandardFolder(t *testing.T) {
	home := isolateRecords(t)
	standard := managedAt(t, filepath.Join(home, "fluxer"))
	other := managedAt(t, t.TempDir())
	if fb, _ := Fallback(); clean(fb) != standard {
		t.Skipf("the standard folder here is %s", fb)
	}
	if err := RememberFirst(ScopeUser, other); err != nil {
		t.Fatal(err)
	}
	if r := ReadRecord(ScopeUser); r.Default != standard || !slices.Equal(r.Instances, []string{standard, other}) {
		t.Errorf("record = %+v", r)
	}
}

func TestRememberFirstLooksOnlyAtItsOwnScope(t *testing.T) {
	isolateRecords(t)
	machine := managedAt(t, t.TempDir())
	record(t, ScopeMachine, Record{Default: machine, Instances: []string{machine}})
	mine := managedAt(t, t.TempDir())
	if err := RememberFirst(ScopeUser, mine); err != nil {
		t.Fatal(err)
	}
	if r := ReadRecord(ScopeUser); !slices.Equal(r.Instances, []string{mine}) {
		t.Errorf("user record = %+v", r)
	}
	if l, err := Resolve(""); err != nil || l.Dir != mine {
		t.Errorf("Resolve = %+v, %v", l, err)
	}
}

func TestStandardFolderComesBeforeTheMachineRecord(t *testing.T) {
	home := isolateRecords(t)
	if asRoot() {
		t.Skip("runs as root")
	}
	standard := managedAt(t, filepath.Join(home, "fluxer"))
	machine := managedAt(t, t.TempDir())
	record(t, ScopeMachine, Record{Default: machine, Instances: []string{machine}})
	if l, err := Resolve(""); err != nil || clean(l.Dir) != standard || l.From != FromFallback {
		t.Errorf("Resolve = %+v, %v", l, err)
	}
}

func TestMachineRecordSkipsInstancesYouCantRead(t *testing.T) {
	isolateRecords(t)
	if runtime.GOOS == "windows" || os.Geteuid() == 0 {
		t.Skip("needs file permissions that keep this account out")
	}
	theirs := managedAt(t, t.TempDir())
	record(t, ScopeMachine, Record{Default: theirs, Instances: []string{theirs}})
	if err := os.Chmod(filepath.Join(theirs, StateDirName), 0); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { os.Chmod(filepath.Join(theirs, StateDirName), 0o700) })
	if l, err := Resolve(""); err != nil || l.From != FromFallback {
		t.Errorf("Resolve = %+v, %v", l, err)
	}
}

func TestRootIgnoresTheUserRecord(t *testing.T) {
	isolateRecords(t)
	user := managedAt(t, t.TempDir())
	record(t, ScopeUser, Record{Default: user, Instances: []string{user}})
	actAsRoot(t)
	if l, err := Resolve(""); err != nil || l.From != FromFallback {
		t.Errorf("only a user record: Resolve = %+v, %v", l, err)
	}
	machine := managedAt(t, t.TempDir())
	if OwnScope() != ScopeMachine {
		t.Fatal("root records in the user scope")
	}
	remember(t, OwnScope(), machine, false)
	if l, err := Resolve(""); err != nil || l.Dir != machine || l.Scope != ScopeMachine {
		t.Errorf("Resolve = %+v, %v", l, err)
	}
	if err := Remember(ScopeUser, user, true); err == nil {
		t.Error("root wrote a user record")
	}
}

func TestRootRefusesRecordsOthersCanChange(t *testing.T) {
	isolateRecords(t)
	actAsRoot(t)
	dir := managedAt(t, t.TempDir())
	remember(t, ScopeMachine, dir, true)
	if l, err := Resolve(""); err != nil || l.Dir != dir {
		t.Fatalf("Resolve = %+v, %v", l, err)
	}
	if err := os.Chmod(MachineRecordPath, 0o666); err != nil {
		t.Fatal(err)
	}
	var untrusted *UntrustedError
	if _, err := Resolve(""); !errors.As(err, &untrusted) || untrusted.Path != MachineRecordPath {
		t.Errorf("writable record: err = %v", err)
	}
	if err := os.Chmod(MachineRecordPath, 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.Chmod(dir, 0o777); err != nil {
		t.Fatal(err)
	}
	if _, err := Resolve(""); !errors.As(err, &untrusted) || !untrusted.Folder || clean(untrusted.Path) != dir {
		t.Errorf("writable folder: err = %v", err)
	}
	rootUID = os.Getuid() + 1
	if _, err := Resolve(""); !errors.As(err, &untrusted) || untrusted.Folder {
		t.Errorf("record owned by another account: err = %v", err)
	}
}

func TestSymlinkRecordsTheSameInstance(t *testing.T) {
	isolateRecords(t)
	dir := managedAt(t, t.TempDir())
	link := filepath.Join(t.TempDir(), "link")
	if err := os.Symlink(dir, link); err != nil {
		t.Skip(err)
	}
	remember(t, ScopeUser, dir, false)
	remember(t, ScopeUser, link, false)
	if r := ReadRecord(ScopeUser); !slices.Equal(r.Instances, []string{dir}) {
		t.Errorf("record = %+v", r)
	}
}

func TestSameIgnoresCaseOnWindows(t *testing.T) {
	if runtime.GOOS != "windows" {
		t.Skip("only Windows paths ignore case")
	}
	if !same(`C:\Fluxer`, `c:\fluxer`) {
		t.Error("the same folder in another case counts as another instance")
	}
}

func TestRememberAtTheSameTimeKeepsEveryInstance(t *testing.T) {
	isolateRecords(t)
	var dirs []string
	for range 12 {
		dirs = append(dirs, managedAt(t, t.TempDir()))
	}
	var wg sync.WaitGroup
	errs := make(chan error, len(dirs))
	for _, d := range dirs {
		wg.Go(func() { errs <- Remember(ScopeUser, d, false) })
	}
	wg.Wait()
	close(errs)
	for err := range errs {
		if err != nil {
			t.Fatal(err)
		}
	}
	got := ReadRecord(ScopeUser).Instances
	for _, d := range dirs {
		if !slices.Contains(got, d) {
			t.Fatalf("lost %s, record has %d of %d", d, len(got), len(dirs))
		}
	}
}

func TestUserRecordLivesInHome(t *testing.T) {
	home := isolateRecords(t)
	want := map[string]string{
		"linux":   filepath.Join(home, ".config"),
		"darwin":  filepath.Join(home, "Library", "Application Support"),
		"windows": filepath.Join(home, "AppData"),
	}[runtime.GOOS]
	if want == "" {
		t.Skipf("no known config folder on %s", runtime.GOOS)
	}
	want = filepath.Join(want, "rexctl", RecordFileName)
	if got := RecordPath(ScopeUser); got != want || !strings.HasPrefix(got, home) {
		t.Fatalf("user record = %s, want %s", got, want)
	}
	dir := managedAt(t, t.TempDir())
	record(t, ScopeUser, Record{Instances: []string{dir}})
	if _, err := os.Stat(want); err != nil {
		t.Fatal(err)
	}
	if l, err := Resolve(""); err != nil || l.Dir != dir || l.Scope != ScopeUser {
		t.Errorf("Resolve = %+v, %v", l, err)
	}
}

func TestRootLooksInItsOwnHomeNotHOME(t *testing.T) {
	home := isolateRecords(t)
	actAsRoot(t)
	rootsHome := t.TempDir()
	prev := rootHome
	rootHome = func() (string, error) { return rootsHome, nil }
	t.Cleanup(func() { rootHome = prev })
	theirs := managedAt(t, filepath.Join(home, "fluxer"))
	if fb, err := Fallback(); err != nil || same(fb, theirs) {
		t.Fatalf("root followed $HOME to %s, %v", fb, err)
	}
	if l, err := Resolve(""); err != nil || same(l.Dir, theirs) {
		t.Fatalf("Resolve = %+v, %v", l, err)
	}
	mine := managedAt(t, filepath.Join(rootsHome, "fluxer"))
	if fb, err := Fallback(); err != nil || !same(fb, mine) {
		t.Fatalf("Fallback = %s, %v, want root's own %s", fb, err, mine)
	}
}
