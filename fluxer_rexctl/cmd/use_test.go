// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func TestMain(m *testing.M) {
	home, err := os.MkdirTemp("", "rexctl-home")
	if err != nil {
		panic(err)
	}
	os.Setenv("HOME", home)
	os.Setenv("XDG_CONFIG_HOME", filepath.Join(home, ".config"))
	os.Setenv("AppData", filepath.Join(home, "AppData"))
	instance.MachineRecordPath = filepath.Join(home, "etc", "rexctl", instance.RecordFileName)
	code := m.Run()
	os.RemoveAll(home)
	os.Exit(code)
}

func clearRecords(t *testing.T) {
	t.Helper()
	for _, s := range []instance.Scope{instance.ScopeUser, instance.ScopeMachine} {
		if err := instance.SaveRecord(s, instance.Record{}); err != nil {
			t.Fatal(err)
		}
	}
	t.Cleanup(func() {
		for _, s := range []instance.Scope{instance.ScopeUser, instance.ScopeMachine} {
			_ = instance.SaveRecord(s, instance.Record{})
		}
	})
}

func recordApp(t *testing.T) (*App, string) {
	t.Helper()
	clearRecords(t)
	inst, err := instance.Create(t.TempDir(), "fluxer")
	if err != nil {
		t.Fatal(err)
	}
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	out := &bytes.Buffer{}
	return &App{
		Flags: GlobalFlags{Dir: inst.Dir},
		UI:    ui.New(ui.Options{Stdin: strings.NewReader(""), Stdout: out, Stderr: out}),
	}, inst.Dir
}

func resolved(t *testing.T, dir string) string {
	t.Helper()
	real, err := filepath.EvalSymlinks(dir)
	if err != nil {
		t.Fatal(err)
	}
	return real
}

func TestFirstUseRecordsInstance(t *testing.T) {
	app, dir := recordApp(t)
	if _, err := app.Instance(); err != nil {
		t.Fatal(err)
	}
	if got := instance.ReadRecord(instance.OwnScope()).Instances; !slices.Equal(got, []string{resolved(t, dir)}) {
		t.Errorf("recorded = %v, want %s", got, dir)
	}
}

func TestForgetRemovesInstance(t *testing.T) {
	app, dir := recordApp(t)
	if _, err := app.Instance(); err != nil {
		t.Fatal(err)
	}
	app.forget(dir)
	if r := instance.ReadRecord(instance.OwnScope()); !r.Empty() {
		t.Errorf("recorded = %+v, want nothing", r)
	}
}

func TestRememberWithDirKeepsStandardFolder(t *testing.T) {
	app, dir := recordApp(t)
	home, _ := os.UserHomeDir()
	standard := filepath.Join(home, "fluxer")
	inst, err := instance.Create(standard, "fluxer")
	if err != nil {
		t.Fatal(err)
	}
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { os.RemoveAll(standard) })
	if fb, _ := instance.Fallback(); fb != standard {
		t.Skipf("the standard folder here is %s", fb)
	}
	app.remember(dir)
	t.Chdir(t.TempDir())
	l, err := instance.Resolve("")
	if err != nil || l.Dir != resolved(t, standard) || l.From != instance.FromRecord {
		t.Errorf("Resolve = %+v, %v", l, err)
	}
}

func TestRerunKeepsThePickedInstance(t *testing.T) {
	app, dir := recordApp(t)
	app.Flags.Dir = ""
	app.located = &instance.Located{Dir: dir, From: instance.FromPicked}
	args := resumeArgs([]string{"upgrade", "--yes"}, release.PhaseSelfUpdated, app.pickedDir())
	if !slices.Equal(args, []string{"upgrade", "--yes", "--dir", dir, "--" + release.ResumeFlag, release.PhaseSelfUpdated}) {
		t.Errorf("resume args %q", args)
	}
	if got := app.takeOverCommand(); got != "rexctl takeover --dir "+dir {
		t.Errorf("takeover command %q", got)
	}
	app.located = &instance.Located{Dir: dir, From: instance.FromWorkdir}
	if args := resumeArgs([]string{"upgrade"}, release.PhaseSelfUpdated, app.pickedDir()); slices.Contains(args, "--dir") {
		t.Errorf("resume args %q added --dir for the current folder", args)
	}
}

func TestUseOnlyChangesTheRecord(t *testing.T) {
	clearRecords(t)
	app, inst, out := oldInstance(t, true)
	tree := func() []string {
		var files []string
		_ = filepath.WalkDir(inst.Dir, func(path string, _ os.DirEntry, err error) error {
			files = append(files, path)
			return err
		})
		return files
	}
	before := tree()
	if err := runUseShow(app); err != nil {
		t.Fatalf("%v\n%s", err, out)
	}
	if err := runUse(app, inst.Dir); err != nil {
		t.Fatalf("%v\n%s", err, out)
	}
	if after := tree(); !slices.Equal(before, after) {
		t.Fatalf("rexctl use changed the instance folder:\n%v\n%v", before, after)
	}
	requireOldLayout(t, inst)
}
