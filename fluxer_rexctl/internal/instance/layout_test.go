// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"maps"
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

var userFiles = map[string]string{
	CustomCompose:                            "services:\n  edge:\n    labels:\n      team: chat\n",
	filepath.Join(CaddyDropInDir, "x.caddy"): "header X-Mine yes\n",
	filepath.Join(CertsDir, "site.pem"):      "pem\n",
}

func oldLayout(t *testing.T) (string, map[string][]byte) {
	t.Helper()
	dir := t.TempDir()
	set := stack.Embedded()
	refs := fluxerRefs("a")
	s := &State{
		Project:  "fluxer",
		Release:  set.Release,
		Images:   Images{Release: set.Release, Refs: refs},
		Settings: map[string]string{settings.Domain: "chat.example.com"},
		Secrets:  map[string]string{},
	}
	if _, err := secrets.Mint(s.Secrets, false); err != nil {
		t.Fatal(err)
	}
	managed, err := stack.Files(set, nil, refs, "fluxer")
	if err != nil {
		t.Fatal(err)
	}
	if s.Rendered, err = stack.Hashes(set, nil, refs, "fluxer"); err != nil {
		t.Fatal(err)
	}
	files := map[string][]byte{}
	for name, data := range managed {
		files[name] = stack.Plain(name, data)
		if stack.Hash(files[name]) != s.Rendered[name] {
			t.Fatalf("the old layout fixture of %s doesn't match what older rexctl wrote", name)
		}
	}
	for name, data := range userFiles {
		files[name] = []byte(data)
	}
	for name, data := range files {
		writeTestFile(t, filepath.Join(dir, name), data)
	}
	if err := os.MkdirAll(filepath.Join(dir, StateDirName), 0o700); err != nil {
		t.Fatal(err)
	}
	s.rootLayout = true
	if err := s.Save(filepath.Join(dir, StateDirName, StateFileName)); err != nil {
		t.Fatal(err)
	}
	return dir, files
}

func writeTestFile(t *testing.T, path string, data []byte) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, data, 0o644); err != nil {
		t.Fatal(err)
	}
}

func readTestFile(t *testing.T, path string) []byte {
	t.Helper()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	return data
}

func diskSchema(t *testing.T, dir string) int {
	t.Helper()
	var head struct{ Schema int }
	if err := json.Unmarshal(readTestFile(t, filepath.Join(dir, StateDirName, StateFileName)), &head); err != nil {
		t.Fatal(err)
	}
	return head.Schema
}

func migrated(t *testing.T, dir string, old map[string][]byte) *Instance {
	t.Helper()
	inst, err := Find(dir)
	if err != nil {
		t.Fatal(err)
	}
	if err := inst.Migrate(nil); err != nil {
		t.Fatal(err)
	}
	return checkMigrated(t, dir, old)
}

func checkMigrated(t *testing.T, dir string, old map[string][]byte) *Instance {
	t.Helper()
	inst, err := Find(dir)
	if err != nil {
		t.Fatal(err)
	}
	if inst.OldLayout() {
		t.Fatal("the instance is still on the old layout")
	}
	for _, name := range stack.FileNames() {
		want, ok := old[name]
		if !ok {
			continue
		}
		if got := readTestFile(t, inst.Generated(name)); !bytes.Equal(got, want) {
			t.Fatalf("%s changed while moving", name)
		}
		_, err := os.Lstat(inst.Path(name))
		if _, mounted := stack.MountedFiles[name]; !mounted && !errors.Is(err, os.ErrNotExist) {
			t.Fatalf("%s is still in the instance folder: %v", name, err)
		}
	}
	for name, data := range userFiles {
		if got := readTestFile(t, inst.Path(name)); string(got) != data {
			t.Fatalf("%s changed", name)
		}
	}
	if s := diskSchema(t, dir); s != StateSchema {
		t.Fatalf("schema on disk %d, want %d", s, StateSchema)
	}
	r, err := Drift(inst)
	if err != nil || len(r.Changed) > 0 || len(r.Missing) > 0 {
		t.Fatalf("drift after moving: %+v %v", r, err)
	}
	for _, f := range inst.ComposeFiles() {
		if f != inst.Path(CustomCompose) && !strings.HasPrefix(f, stack.GeneratedPath(dir)) {
			t.Fatalf("compose file %s isn't in %s", f, stack.GeneratedDir)
		}
	}
	return inst
}

func checkOldLayout(t *testing.T, dir string, old map[string][]byte) {
	t.Helper()
	if s := diskSchema(t, dir); s != 2 {
		t.Fatalf("schema on disk %d, want 2", s)
	}
	for name, want := range old {
		if got := readTestFile(t, filepath.Join(dir, name)); !bytes.Equal(got, want) {
			t.Fatalf("%s changed", name)
		}
	}
}

func TestFindLeavesOldLayout(t *testing.T) {
	dir, old := oldLayout(t)
	inst, err := Find(dir)
	if err != nil {
		t.Fatal(err)
	}
	if !inst.OldLayout() {
		t.Fatal("an older rexctl's instance loaded as the new layout")
	}
	checkOldLayout(t, dir, old)
	if _, err := os.Stat(stack.GeneratedPath(dir)); !errors.Is(err, os.ErrNotExist) {
		t.Fatal("loading the instance made the generated folder")
	}
	r, err := Drift(inst)
	if err != nil || len(r.Changed) > 0 || len(r.Missing) > 0 || len(inst.MissingFiles()) > 0 {
		t.Fatalf("drift on the old layout: %+v %v %v", r, err, inst.MissingFiles())
	}
	for _, f := range inst.ComposeFiles() {
		if filepath.Dir(f) != dir {
			t.Fatalf("compose file %s isn't in the instance folder", f)
		}
	}
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	checkOldLayout(t, dir, old)
}

func TestFindWorksWhileLockedAndReadOnly(t *testing.T) {
	dir, old := oldLayout(t)
	l, err := (&Instance{Dir: dir}).Lock(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	defer l.Release()
	if runtime.GOOS != "windows" && os.Geteuid() != 0 {
		if err := os.Chmod(filepath.Join(dir, StateDirName), 0o500); err != nil {
			t.Fatal(err)
		}
		t.Cleanup(func() { os.Chmod(filepath.Join(dir, StateDirName), 0o700) })
	}
	if _, err := Find(dir); err != nil {
		t.Fatalf("Find on an old layout another command holds: %v", err)
	}
	checkOldLayout(t, dir, old)
}

func TestOldLayoutRendersInPlace(t *testing.T) {
	dir, old := oldLayout(t)
	inst, err := Find(dir)
	if err != nil {
		t.Fatal(err)
	}
	inst.State.Settings[settings.EdgeMode] = settings.EdgeProxy
	r, err := inst.Render(stack.Embedded(), inst.Overlays(), inst.State.Images.Refs)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(stack.GeneratedPath(dir)); !errors.Is(err, os.ErrNotExist) {
		t.Fatal("rendering the old layout wrote the generated folder")
	}
	for name, hash := range r.Hashes {
		data := readTestFile(t, filepath.Join(dir, name))
		if stack.Hash(data) != hash || bytes.HasPrefix(data, []byte("# rexctl writes this file")) {
			t.Fatalf("%s isn't what an older rexctl writes", name)
		}
	}
	if !bytes.Equal(readTestFile(t, filepath.Join(dir, stack.ComposeFile)), old[stack.ComposeFile]) {
		t.Fatal("the compose file changed")
	}
}

func TestMigrateMovesOldLayout(t *testing.T) {
	dir, old := oldLayout(t)
	migrated(t, dir, old)
	if got := readTestFile(t, filepath.Join(dir, stack.Caddyfile)); !bytes.Equal(got, old[stack.Caddyfile]) {
		t.Fatal("the Caddyfile the running edge mounts went away before the next start")
	}
}

func TestMigrateOnce(t *testing.T) {
	dir, old := oldLayout(t)
	inst := migrated(t, dir, old)
	before := readTestFile(t, filepath.Join(dir, StateDirName, StateFileName))
	if err := inst.Migrate(func() error { return errors.New("ran again") }); err != nil {
		t.Fatal(err)
	}
	if after := readTestFile(t, filepath.Join(dir, StateDirName, StateFileName)); !bytes.Equal(before, after) {
		t.Fatal("a second migration changed the state again")
	}
}

func TestMigrateNoticesAnotherProcessMoved(t *testing.T) {
	dir, old := oldLayout(t)
	stale, err := Find(dir)
	if err != nil {
		t.Fatal(err)
	}
	migrated(t, dir, old)
	if err := stale.Migrate(func() error { return errors.New("ran again") }); err != nil {
		t.Fatal(err)
	}
	if err := stale.Save(); err != nil {
		t.Fatal(err)
	}
	checkMigrated(t, dir, old)
}

func TestMigrateStopsBeforeSavingWhenPrepareFails(t *testing.T) {
	dir, old := oldLayout(t)
	inst, err := Find(dir)
	if err != nil {
		t.Fatal(err)
	}
	var schemaThen int
	err = inst.Migrate(func() error {
		schemaThen = diskSchema(t, dir)
		return errors.New("no copy of rexctl")
	})
	if err == nil || schemaThen != 2 {
		t.Fatalf("got %v with schema %d while preparing", err, schemaThen)
	}
	if !inst.OldLayout() {
		t.Fatal("the instance switched layouts without its files")
	}
	checkOldLayout(t, dir, old)
}

func TestMigrateAfterCrashBeforeSave(t *testing.T) {
	dir, old := oldLayout(t)
	inst, err := Find(dir)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := copyGenerated(inst); err != nil {
		t.Fatal(err)
	}
	checkOldLayout(t, dir, old)
	again, err := Find(dir)
	if err != nil || !again.OldLayout() {
		t.Fatalf("after the crash: %v", err)
	}
	migrated(t, dir, old)
}

func TestMigrateAsRootGivesFilesToOwner(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("no owners on Windows")
	}
	dir, _ := oldLayout(t)
	inst, err := Find(dir)
	if err != nil {
		t.Fatal(err)
	}
	owner, _ := inst.OwnerUID()
	prevUID, prevChown := euid, chown
	t.Cleanup(func() { euid, chown = prevUID, prevChown })
	euid = func() int { return 0 }
	given := map[string]int{}
	chown = func(path string, uid, _ int) error {
		given[path] = uid
		return nil
	}
	if owner == 0 {
		t.Skip("the test folder belongs to root")
	}
	if err := inst.Migrate(nil); err != nil {
		t.Fatal(err)
	}
	for _, path := range []string{stack.GeneratedPath(dir), inst.Generated(stack.ComposeFile), inst.Generated(stack.Caddyfile)} {
		if uid, ok := given[path]; !ok || uid != owner {
			t.Fatalf("%s wasn't given to the folder's owner: %v", path, given)
		}
	}
}

func startedWithGenerated(t *testing.T, inst *Instance) {
	t.Helper()
	compose := inst.Generated(stack.ComposeFile)
	writeTestFile(t, compose, stack.Managed(stack.ComposeFile, stack.Plain(stack.ComposeFile, readTestFile(t, compose))))
	if err := inst.DropRootCopies(context.Background(), true); err != nil {
		t.Fatal(err)
	}
}

func TestDropRootCopiesRemovesOnlyWhatMigrationLeft(t *testing.T) {
	dir, old := oldLayout(t)
	inst := migrated(t, dir, old)
	root := inst.Path(stack.Caddyfile)
	if err := inst.DropRootCopies(context.Background(), true); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(root); err != nil {
		t.Fatal("removed the Caddyfile an older compose file still mounts")
	}
	startedWithGenerated(t, inst)
	if _, err := os.Stat(root); !errors.Is(err, os.ErrNotExist) {
		t.Fatal("kept the Caddyfile the migration left after edge moved to the generated one")
	}
	again, err := Find(dir)
	if err != nil || len(again.State.LeftInRoot) != 0 {
		t.Fatalf("the state still lists %v, %v", again.State.LeftInRoot, err)
	}
}

func TestDropRootCopiesKeepsYourCaddyfile(t *testing.T) {
	for name, change := range map[string]func(t *testing.T, inst *Instance){
		"mounted from custom.compose.yml": func(t *testing.T, inst *Instance) {
			writeTestFile(t, inst.Path(CustomCompose), []byte("services:\n  edge:\n    volumes:\n      - ./Caddyfile:/etc/caddy/Caddyfile:ro\n"))
		},
		"mounted from ${PWD}": func(t *testing.T, inst *Instance) {
			writeTestFile(t, inst.Path(CustomCompose), []byte("services:\n  edge:\n    volumes:\n      - ${PWD}/Caddyfile:/etc/caddy/Caddyfile:ro\n"))
		},
		"mounted with the long syntax": func(t *testing.T, inst *Instance) {
			writeTestFile(t, inst.Path(CustomCompose), []byte("services:\n  edge:\n    volumes:\n      - type: bind\n        source: Caddyfile\n        target: /etc/caddy/Caddyfile\n"))
		},
		"custom.compose.yml that doesn't load": func(t *testing.T, inst *Instance) {
			writeTestFile(t, inst.Path(CustomCompose), []byte("services: [\n"))
		},
		"changed by you": func(t *testing.T, inst *Instance) {
			writeTestFile(t, inst.Path(stack.Caddyfile), []byte(":443 {\n}\n"))
		},
		"never left by rexctl": func(t *testing.T, inst *Instance) {
			inst.State.LeftInRoot = nil
		},
	} {
		t.Run(name, func(t *testing.T) {
			dir, old := oldLayout(t)
			inst := migrated(t, dir, old)
			change(t, inst)
			startedWithGenerated(t, inst)
			if _, err := os.Stat(inst.Path(stack.Caddyfile)); err != nil {
				t.Fatalf("removed your Caddyfile: %v", err)
			}
		})
	}
}

func TestDropRootCopiesLooksAtWhatCustomComposeMounts(t *testing.T) {
	dir, old := oldLayout(t)
	inst := migrated(t, dir, old)
	writeTestFile(t, inst.Path(CustomCompose), []byte("services:\n  edge:\n    volumes:\n      - ${PWD}/caddy.d/x.caddy:/etc/caddy/x.caddy:ro\n      - ./Caddyfile.mine:/etc/caddy/mine:ro\n"))
	startedWithGenerated(t, inst)
	if _, err := os.Stat(inst.Path(stack.Caddyfile)); !errors.Is(err, os.ErrNotExist) {
		t.Fatal("kept the Caddyfile though custom.compose.yml mounts other files")
	}
}

func TestUnusedRootFilesNamesAnEditedCaddyfile(t *testing.T) {
	dir, old := oldLayout(t)
	inst := migrated(t, dir, old)
	ctx := context.Background()
	writeTestFile(t, inst.Path(stack.Caddyfile), []byte(":443 {\n}\n"))
	if got := inst.UnusedRootFiles(ctx); len(got) != 0 {
		t.Fatalf("the edge still mounts the root Caddyfile, but got %v", got)
	}
	startedWithGenerated(t, inst)
	if got := inst.UnusedRootFiles(ctx); !slices.Equal(got, []string{stack.Caddyfile}) {
		t.Fatalf("UnusedRootFiles = %v", got)
	}
	writeTestFile(t, inst.Path(CustomCompose), []byte("services:\n  edge:\n    volumes:\n      - type: bind\n        source: Caddyfile\n        target: /etc/caddy/Caddyfile\n"))
	if got := inst.UnusedRootFiles(ctx); len(got) != 0 {
		t.Fatalf("custom.compose.yml mounts it, but got %v", got)
	}
}

func TestDropRootCopiesLeavesTheOldLayoutAlone(t *testing.T) {
	dir, old := oldLayout(t)
	inst, err := Find(dir)
	if err != nil {
		t.Fatal(err)
	}
	inst.State.LeftInRoot = map[string]string{}
	for _, name := range []string{stack.ComposeFile, stack.Caddyfile} {
		inst.State.LeftInRoot[name] = stack.Hash(old[name])
	}
	if err := inst.DropRootCopies(context.Background(), true); err != nil {
		t.Fatal(err)
	}
	checkOldLayout(t, dir, old)
}

func TestEjectAfterMoveGivesTheOldFiles(t *testing.T) {
	dir, old := oldLayout(t)
	inst := migrated(t, dir, old)
	if err := Eject(inst, EjectOptions{Rexctl: "test"}); err != nil {
		t.Fatal(err)
	}
	for _, name := range stack.FileNames() {
		want, ok := old[name]
		if !ok {
			continue
		}
		if got := readTestFile(t, inst.Path(name)); !bytes.Equal(got, want) {
			t.Fatalf("ejected %s differs from the file older rexctl left", name)
		}
		_, err := os.Stat(inst.Generated(name))
		if _, mounted := stack.MountedFiles[name]; !mounted && !errors.Is(err, os.ErrNotExist) {
			t.Fatalf("%s stayed in %s after eject", name, stack.GeneratedDir)
		}
	}
	env, err := envfile.Load(inst.Path(EnvFileName))
	if err != nil {
		t.Fatal(err)
	}
	if got, want := env.Value("COMPOSE_FILE"), stack.ComposeFile+string(os.PathListSeparator)+CustomCompose; got != want {
		t.Fatalf("COMPOSE_FILE = %q, want %q", got, want)
	}
	again, err := Find(dir)
	if err != nil || again.State.Ejected == nil {
		t.Fatalf("not ejected: %v", err)
	}
	if _, err := os.Stat(inst.Generated(stack.ComposeFile)); !errors.Is(err, os.ErrNotExist) {
		t.Fatal("loading an ejected instance moved its files")
	}

	s, r, err := Import(dir, nil)
	if err != nil {
		t.Fatal(err)
	}
	if len(r.KeepAsMine) > 0 || s.Ejected != nil {
		t.Fatalf("takeover kept %v", r.KeepAsMine)
	}
	checkOldLayout(t, dir, old)
	migrated(t, dir, old)
}

func TestFreshLayoutLoadsWithTemplate(t *testing.T) {
	inst, err := Create(t.TempDir(), "fluxer")
	if err != nil {
		t.Fatal(err)
	}
	set := stack.Embedded()
	inst.State.Release = set.Release
	inst.State.Images = Images{Release: set.Release, Refs: fluxerRefs("a")}
	inst.State.Settings[settings.Domain] = "chat.example.com"
	if _, err := secrets.Mint(inst.State.Secrets, false); err != nil {
		t.Fatal(err)
	}
	r, err := stack.Render(set, nil, inst.State.Images.Refs, "fluxer", inst.Dir)
	if err != nil {
		t.Fatal(err)
	}
	inst.State.Rendered = r.Hashes
	if err := inst.WriteCustomTemplate(); err != nil {
		t.Fatal(err)
	}
	if got := string(readTestFile(t, inst.Path(CustomCompose))); got != CustomTemplate {
		t.Fatalf("template %q", got)
	}
	mine := []byte("services: {}\n")
	writeTestFile(t, inst.Path(CustomCompose), mine)
	if err := inst.WriteCustomTemplate(); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(readTestFile(t, inst.Path(CustomCompose)), mine) {
		t.Fatal("the template replaced the file you wrote")
	}
	writeTestFile(t, inst.Path(CustomCompose), []byte(CustomTemplate))
	for _, name := range slices.Sorted(maps.Keys(r.Hashes)) {
		if !bytes.HasPrefix(readTestFile(t, inst.Generated(name)), []byte("# rexctl writes this file and puts its own version back")) {
			t.Fatalf("%s has no header", name)
		}
		if _, err := os.Stat(inst.Path(name)); !errors.Is(err, os.ErrNotExist) {
			t.Fatalf("%s is in the instance folder", name)
		}
	}
	env, err := inst.Environment()
	if err != nil {
		t.Fatal(err)
	}
	var none engine.Engine
	p, err := none.Load(context.Background(), engine.Spec{Name: "fluxer", Dir: inst.Dir, Files: inst.ComposeFiles(), Env: env})
	if err != nil {
		t.Fatal(err)
	}
	if p.Model.WorkingDir != inst.Dir {
		t.Fatalf("project folder %s, want %s", p.Model.WorkingDir, inst.Dir)
	}
	var sources []string
	for _, v := range p.Model.Services["edge"].Volumes {
		sources = append(sources, v.Source)
	}
	for _, want := range []string{inst.Generated(stack.Caddyfile), inst.Path(CertsDir), inst.Path(CaddyDropInDir)} {
		if !slices.Contains(sources, want) {
			t.Fatalf("edge mounts %v, want %s", sources, want)
		}
	}
}

func TestOlderRexctlRefusesMovedState(t *testing.T) {
	dir, old := oldLayout(t)
	migrated(t, dir, old)
	data := readTestFile(t, filepath.Join(dir, StateDirName, StateFileName))
	older := strings.Replace(string(data), `"schema": 3`, `"schema": 4`, 1)
	if _, err := ParseState([]byte(older), dir); !errors.Is(err, ErrNewerState) {
		t.Fatalf("got %v, want ErrNewerState", err)
	}
	if !bytes.Contains(data, []byte(`"schema": 3`)) {
		t.Fatal("the moved state isn't schema 3, so rexctl with schema 2 would read it")
	}
}

func TestKeepEditsKeepsOneCopyWithDiff(t *testing.T) {
	dir, old := oldLayout(t)
	inst := migrated(t, dir, old)
	path := inst.Generated(stack.ComposeFile)
	writeTestFile(t, path, append(readTestFile(t, path), []byte("# my change\n")...))
	edits, err := KeepEdits(inst)
	if err != nil || len(edits) != 1 {
		t.Fatalf("edits %v, %v", edits, err)
	}
	if !slices.Contains(edits[0].Diff, "+ # my change") {
		t.Fatalf("diff %q", edits[0].Diff)
	}
	if !strings.HasPrefix(edits[0].Copy, inst.Path(StateDirName, EditedDir)) || !bytes.Equal(readTestFile(t, edits[0].Copy), readTestFile(t, path)) {
		t.Fatalf("copy %s", edits[0].Copy)
	}
	again, err := KeepEdits(inst)
	if err != nil || len(again) != 1 || again[0].Copy != edits[0].Copy {
		t.Fatalf("second copy %v, %v", again, err)
	}
}

func TestEjectOnTheOldLayoutKeepsTheFiles(t *testing.T) {
	dir, old := oldLayout(t)
	inst, err := Find(dir)
	if err != nil {
		t.Fatal(err)
	}
	if err := Eject(inst, EjectOptions{Rexctl: "test"}); err != nil {
		t.Fatal(err)
	}
	checkOldLayout(t, dir, old)
}
