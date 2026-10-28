// SPDX-License-Identifier: AGPL-3.0-or-later

package backup

import (
	"bytes"
	"errors"
	"os"
	"path/filepath"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const oldState = `{"schema": 2, "project": "fluxer", "release": "", "settings": {}, "rendered": {}}`

func oldLayoutDir(t *testing.T) (string, map[string][]byte) {
	t.Helper()
	dir := t.TempDir()
	files := map[string][]byte{
		stack.ComposeFile:      []byte("name: fluxer\nservices: {}\n"),
		stack.Caddyfile:        []byte(":80 {\n}\n"),
		instance.CustomCompose: []byte("services: {}\n"),
		filepath.Join(instance.CaddyDropInDir, "x.caddy"):            []byte("header X-Mine yes\n"),
		filepath.Join(instance.StateDirName, instance.StateFileName): []byte(oldState),
	}
	for name, data := range files {
		if err := os.MkdirAll(filepath.Dir(filepath.Join(dir, name)), 0o700); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join(dir, name), data, 0o644); err != nil {
			t.Fatal(err)
		}
	}
	return dir, files
}

func archiveOf(t *testing.T, inst *instance.Instance) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "backup.tar.zst")
	w, err := createArchive(path)
	if err != nil {
		t.Fatal(err)
	}
	if err := addInstanceFiles(w, inst); err != nil {
		w.abort()
		t.Fatal(err)
	}
	if err := w.finish(&Manifest{Project: "fluxer"}); err != nil {
		t.Fatal(err)
	}
	return path
}

func restoredFrom(t *testing.T, archive string) *instance.Instance {
	t.Helper()
	dir := t.TempDir()
	if err := extractFiles(archive, dir); err != nil {
		t.Fatal(err)
	}
	inst, err := instance.Find(dir)
	if err != nil {
		t.Fatal(err)
	}
	return inst
}

func movedInstance(t *testing.T, dir string) *instance.Instance {
	t.Helper()
	inst, err := instance.Find(dir)
	if err != nil {
		t.Fatal(err)
	}
	if err := inst.Migrate(nil); err != nil {
		t.Fatal(err)
	}
	return inst
}

func TestOldBackupRestoresOnTheOldLayout(t *testing.T) {
	dir, files := oldLayoutDir(t)
	inst := restoredFrom(t, archiveOf(t, &instance.Instance{Dir: dir}))
	if !inst.OldLayout() {
		t.Fatal("an old backup restored as the new layout")
	}
	for _, name := range []string{stack.ComposeFile, stack.Caddyfile} {
		if inst.Generated(name) != inst.Path(name) {
			t.Fatalf("%s resolves to %s", name, inst.Generated(name))
		}
		got, err := os.ReadFile(inst.Path(name))
		if err != nil || !bytes.Equal(got, files[name]) {
			t.Fatalf("%s: %q, %v", name, got, err)
		}
	}
	moved := movedInstance(t, inst.Dir)
	for _, name := range []string{stack.ComposeFile, stack.Caddyfile} {
		got, err := os.ReadFile(moved.Generated(name))
		if err != nil || !bytes.Equal(got, files[name]) {
			t.Fatalf("%s after moving: %q, %v", name, got, err)
		}
	}
	for _, name := range []string{instance.CustomCompose, filepath.Join(instance.CaddyDropInDir, "x.caddy")} {
		if got, err := os.ReadFile(inst.Path(name)); err != nil || !bytes.Equal(got, files[name]) {
			t.Fatalf("%s: %q, %v", name, got, err)
		}
	}
}

func TestNewBackupKeepsGeneratedFolder(t *testing.T) {
	dir, files := oldLayoutDir(t)
	inst := restoredFrom(t, archiveOf(t, movedInstance(t, dir)))
	if got, err := os.ReadFile(inst.Generated(stack.ComposeFile)); err != nil || !bytes.Equal(got, files[stack.ComposeFile]) {
		t.Fatalf("compose file: %q, %v", got, err)
	}
	if _, err := os.Stat(inst.Path(stack.ComposeFile)); !errors.Is(err, os.ErrNotExist) {
		t.Fatal("a compose file showed up in the instance folder")
	}
}

func TestClearingAnInstanceRemovesGeneratedFolder(t *testing.T) {
	dir, _ := oldLayoutDir(t)
	movedInstance(t, dir)
	if err := clearInstanceFiles(dir); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(stack.GeneratedPath(dir)); !errors.Is(err, os.ErrNotExist) {
		t.Fatal("generated files stayed behind before the restore")
	}
}
