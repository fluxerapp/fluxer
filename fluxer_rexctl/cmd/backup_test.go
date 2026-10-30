// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"context"
	"encoding/binary"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func TestBackupCompressErrorKeepsInstanceUp(t *testing.T) {
	err := backupCreateErr(&backup.CompressError{Err: errors.New("no space left on device")})
	var ee *ui.ExitError
	if !errors.As(err, &ee) {
		t.Fatalf("backupCreateErr returned %T, want a ui.ExitError", err)
	}
	if ee.Code == ui.ExitInterrupted {
		t.Fatal("a compression failure is not a cancellation")
	}
	if !strings.Contains(ee.Msg, "running again") {
		t.Fatalf("message = %q, want it to say Fluxer is running again", ee.Msg)
	}
}

func TestBackupCompressingStageHasTitle(t *testing.T) {
	if backupStageTitle(backup.StageCompressing) == "" {
		t.Fatal("the compressing stage has no progress title")
	}
}

func writeTestBackup(t *testing.T, inst *instance.Instance, release string, at time.Time, scope string) string {
	t.Helper()
	m := backup.Manifest{Format: 1, Release: release, Project: inst.Project(), Domain: inst.Domain(), CreatedAt: at, Reason: "upgrade", Scope: scope, Volumes: []string{}}
	if scope != "" {
		m.Format = backup.FormatVersion
	}
	return writeManifestBackup(t, inst, m)
}

func writeManifestBackup(t *testing.T, inst *instance.Instance, m backup.Manifest) string {
	t.Helper()
	at := m.CreatedAt
	data, err := json.Marshal(m)
	if err != nil {
		t.Fatal(err)
	}
	var b bytes.Buffer
	b.Write(binary.LittleEndian.AppendUint32(nil, 0x184D2A5E))
	b.Write(binary.LittleEndian.AppendUint32(nil, uint32(len(data)+16)))
	b.Write(data)
	b.Write(binary.LittleEndian.AppendUint64(nil, uint64(len(data))))
	b.WriteString("RXBKMAN1")
	dir := inst.BackupDir()
	if err := os.MkdirAll(dir, 0o700); err != nil {
		t.Fatal(err)
	}
	path := filepath.Join(dir, "fluxer-test-"+at.UTC().Format("20060102-150405")+backup.Extension)
	if err := os.WriteFile(path, b.Bytes(), 0o600); err != nil {
		t.Fatal(err)
	}
	return path
}

func backupApp(t *testing.T) (*App, *instance.Instance, *bytes.Buffer) {
	t.Helper()
	app, _ := notifyRun(t, false)
	inst, err := instance.Find(app.Flags.Dir)
	if err != nil {
		t.Fatal(err)
	}
	return app, inst, app.UI.Stdout().(*bytes.Buffer)
}

func remaining(t *testing.T, inst *instance.Instance) []string {
	t.Helper()
	list, err := backup.List(inst.BackupDir())
	if err != nil {
		t.Fatal(err)
	}
	var out []string
	for _, b := range list {
		out = append(out, b.Path)
	}
	return out
}

func TestPruneKeepsNewestBackupWithUploads(t *testing.T) {
	app, inst, out := backupApp(t)
	inst.State.KeepBackups = 2
	day := time.Date(2026, 10, 1, 4, 0, 0, 0, time.UTC)
	full := writeTestBackup(t, inst, "2026.1001.100000", day, "")
	older := writeTestBackup(t, inst, "2026.1001.100000", day.Add(-24*time.Hour), "")
	var lean []string
	for i := 1; i <= 4; i++ {
		lean = append(lean, writeTestBackup(t, inst, "2026.1001.100000", day.Add(time.Duration(i)*7*24*time.Hour), backup.ScopeWithoutUploads))
	}
	pruneBackups(app, inst)
	got := remaining(t, inst)
	if !slices.Contains(got, full) {
		t.Errorf("the newest backup with uploads was deleted, left %v", got)
	}
	if slices.Contains(got, older) || slices.Contains(got, lean[0]) || slices.Contains(got, lean[1]) {
		t.Errorf("older backups were kept, left %v", got)
	}
	if !slices.Contains(got, lean[3]) || !slices.Contains(got, lean[2]) {
		t.Errorf("the newest backups were deleted, left %v", got)
	}
	want := "Deleted 3 older backups to keep the newest 2. rexctl also keeps the newest backup with uploads and the one rexctl rollback needs."
	if !strings.Contains(out.String(), want) {
		t.Errorf("prune says %q, want %q", out.String(), want)
	}
	out.Reset()
	writeTestBackup(t, inst, "2026.1001.100000", day.Add(5*7*24*time.Hour), "")
	pruneBackups(app, inst)
	if s := out.String(); !strings.Contains(s, "Deleted 2 older backups to keep the newest 2.") || strings.Contains(s, "also keeps") {
		t.Errorf("with nothing kept beyond the count, prune says %q", s)
	}
}

func TestPruneKeepsBackupForRealPreviousRelease(t *testing.T) {
	app, inst, _ := backupApp(t)
	inst.State.KeepBackups = 1
	day := time.Date(2026, 10, 1, 4, 0, 0, 0, time.UTC)
	stale := writeTestBackup(t, inst, "2026.1001.100000", day, "")
	forB := writeTestBackup(t, inst, "2026.1008.100000", day.Add(7*24*time.Hour), backup.ScopeWithoutUploads)
	writeTestBackup(t, inst, "2026.1015.100000", day.Add(14*24*time.Hour), backup.ScopeWithoutUploads)
	inst.State.PreviousRelease, inst.State.PreviousBackup = "2026.1008.100000", stale
	pruneBackups(app, inst)
	got := remaining(t, inst)
	if !slices.Contains(got, forB) {
		t.Errorf("the backup for the previous release was deleted, left %v", got)
	}
	list, err := backup.List(inst.BackupDir())
	if err != nil {
		t.Fatal(err)
	}
	if b := backupFor(inst, list, inst.State.PreviousRelease); b == nil || b.Path != forB {
		t.Errorf("rollback would restore %v, want %s", b, forB)
	}
}

func TestPruneKeepsPreviousBackup(t *testing.T) {
	app, inst, _ := backupApp(t)
	inst.State.KeepBackups = 1
	inst.State.PreviousRelease = "2026.1001.100000"
	day := time.Date(2026, 10, 1, 4, 0, 0, 0, time.UTC)
	recorded := writeTestBackup(t, inst, "2026.1001.100000", day, backup.ScopeWithoutUploads)
	later := writeTestBackup(t, inst, "2026.1001.100000", day.Add(time.Hour), backup.ScopeWithoutUploads)
	writeTestBackup(t, inst, "2026.1008.100000", day.Add(48*time.Hour), backup.ScopeWithoutUploads)
	inst.State.PreviousBackup = recorded
	pruneBackups(app, inst)
	got := remaining(t, inst)
	if !slices.Contains(got, recorded) {
		t.Errorf("the backup rollback needs was deleted, left %v", got)
	}
	if slices.Contains(got, later) {
		t.Errorf("a newer backup on the same release was kept over the recorded one, left %v", got)
	}
}

func TestBackupListMarksWithoutUploads(t *testing.T) {
	app, inst, out := backupApp(t)
	day := time.Date(2026, 10, 1, 4, 0, 0, 0, time.UTC)
	writeTestBackup(t, inst, "2026.1001.100000", day, "")
	writeTestBackup(t, inst, "2026.1001.100000", day.Add(time.Hour), backup.ScopeWithoutUploads)
	if err := runBackupList(app); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(out.String(), "upgrade, without uploads") || strings.Count(out.String(), "without uploads") != 1 {
		t.Errorf("backup list:\n%s", out.String())
	}
	out.Reset()
	app.Flags.JSON = true
	app.UI = ui.New(ui.Options{Stdin: strings.NewReader(""), Stdout: out, Stderr: out, JSON: true})
	if err := runBackupList(app); err != nil {
		t.Fatal(err)
	}
	var list []map[string]any
	if err := json.Unmarshal(out.Bytes(), &list); err != nil {
		t.Fatalf("%v in %s", err, out.String())
	}
	if len(list) != 2 || list[0]["scope"] != "without-uploads" {
		t.Errorf("JSON list = %s", out.String())
	}
	if _, ok := list[1]["scope"]; ok {
		t.Errorf("a full backup has a scope in %s", out.String())
	}
}

func TestUploadsRefusalSuggestsOnlyThisInstancesBackup(t *testing.T) {
	_, inst, _ := backupApp(t)
	own := writeTestBackup(t, inst, "2026.1020.100000", time.Date(2026, 10, 12, 4, 0, 0, 0, time.UTC), "")
	other := writeManifestBackup(t, inst, backup.Manifest{Format: backup.FormatVersion, Release: "2026.1020.100000", Project: inst.Project(), Domain: "chat-a.example.com", CreatedAt: time.Date(2026, 10, 20, 4, 0, 0, 0, time.UTC), Reason: "upgrade", Scope: backup.ScopeWithoutUploads, Volumes: []string{}})
	err := checkUploadsInPlace(context.Background(), inst.Dir, inst, nil, other)
	var ee *ui.ExitError
	if !errors.As(err, &ee) || ee.Code != ui.ExitRefused || !errors.Is(err, backup.ErrUploadsNotHere) {
		t.Fatalf("restoring another instance's backup without uploads returned %v", err)
	}
	if strings.Contains(ee.Fix, own) || ee.Fix != "Restore a backup made with rexctl backup." {
		t.Errorf("the refusal for chat-a.example.com says %q", ee.Fix)
	}
	if err := uploadsNotHere(inst, inst.Domain(), inst.Project(), backup.ErrUploadsNotHere); !errors.As(err, &ee) || !strings.Contains(ee.Fix, own) {
		t.Errorf("the refusal for this instance's own backup says %v", err)
	}
}
