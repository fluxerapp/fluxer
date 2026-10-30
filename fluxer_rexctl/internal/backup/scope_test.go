// SPDX-License-Identifier: AGPL-3.0-or-later

package backup

import (
	"archive/tar"
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"maps"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/compose-spec/compose-go/v2/types"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const (
	uploadsCreated = "2026-10-01T04:00:00Z"
	testDefinition = "sha256:uploads"
)

func projectVolumes() []engine.Volume {
	var out []engine.Volume
	for _, short := range []string{"postgres-data", "valkey-data", "nats-data", "meilisearch-data", UploadsVolume} {
		out = append(out, engine.Volume{Name: "fluxer_" + short, Project: "fluxer", Labels: map[string]string{"com.docker.compose.volume": short}, CreatedAt: uploadsCreated})
	}
	return out
}

func shorts(vols []volumeRef) []string {
	var out []string
	for _, v := range vols {
		out = append(out, v.short)
	}
	return out
}

func TestWithoutUploadsLeavesSeaweedOut(t *testing.T) {
	m := &Manifest{}
	got := shorts(pickVolumes(nil, "fluxer", projectVolumes(), false, ScopeWithoutUploads, testDefinition, true, m))
	if slices.Contains(got, UploadsVolume) || !slices.Equal(got, []string{"valkey-data", "nats-data", "meilisearch-data"}) {
		t.Errorf("a backup without uploads copies %v", got)
	}
	if m.Scope != ScopeWithoutUploads || m.Uploads == nil || m.Uploads.Volume != "fluxer_seaweedfs-data" || m.Uploads.CreatedAt != uploadsCreated {
		t.Errorf("manifest = %+v, uploads %+v", m, m.Uploads)
	}
	if m.HasSkipped(SkippedUploads) {
		t.Error("leaving uploads out on purpose was recorded as uploads stored outside Fluxer")
	}
	if m.Uploads.Definition != testDefinition {
		t.Errorf("the backup records the uploads service as %q", m.Uploads.Definition)
	}
	unknown := &Manifest{}
	if got := shorts(pickVolumes(nil, "fluxer", projectVolumes(), false, ScopeWithoutUploads, "", true, unknown)); !slices.Contains(got, UploadsVolume) || unknown.WithoutUploads() {
		t.Errorf("without a known uploads service the backup copies %v with scope %q", got, unknown.Scope)
	}
	full := &Manifest{}
	if got := shorts(pickVolumes(nil, "fluxer", projectVolumes(), false, ScopeFull, testDefinition, true, full)); !slices.Contains(got, UploadsVolume) || full.Scope != "" || full.Uploads != nil {
		t.Errorf("a full backup copies %v with scope %q", got, full.Scope)
	}
	none := &Manifest{}
	pickVolumes(nil, "fluxer", projectVolumes()[:4], false, ScopeWithoutUploads, testDefinition, true, none)
	if none.Scope != "" {
		t.Error("an instance without an uploads volume made a backup that can't restore on its own")
	}
}

func writeScoped(t *testing.T, m *Manifest) string {
	t.Helper()
	final := filepath.Join(t.TempDir(), "fluxer-test"+Extension)
	w, err := createArchive(final + PartialSuffix)
	if err != nil {
		t.Fatal(err)
	}
	if err := w.addBytes(FilesPrefix+"compose.yaml", 0o644, []byte("services: {}")); err != nil {
		t.Fatal(err)
	}
	if err := w.finish(m); err != nil {
		t.Fatal(err)
	}
	if err := os.Rename(final+PartialSuffix, final); err != nil {
		t.Fatal(err)
	}
	return final
}

func TestWithoutUploadsWritesFormatTwo(t *testing.T) {
	path := writeScoped(t, &Manifest{Project: "fluxer", Domain: "chat.example.com", CreatedAt: time.Now().UTC(), Scope: ScopeWithoutUploads, Uploads: &UploadsRef{Volume: "fluxer_seaweedfs-data", CreatedAt: uploadsCreated}})
	m, err := Verify(path)
	if err != nil {
		t.Fatal(err)
	}
	if m.Format != 2 || !m.WithoutUploads() || m.Uploads.Volume != "fluxer_seaweedfs-data" {
		t.Errorf("manifest = %+v", m)
	}
	info, err := Stat(path)
	if err != nil || !info.WithoutUploads() {
		t.Errorf("Stat = %+v, %v", info, err)
	}
	if _, err := parseManifest([]byte(`{"format":3,"release":"x"}`)); !errors.Is(err, ErrNewer) {
		t.Errorf("format 3 = %v, want ErrNewer", err)
	}
}

func TestFullBackupStaysFormatOne(t *testing.T) {
	path := writeScoped(t, &Manifest{Project: "fluxer", Domain: "chat.example.com", CreatedAt: time.Now().UTC()})
	m, err := ReadManifest(path)
	if err != nil {
		t.Fatal(err)
	}
	if m.Format != 1 || m.Scope != "" || m.Uploads != nil {
		t.Errorf("manifest = %+v", m)
	}
}

func TestEstimateAndCreatePickTheSameVolumes(t *testing.T) {
	lean := pickVolumes(nil, "fluxer", projectVolumes(), false, ScopeWithoutUploads, testDefinition, true, &Manifest{})
	full := pickVolumes(nil, "fluxer", projectVolumes(), false, ScopeFull, "", true, &Manifest{})
	if len(full)-len(lean) != 1 || slices.Contains(shorts(lean), UploadsVolume) {
		t.Errorf("the estimate measures %v without uploads and %v with them", shorts(lean), shorts(full))
	}
}

func TestExternalUploadsStillMarkedSkipped(t *testing.T) {
	for _, scope := range []string{ScopeFull, ScopeWithoutUploads} {
		m := &Manifest{}
		got := shorts(pickVolumes(nil, "fluxer", projectVolumes(), true, scope, testDefinition, true, m))
		if slices.Contains(got, UploadsVolume) || !m.HasSkipped(SkippedUploads) || m.Scope != "" || m.Uploads != nil {
			t.Errorf("%s with external uploads copies %v with manifest %+v", scope, got, m)
		}
	}
}

func leanInstance(t *testing.T) *instance.Instance {
	t.Helper()
	inst, err := instance.Create(t.TempDir(), "fluxer")
	if err != nil {
		t.Fatal(err)
	}
	inst.State.Settings["FLUXER_DOMAIN"] = "chat.example.com"
	inst.State.Engine = instance.EngineRef{Kind: "docker", ID: "engine-1"}
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	return inst
}

func leanManifest(inst *instance.Instance) *Manifest {
	return &Manifest{Project: inst.Project(), Domain: inst.Domain(), Scope: ScopeWithoutUploads, Uploads: &UploadsRef{Volume: "fluxer_seaweedfs-data", CreatedAt: uploadsCreated, Definition: uploadsDefinition(inst)}}
}

var dockerEngine = &engine.Engine{Kind: engine.Docker, ID: "engine-1"}

func recordDown(t *testing.T) *[]string {
	t.Helper()
	var calls []string
	prev := downProject
	t.Cleanup(func() { downProject = prev })
	downProject = func(_ context.Context, p *engine.Project, keep []string) error {
		calls = append(calls, p.Name+" keeping "+strings.Join(keep, ","))
		return nil
	}
	return &calls
}

func TestRestoreWithoutUploadsKeepsUploadsVolume(t *testing.T) {
	inst := leanInstance(t)
	m := leanManifest(inst)
	if err := uploadsInPlace(inst.Dir, inst, dockerEngine, projectVolumes(), m); err != nil {
		t.Fatalf("the instance the backup was made for refused it with %v", err)
	}
	calls := recordDown(t)
	if _, err := clearForRestore(context.Background(), inst.Dir, inst, dockerEngine, m); err != nil {
		t.Fatal(err)
	}
	if want := []string{"fluxer keeping " + UploadsVolume}; !slices.Equal(*calls, want) {
		t.Errorf("a restore without uploads ran %q, want %q", *calls, want)
	}
	*calls = nil
	full := leanInstance(t)
	if _, err := clearForRestore(context.Background(), full.Dir, full, dockerEngine, &Manifest{Project: "fluxer"}); err != nil {
		t.Fatal(err)
	}
	if want := []string{"fluxer keeping "}; !slices.Equal(*calls, want) {
		t.Errorf("a full restore ran %q, want %q", *calls, want)
	}
	if _, err := os.Stat(restoreMarkerPath(full.Dir)); !errors.Is(err, os.ErrNotExist) {
		t.Errorf("a full restore left a marker (%v)", err)
	}
}

func TestRestoreWithoutUploadsLeavesUploadsDataAlone(t *testing.T) {
	inst := leanInstance(t)
	m := leanManifest(inst)
	m.Volumes = []string{"valkey-data"}
	final := filepath.Join(t.TempDir(), "fluxer-test"+Extension)
	w, err := createArchive(final + PartialSuffix)
	if err != nil {
		t.Fatal(err)
	}
	if err := w.addBytes(FilesPrefix+"compose.yaml", 0o644, []byte("services: {}")); err != nil {
		t.Fatal(err)
	}
	if err := w.addBytes(VolumesPrefix+UploadsVolume+"/1.dat", 0o644, []byte("volume")); err != nil {
		t.Fatal(err)
	}
	if err := w.finish(m); err != nil {
		t.Fatal(err)
	}
	err = restoreData(context.Background(), final+PartialSuffix, &engine.Project{Name: "fluxer"}, "helper", m, projectVolumes(), "", func(string) {})
	if !errors.Is(err, ErrDamaged) {
		t.Errorf("restoring uploads data from a backup without uploads gave %v, want ErrDamaged", err)
	}
}

func TestRestoreWithoutUploadsRefusesNewerUploadsService(t *testing.T) {
	inst := leanInstance(t)
	images, err := stack.EmbeddedImages()
	if err != nil {
		t.Fatal(err)
	}
	inst.State.Images.Refs = maps.Clone(images.Images)
	onB := leanManifest(inst)
	if onB.Uploads.Definition == "" {
		t.Fatal("the uploads service of release B has no definition")
	}
	if err := uploadsInPlace(inst.Dir, inst, dockerEngine, projectVolumes(), onB); err != nil {
		t.Fatalf("the same seaweedfs was refused with %v", err)
	}
	inst.State.Images.Refs["seaweedfs"] = "chrislusf/seaweedfs:99.1"
	refused(t, uploadsInPlace(inst.Dir, inst, dockerEngine, projectVolumes(), onB), "a backup from before a new seaweedfs")
	onB.Uploads.Definition = ""
	refused(t, uploadsInPlace(inst.Dir, inst, dockerEngine, projectVolumes(), onB), "a backup that doesn't record the uploads service")
}

func TestRestoreWithoutUploadsRefusesSeaweedImageSetting(t *testing.T) {
	inst := leanInstance(t)
	images, err := stack.EmbeddedImages()
	if err != nil {
		t.Fatal(err)
	}
	inst.State.Images.Refs = maps.Clone(images.Images)
	m := leanManifest(inst)
	inst.State.Settings["images.seaweedfs"] = "chrislusf/seaweedfs:99.1"
	refused(t, uploadsInPlace(inst.Dir, inst, dockerEngine, projectVolumes(), m), "a backup from before images.seaweedfs was set")
	delete(inst.State.Settings, "images.seaweedfs")
	inst.State.Settings["FLUXER_SEAWEEDFS_IMAGE"] = "chrislusf/seaweedfs:99.1"
	refused(t, uploadsInPlace(inst.Dir, inst, dockerEngine, projectVolumes(), m), "a backup from before FLUXER_SEAWEEDFS_IMAGE was set")
	delete(inst.State.Settings, "FLUXER_SEAWEEDFS_IMAGE")
	if err := uploadsInPlace(inst.Dir, inst, dockerEngine, projectVolumes(), m); err != nil {
		t.Errorf("the same seaweedfs image was refused with %v", err)
	}
}

func TestLeanRestoreCanRunAgainAfterFailing(t *testing.T) {
	inst := leanInstance(t)
	uploads := filepath.Join(inst.Dir, "uploads")
	if err := os.Mkdir(uploads, 0o755); err != nil {
		t.Fatal(err)
	}
	m := &Manifest{Project: inst.Project(), Domain: inst.Domain(), Scope: ScopeWithoutUploads, Uploads: bindRef(t, uploads, uploadsDefinition(inst))}
	path := writeScoped(t, m)
	recordDown(t)
	if _, err := clearForRestore(context.Background(), inst.Dir, inst, dockerEngine, m); err != nil {
		t.Fatal(err)
	}
	if _, err := instance.Find(inst.Dir); !errors.Is(err, instance.ErrNotFound) {
		t.Fatalf("after clearing, Find = %v", err)
	}
	if err := CheckUploadsInPlace(context.Background(), inst.Dir, nil, dockerEngine, m); err != nil {
		t.Errorf("running the same restore again was refused with %v", err)
	}
	if !ResumesRestore(inst.Dir, m) {
		t.Error("the restore that was cut short isn't recognised")
	}
	if err := uploadsInPlace(inst.Dir, nil, &engine.Engine{Kind: engine.Podman, ID: "engine-1"}, nil, m); err == nil {
		t.Error("running it again on another engine was allowed")
	}
	other := *m
	otherRef := *m.Uploads
	otherRef.Definition = "sha256:other"
	other.Uploads = &otherRef
	refused(t, uploadsInPlace(inst.Dir, nil, dockerEngine, nil, &other), "another backup without uploads")
	if err := os.RemoveAll(uploads); err != nil {
		t.Fatal(err)
	}
	refused(t, uploadsInPlace(inst.Dir, nil, dockerEngine, nil, m), "the same restore with the uploads gone")
	if err := os.Remove(restoreMarkerPath(inst.Dir)); err != nil {
		t.Fatal(err)
	}
	_, err := Restore(context.Background(), path, inst.Dir, dockerEngine, RestoreOptions{})
	refused(t, err, "a restore without the marker or the state")
}

func TestRestoreWithoutUploadsKeepsBindMount(t *testing.T) {
	inst := leanInstance(t)
	uploads := filepath.Join(inst.Dir, "uploads")
	if err := os.MkdirAll(uploads, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(uploads, "a.bin"), []byte("attachment"), 0o600); err != nil {
		t.Fatal(err)
	}
	model := &types.Project{Services: types.Services{"seaweedfs": {Name: "seaweedfs", Volumes: []types.ServiceVolumeConfig{{Type: types.VolumeTypeBind, Source: uploads, Target: "/data"}}}}}
	m := &Manifest{Project: inst.Project(), Domain: inst.Domain()}
	got := shorts(pickVolumes(model, "fluxer", projectVolumes(), false, ScopeWithoutUploads, uploadsDefinition(inst), true, m))
	if slices.Contains(got, UploadsVolume) || m.Uploads == nil || m.Uploads.Bind != uploads {
		t.Fatalf("volumes %v, uploads %+v", got, m.Uploads)
	}
	if m.Uploads.ID == "" {
		t.Fatal("the backup doesn't record which uploads folder it belongs with")
	}
	if err := uploadsInPlace("", inst, dockerEngine, nil, m); err != nil {
		t.Fatalf("the bind mount the backup was made with was refused with %v", err)
	}
	again := &Manifest{Project: inst.Project(), Domain: inst.Domain()}
	pickVolumes(model, "fluxer", projectVolumes(), false, ScopeWithoutUploads, uploadsDefinition(inst), true, again)
	if again.Uploads == nil || again.Uploads.ID != m.Uploads.ID {
		t.Errorf("a second backup records %+v, want the same identity as %+v", again.Uploads, m.Uploads)
	}
	if err := clearInstanceFiles(inst.Dir); err != nil {
		t.Fatal(err)
	}
	if data, err := os.ReadFile(filepath.Join(uploads, "a.bin")); err != nil || string(data) != "attachment" {
		t.Errorf("clearing the instance touched the uploads, which hold %q (%v)", data, err)
	}
	if err := os.RemoveAll(uploads); err != nil {
		t.Fatal(err)
	}
	if err := os.Mkdir(uploads, 0o755); err != nil {
		t.Fatal(err)
	}
	refused(t, uploadsInPlace("", inst, dockerEngine, nil, m), "an uploads folder removed and made again")
	recreated := *m.Uploads
	recreated.Inode, _ = fileInode(uploads)
	refused(t, uploadsIdentity(&recreated, nil), "a folder made again with the same inode")
	if err := os.WriteFile(filepath.Join(uploads, UploadsIDFile), []byte("someone-else\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	refused(t, uploadsIdentity(&recreated, nil), "a folder with another identity")
}

func TestBindMountIdentityIgnoresDeviceNumber(t *testing.T) {
	inst := leanInstance(t)
	uploads := filepath.Join(inst.Dir, "uploads")
	if err := os.MkdirAll(uploads, 0o755); err != nil {
		t.Fatal(err)
	}
	model := &types.Project{Services: types.Services{"seaweedfs": {Name: "seaweedfs", Volumes: []types.ServiceVolumeConfig{{Type: types.VolumeTypeBind, Source: uploads, Target: "/data"}}}}}
	m := &Manifest{Project: inst.Project(), Domain: inst.Domain()}
	pickVolumes(model, "fluxer", projectVolumes(), false, ScopeWithoutUploads, uploadsDefinition(inst), true, m)
	if m.Uploads == nil || m.Uploads.ID == "" {
		t.Fatalf("uploads %+v", m.Uploads)
	}
	data, err := json.Marshal(m.Uploads)
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(string(data), `"dev"`) {
		t.Errorf("the backup records the device number of the uploads folder in %s", data)
	}
	var afterReboot UploadsRef
	moved := strings.Replace(string(data), "{", `{"dev":4242,`, 1)
	if err := json.Unmarshal([]byte(moved), &afterReboot); err != nil {
		t.Fatal(err)
	}
	if err := uploadsIdentity(&afterReboot, nil); err != nil {
		t.Errorf("the same folder with another device number was refused with %v", err)
	}
	otherInode := *m.Uploads
	otherInode.Inode++
	refused(t, uploadsIdentity(&otherInode, nil), "the same identity file in another folder")
}

func TestEstimateDoesNotMarkBindMount(t *testing.T) {
	uploads := t.TempDir()
	model := &types.Project{Services: types.Services{"seaweedfs": {Name: "seaweedfs", Volumes: []types.ServiceVolumeConfig{{Type: types.VolumeTypeBind, Source: uploads, Target: "/data"}}}}}
	got := shorts(pickVolumes(model, "fluxer", projectVolumes(), false, ScopeWithoutUploads, testDefinition, false, &Manifest{}))
	if slices.Contains(got, UploadsVolume) {
		t.Errorf("the estimate for a bind mount copies %v", got)
	}
	if _, err := os.Stat(filepath.Join(uploads, UploadsIDFile)); !errors.Is(err, os.ErrNotExist) {
		t.Errorf("the estimate wrote %s (%v)", UploadsIDFile, err)
	}
}

func TestBindMountWithoutIdentityGetsFullBackup(t *testing.T) {
	uploads := t.TempDir()
	if err := os.Mkdir(filepath.Join(uploads, UploadsIDFile), 0o755); err != nil {
		t.Fatal(err)
	}
	model := &types.Project{Services: types.Services{"seaweedfs": {Name: "seaweedfs", Volumes: []types.ServiceVolumeConfig{{Type: types.VolumeTypeBind, Source: uploads, Target: "/data"}}}}}
	m := &Manifest{}
	got := shorts(pickVolumes(model, "fluxer", projectVolumes(), false, ScopeWithoutUploads, testDefinition, true, m))
	if !slices.Contains(got, UploadsVolume) || m.WithoutUploads() {
		t.Errorf("a bind mount rexctl can't mark copies %v with scope %q", got, m.Scope)
	}
}

func refused(t *testing.T, err error, what string) {
	t.Helper()
	var ue *UploadsNotHereError
	if !errors.Is(err, ErrUploadsNotHere) || !errors.As(err, &ue) {
		t.Errorf("%s gave %v, want a refusal", what, err)
	}
}

func TestRestoreWithoutUploadsRefusesNewMachine(t *testing.T) {
	inst := leanInstance(t)
	m := leanManifest(inst)
	refused(t, uploadsInPlace("", nil, dockerEngine, nil, m), "no instance")
	path := writeScoped(t, m)
	fresh := filepath.Join(t.TempDir(), "fluxer")
	_, err := Restore(context.Background(), path, fresh, dockerEngine, RestoreOptions{})
	refused(t, err, "Restore on a new machine")
	if _, err := os.Stat(fresh); !errors.Is(err, os.ErrNotExist) {
		t.Errorf("the refused restore made %s", fresh)
	}
	if err := CheckUploadsInPlace(context.Background(), "", nil, nil, &Manifest{}); err != nil {
		t.Errorf("a full backup was refused with %v", err)
	}
}

func TestRestoreWithoutUploadsRefusesOtherInstance(t *testing.T) {
	inst := leanInstance(t)
	m := leanManifest(inst)
	m.Domain = "other.example.com"
	refused(t, uploadsInPlace("", inst, dockerEngine, projectVolumes(), m), "another domain")
	m = leanManifest(inst)
	m.Project = "blue"
	refused(t, uploadsInPlace("", inst, dockerEngine, projectVolumes(), m), "another project")
}

func TestRestoreWithoutUploadsRefusesReplacedVolume(t *testing.T) {
	inst := leanInstance(t)
	m := leanManifest(inst)
	vols := projectVolumes()
	vols[4].CreatedAt = "2026-11-02T09:00:00Z"
	refused(t, uploadsInPlace("", inst, dockerEngine, vols, m), "a volume made again")
	refused(t, uploadsInPlace("", inst, dockerEngine, projectVolumes()[:4], m), "a missing volume")

	root := t.TempDir()
	uploads := filepath.Join(root, "uploads")
	if err := os.Mkdir(uploads, 0o755); err != nil {
		t.Fatal(err)
	}
	b := &Manifest{Project: inst.Project(), Domain: inst.Domain(), Scope: ScopeWithoutUploads, Uploads: bindRef(t, uploads, uploadsDefinition(inst))}
	if err := uploadsInPlace("", inst, dockerEngine, nil, b); err != nil {
		t.Fatalf("the same folder was refused with %v", err)
	}
	if err := os.Rename(uploads, filepath.Join(root, "old")); err != nil {
		t.Fatal(err)
	}
	refused(t, uploadsInPlace("", inst, dockerEngine, nil, b), "a missing folder")
	if err := os.Mkdir(uploads, 0o755); err != nil {
		t.Fatal(err)
	}
	refused(t, uploadsInPlace("", inst, dockerEngine, nil, b), "a folder made again")
}

func bindRef(t *testing.T, dir, definition string) *UploadsRef {
	t.Helper()
	ref := uploadsRef(volumeRef{short: UploadsVolume, bind: dir}, nil, true)
	if ref == nil || ref.ID == "" {
		t.Fatalf("no identity for %s: %+v", dir, ref)
	}
	ref.Definition = definition
	return ref
}

func TestRestoreWithoutUploadsRefusesEngineMove(t *testing.T) {
	inst := leanInstance(t)
	m := leanManifest(inst)
	refused(t, uploadsInPlace("", inst, &engine.Engine{Kind: engine.Podman, ID: "engine-1"}, projectVolumes(), m), "another kind of engine")
	refused(t, uploadsInPlace("", inst, &engine.Engine{Kind: engine.Docker, ID: "engine-2"}, projectVolumes(), m), "another engine")
	refused(t, uploadsInPlace("", inst, nil, projectVolumes(), m), "no engine")
}

func TestRestoreForgetsBackupOfAMissingRelease(t *testing.T) {
	inst := leanInstance(t)
	kept := filepath.Join(inst.BackupDir(), "fluxer-a"+Extension)
	if err := os.WriteFile(kept, []byte("x"), 0o600); err != nil {
		t.Fatal(err)
	}
	inst.State.PreviousRelease, inst.State.PreviousBackup = "2026.1001.100000", kept
	if err := adoptRestored(inst, dockerEngine); err != nil {
		t.Fatal(err)
	}
	if inst.State.PreviousRelease != "" || inst.State.PreviousBackup != "" {
		t.Errorf("after the restore the previous release is %q and the previous backup %q", inst.State.PreviousRelease, inst.State.PreviousBackup)
	}
}

func TestLeftoverRestoreMarkerKeepsUploadsServiceCheck(t *testing.T) {
	inst := leanInstance(t)
	images, err := stack.EmbeddedImages()
	if err != nil {
		t.Fatal(err)
	}
	inst.State.Images.Refs = maps.Clone(images.Images)
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	uploads := filepath.Join(inst.Dir, "uploads")
	if err := os.Mkdir(uploads, 0o755); err != nil {
		t.Fatal(err)
	}
	m := &Manifest{Project: inst.Project(), Domain: inst.Domain(), Scope: ScopeWithoutUploads, Uploads: bindRef(t, uploads, uploadsDefinition(inst))}
	if err := writeRestoreMarker(inst.Dir, m, dockerEngine); err != nil {
		t.Fatal(err)
	}
	if err := uploadsInPlace(inst.Dir, inst, dockerEngine, nil, m); err != nil {
		t.Fatalf("the same backup after a failed restore was refused with %v", err)
	}
	inst.State.Images.Refs["seaweedfs"] = "chrislusf/seaweedfs:99.1"
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	refused(t, uploadsInPlace(inst.Dir, inst, dockerEngine, nil, m), "a leftover marker with a newer seaweedfs")
	refused(t, CheckUploadsInPlace(context.Background(), inst.Dir, inst, dockerEngine, m), "the check with a leftover marker and a newer seaweedfs")
	if _, err := os.Stat(restoreMarkerPath(inst.Dir)); !errors.Is(err, os.ErrNotExist) {
		t.Errorf("the check left the marker in place (%v)", err)
	}
	if err := writeRestoreMarker(inst.Dir, m, dockerEngine); err != nil {
		t.Fatal(err)
	}
	path := writeScoped(t, m)
	_, err = Restore(context.Background(), path, inst.Dir, dockerEngine, RestoreOptions{})
	refused(t, err, "Restore with a leftover marker and a newer seaweedfs")
	if _, err := os.Stat(restoreMarkerPath(inst.Dir)); !errors.Is(err, os.ErrNotExist) {
		t.Errorf("the leftover marker is still there (%v)", err)
	}
}

func TestFullRestoreGivesBindUploadsNewIdentity(t *testing.T) {
	uploads := t.TempDir()
	if err := os.WriteFile(filepath.Join(uploads, "new.bin"), []byte("after the full backup"), 0o600); err != nil {
		t.Fatal(err)
	}
	lean := bindRef(t, uploads, testDefinition)
	idFile := filepath.Join(uploads, UploadsIDFile)
	ents, err := os.ReadDir(uploads)
	if err != nil {
		t.Fatal(err)
	}
	for _, e := range ents {
		if err := os.RemoveAll(filepath.Join(uploads, e.Name())); err != nil {
			t.Fatal(err)
		}
	}
	if err := os.WriteFile(idFile, []byte(lean.ID+"\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(uploads, "old.bin"), []byte("from the full backup"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := forgetUploadsID(uploads); err != nil {
		t.Fatal(err)
	}
	refused(t, uploadsIdentity(lean, nil), "a newer backup without uploads after a full restore")
	again := bindRef(t, uploads, testDefinition)
	if again.ID == lean.ID {
		t.Error("the next backup without uploads kept the identity from before the full restore")
	}
	if err := forgetUploadsID(t.TempDir()); err != nil {
		t.Errorf("a folder without an identity gave %v", err)
	}
}

func TestVolumeCopyLeavesUploadsIdentityOut(t *testing.T) {
	var src bytes.Buffer
	tw := tar.NewWriter(&src)
	for _, e := range []struct{ name, body string }{{"./" + UploadsIDFile, "abc\n"}, {"./a.bin", "attachment"}} {
		if err := tw.WriteHeader(&tar.Header{Name: e.name, Mode: 0o644, Size: int64(len(e.body)), Typeflag: tar.TypeReg}); err != nil {
			t.Fatal(err)
		}
		if _, err := tw.Write([]byte(e.body)); err != nil {
			t.Fatal(err)
		}
	}
	if err := tw.Close(); err != nil {
		t.Fatal(err)
	}
	for _, short := range []string{UploadsVolume, "valkey-data"} {
		path := filepath.Join(t.TempDir(), "spool")
		s, err := createSpool(path)
		if err != nil {
			t.Fatal(err)
		}
		copied, err := relayVolume(s, short, tar.NewReader(bytes.NewReader(src.Bytes())))
		if err != nil {
			t.Fatal(err)
		}
		if err := s.finish(); err != nil {
			t.Fatal(err)
		}
		if copied != int64(len("abc\n")+len("attachment")) {
			t.Errorf("%s counted %d bytes", short, copied)
		}
		raw, err := os.ReadFile(path)
		if err != nil {
			t.Fatal(err)
		}
		var names []string
		tr := tar.NewReader(bytes.NewReader(raw))
		for {
			hdr, err := tr.Next()
			if err != nil {
				break
			}
			names = append(names, hdr.Name)
		}
		stored := slices.Contains(names, VolumesPrefix+short+"/"+UploadsIDFile)
		if stored != (short != UploadsVolume) {
			t.Errorf("%s stored %v", short, names)
		}
	}
}
