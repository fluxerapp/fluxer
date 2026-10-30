// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"maps"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"strings"
	"testing"
	"testing/fstest"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	recoveryFrom   = "2026.1020.100000"
	recoveryRexctl = "2026.1025.120000"
	recoveryLatest = "2026.1025.233929"
)

type recoveryRun struct {
	app    *App
	dir    string
	out    *bytes.Buffer
	events []string
}

func newRecoveryRun(t *testing.T, latest string) *recoveryRun {
	t.Helper()
	inst, err := instance.Create(t.TempDir(), "fluxer")
	if err != nil {
		t.Fatal(err)
	}
	inst.State.Release = recoveryFrom
	inst.State.Upgrade = &instance.Upgrade{From: recoveryFrom, To: recoveryRexctl, Phase: release.PhaseResolved}
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		if latest == "" {
			w.WriteHeader(http.StatusServiceUnavailable)
			return
		}
		_ = json.NewEncoder(w).Encode(release.Release{Version: latest, Assets: []release.Asset{{OS: runtime.GOOS, Arch: runtime.GOARCH, URL: "https://example.invalid/rexctl.tar.gz", SHA256: strings.Repeat("0", 64)}}})
	}))
	t.Cleanup(srv.Close)
	t.Setenv(release.ResolverURLEnv, srv.URL)
	r := &recoveryRun{dir: inst.Dir, out: &bytes.Buffer{}}
	r.app = &App{
		Version: recoveryRexctl,
		Flags:   GlobalFlags{Dir: inst.Dir},
		UI:      ui.New(ui.Options{Stdin: strings.NewReader(""), Stdout: r.out, Stderr: r.out}),
		engine:  &engine.Engine{Kind: engine.Docker},
	}
	prevUpdate, prevReexec := selfUpdate, reexec
	t.Cleanup(func() { selfUpdate, reexec = prevUpdate, prevReexec })
	selfUpdate = func(context.Context, release.Asset) error {
		r.events = append(r.events, fmt.Sprintf("self-update pending=%v", r.pending(t)))
		return nil
	}
	reexec = func(args []string) error {
		r.events = append(r.events, "reexec "+strings.Join(args[len(args)-2:], " "))
		return nil
	}
	return r
}

func (r *recoveryRun) pending(t *testing.T) bool {
	t.Helper()
	inst, err := instance.Find(r.dir)
	if err != nil {
		t.Fatal(err)
	}
	return inst.State.Upgrade != nil
}

func TestPendingUpgradeUpdatesRexctlBeforeRecovery(t *testing.T) {
	for name, run := range map[string]func(*recoveryRun) error{
		"upgrade":  func(r *recoveryRun) error { return runUpgrade(context.Background(), r.app, &upgradeFlags{}) },
		"rollback": func(r *recoveryRun) error { return runRollback(context.Background(), r.app, &rollbackFlags{}) },
	} {
		t.Run(name, func(t *testing.T) {
			r := newRecoveryRun(t, recoveryLatest)
			if err := run(r); err != nil {
				t.Fatal(err)
			}
			want := []string{"self-update pending=true", "reexec --resume " + release.ResumeRecovery}
			if !slices.Equal(r.events, want) {
				t.Fatalf("events = %q, want %q", r.events, want)
			}
			if !r.pending(t) {
				t.Fatal("the old rexctl recovered the upgrade before handing over")
			}
		})
	}
}

func TestReexecutedRecoveryDoesNotUpdateRexctlAgain(t *testing.T) {
	for name, run := range map[string]func(*recoveryRun) error{
		"upgrade": func(r *recoveryRun) error {
			return runUpgrade(context.Background(), r.app, &upgradeFlags{Resume: release.ResumeRecovery})
		},
		"rollback": func(r *recoveryRun) error {
			return runRollback(context.Background(), r.app, &rollbackFlags{Resume: release.ResumeRecovery})
		},
	} {
		t.Run(name, func(t *testing.T) {
			r := newRecoveryRun(t, recoveryLatest)
			_ = run(r)
			if len(r.events) > 0 {
				t.Fatalf("events = %q, want none", r.events)
			}
			if r.pending(t) {
				t.Fatal("the upgrade wasn't recovered")
			}
		})
	}
}

func TestPendingUpgradeRecoversWithCurrentRexctlWhenResolverFails(t *testing.T) {
	r := newRecoveryRun(t, "")
	_ = runUpgrade(context.Background(), r.app, &upgradeFlags{})
	if len(r.events) > 0 {
		t.Fatalf("events = %q, want none", r.events)
	}
	if r.pending(t) {
		t.Fatal("the upgrade wasn't recovered")
	}
	if note := "Couldn't check for a newer rexctl. Continuing with rexctl " + recoveryRexctl + "."; !strings.Contains(r.out.String(), note) {
		t.Fatalf("output %q doesn't say %q", r.out.String(), note)
	}
}

func TestPendingUpgradeRecoversWithCurrentRexctlWhenItIsLatest(t *testing.T) {
	r := newRecoveryRun(t, recoveryRexctl)
	if err := runRollback(context.Background(), r.app, &rollbackFlags{}); err != nil {
		t.Fatal(err)
	}
	if len(r.events) > 0 {
		t.Fatalf("events = %q, want none", r.events)
	}
	if r.pending(t) {
		t.Fatal("the upgrade wasn't recovered")
	}
}

func recoveryInstance(t *testing.T, r *recoveryRun) *instance.Instance {
	t.Helper()
	inst, err := instance.Find(r.dir)
	if err != nil {
		t.Fatal(err)
	}
	return inst
}

func TestFinishUpgradeRecordsPreviousBackup(t *testing.T) {
	r := newRecoveryRun(t, "")
	inst := recoveryInstance(t, r)
	made := writeTestBackup(t, inst, recoveryFrom, time.Date(2026, 10, 25, 4, 0, 0, 0, time.UTC), backup.ScopeWithoutUploads)
	inst.State.Upgrade = &instance.Upgrade{From: recoveryFrom, To: recoveryRexctl, Phase: release.PhaseVerified, Backup: made}
	next := *inst.State
	next.Release, next.Upgrade = recoveryRexctl, nil
	if err := release.Snapshot(&instance.Instance{Dir: inst.Dir, State: &next}, recoveryRexctl, nil); err != nil {
		t.Fatal(err)
	}
	if err := finishUpgrade(context.Background(), r.app, inst, r.app.engine); err != nil {
		t.Fatal(err)
	}
	saved := recoveryInstance(t, r)
	if saved.State.PreviousBackup != made || saved.State.PreviousRelease != recoveryFrom || saved.State.Release != recoveryRexctl {
		t.Errorf("after the upgrade the release is %s, the previous one %s and the previous backup %q", saved.State.Release, saved.State.PreviousRelease, saved.State.PreviousBackup)
	}
}

func TestRollbackUsesRecordedBackup(t *testing.T) {
	r := newRecoveryRun(t, "")
	inst := recoveryInstance(t, r)
	inst.State.Upgrade = nil
	day := time.Date(2026, 10, 25, 4, 0, 0, 0, time.UTC)
	recorded := writeTestBackup(t, inst, recoveryFrom, day, backup.ScopeWithoutUploads)
	manual := writeTestBackup(t, inst, recoveryFrom, day.Add(2*time.Hour), "")
	inst.State.PreviousRelease, inst.State.PreviousBackup = recoveryFrom, recorded
	b, err := upgradeBackup(inst, recoveryFrom)
	if err != nil || b.Path != recorded {
		t.Fatalf("upgradeBackup = %v, %v, want the recorded backup %s", b, err, recorded)
	}
	inst.State.PreviousBackup = filepath.Join(inst.BackupDir(), "gone"+backup.Extension)
	if b, _ := upgradeBackup(inst, recoveryFrom); b == nil || b.Path != manual {
		t.Errorf("with the recorded backup gone, upgradeBackup = %v, want the newest backup %s", b, manual)
	}
	inst.State.PreviousBackup = recorded
	if b, _ := upgradeBackup(inst, "2026.1001.100000"); b != nil {
		t.Errorf("upgradeBackup for another release = %v, want none", b)
	}
}

func TestSearchGoBackUsesUpgradeBackup(t *testing.T) {
	r := newRecoveryRun(t, "")
	inst := recoveryInstance(t, r)
	day := time.Date(2026, 10, 25, 4, 0, 0, 0, time.UTC)
	made := writeTestBackup(t, inst, recoveryFrom, day, backup.ScopeWithoutUploads)
	writeTestBackup(t, inst, recoveryFrom, day.Add(time.Hour), "")
	inst.State.Upgrade.Backup = made
	list, err := backup.List(inst.BackupDir())
	if err != nil {
		t.Fatal(err)
	}
	if b := backupFor(inst, list, recoveryFrom); b == nil || b.Path != made {
		t.Errorf("backupFor = %v, want the backup made for this upgrade %s", b, made)
	}
	inst.State.Upgrade.Backup = ""
	if b := backupFor(inst, list, recoveryFrom); b == nil || b.Path == made {
		t.Errorf("without an upgrade backup, backupFor = %v, want the newest one", b)
	}
}

func testSet(t *testing.T, name string, edit func(string) string) *stack.Set {
	t.Helper()
	src := stack.Embedded()
	files := fstest.MapFS{}
	err := fs.WalkDir(src.Files, ".", func(p string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() {
			return err
		}
		data, err := fs.ReadFile(src.Files, p)
		if err != nil {
			return err
		}
		if p == stack.ComposeFile && edit != nil {
			data = []byte(edit(string(data)))
		}
		files[p] = &fstest.MapFile{Data: data, Mode: 0o644}
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}
	return &stack.Set{Release: name, Files: files, Profiles: src.Profiles, Vars: src.Vars}
}

func TestUploadsChangeForcesFullBackup(t *testing.T) {
	r := newRecoveryRun(t, "")
	inst := recoveryInstance(t, r)
	images, err := stack.EmbeddedImages()
	if err != nil {
		t.Fatal(err)
	}
	inst.State.Images = instance.Images{Release: recoveryFrom, Refs: images.Images}
	if err := release.WriteRelease(inst, testSet(t, recoveryFrom, nil)); err != nil {
		t.Fatal(err)
	}
	next := *inst.State
	if uploadsChange(inst, recoveryFrom, testSet(t, recoveryRexctl, nil), &next) {
		t.Error("the same uploads services count as a change")
	}
	api := testSet(t, recoveryRexctl, func(c string) string {
		return strings.Replace(c, "FLUXER_API_HEADERS_TIMEOUT_MS:-}", "FLUXER_API_HEADERS_TIMEOUT_MS:-5}", 1)
	})
	if uploadsChange(inst, recoveryFrom, api, &next) {
		t.Error("a change to another service counts as an uploads change")
	}
	changed := testSet(t, recoveryRexctl, func(c string) string {
		return strings.Replace(c, "-volume.max=${FLUXER_SEAWEEDFS_VOLUME_MAX:-10000}", "-volume.max=${FLUXER_SEAWEEDFS_VOLUME_MAX:-20000}", 1)
	})
	if !uploadsChange(inst, recoveryFrom, changed, &next) {
		t.Error("a new seaweedfs command doesn't count as an uploads change")
	}
	init := testSet(t, recoveryRexctl, func(c string) string {
		return strings.Replace(c, "FLUXER_SEAWEEDFS_UPLOADS_TTL:-1d", "FLUXER_SEAWEEDFS_UPLOADS_TTL:-2d", 1)
	})
	if !uploadsChange(inst, recoveryFrom, init, &next) {
		t.Error("a new seaweedfs-init definition doesn't count as an uploads change")
	}
	bumped := next
	bumped.Images.Refs = map[string]string{}
	for k, v := range next.Images.Refs {
		bumped.Images.Refs[k] = v
	}
	bumped.Images.Refs["seaweedfs"] = "chrislusf/seaweedfs:99.1"
	if !uploadsChange(inst, recoveryFrom, testSet(t, recoveryRexctl, nil), &bumped) {
		t.Error("a new seaweedfs image doesn't count as an uploads change")
	}
	if !uploadsChange(inst, "2026.0101.000000", testSet(t, recoveryRexctl, nil), &next) {
		t.Error("a release without saved files doesn't fall back to a full backup")
	}

	lean := &upgradeFlags{Backup: backup.ScopeWithoutUploads}
	if f := backupFlagsFor(r.app, inst, recoveryFrom, testSet(t, recoveryRexctl, nil), &next, lean, "2026.1025"); f != lean || strings.Contains(r.out.String(), "changes how uploads are stored") {
		t.Errorf("an upgrade that keeps the uploads services got %+v and printed %q", f, r.out.String())
	}
	f := backupFlagsFor(r.app, inst, recoveryFrom, changed, &next, lean, "2026.1025")
	if upgradeScope(f) != backup.ScopeFull || upgradeScope(lean) != backup.ScopeWithoutUploads {
		t.Fatalf("an upgrade that changes uploads backs up with %s", upgradeScope(f))
	}
	if !strings.Contains(r.out.String(), "Fluxer 2026.1025 changes how uploads are stored. This backup includes them.") {
		t.Errorf("the forced full backup printed %q", r.out.String())
	}
	var scopes []string
	prevCreate, prevEstimate := createBackup, estimateBackup
	t.Cleanup(func() { createBackup, estimateBackup = prevCreate, prevEstimate })
	estimateBackup = func(_ context.Context, _ *instance.Instance, _ *engine.Project, scope string) (uint64, error) {
		scopes = append(scopes, "estimate "+scope)
		return 0, nil
	}
	createBackup = func(_ context.Context, _ *instance.Instance, _ *engine.Project, o backup.CreateOptions) (*backup.Info, error) {
		scopes = append(scopes, "create "+o.Scope)
		return nil, backup.ErrNeverStarted
	}
	upgradeSpace(context.Background(), r.app, inst, r.app.engine, next.Images.Refs, f)
	if err := backUpForUpgrade(context.Background(), r.app, inst, r.app.engine.Named(inst.Project(), inst.Dir), upgradeScope(f)); err != nil {
		t.Fatal(err)
	}
	if want := []string{"estimate full", "create full"}; !slices.Equal(scopes, want) {
		t.Errorf("the forced full backup ran %q, want %q", scopes, want)
	}
	scopes = nil
	upgradeSpace(context.Background(), r.app, inst, r.app.engine, next.Images.Refs, lean)
	if want := []string{"estimate without-uploads"}; !slices.Equal(scopes, want) {
		t.Errorf("a lean upgrade estimates %q, want %q", scopes, want)
	}
}

func TestLeanBackupVerifyFailureAbortsUpgrade(t *testing.T) {
	r := newRecoveryRun(t, "")
	inst := recoveryInstance(t, r)
	inst.State.Upgrade.Phase = release.PhasePulled
	bad := filepath.Join(t.TempDir(), "broken"+backup.Extension)
	prev := createBackup
	t.Cleanup(func() { createBackup = prev })
	var got backup.CreateOptions
	createBackup = func(_ context.Context, _ *instance.Instance, _ *engine.Project, o backup.CreateOptions) (*backup.Info, error) {
		got = o
		if err := os.WriteFile(bad, []byte("not a backup"), 0o600); err != nil {
			return nil, err
		}
		return &backup.Info{Path: bad, Release: recoveryFrom, Scope: backup.ScopeWithoutUploads}, nil
	}
	err := backUpForUpgrade(context.Background(), r.app, inst, r.app.engine.Named(inst.Project(), inst.Dir), backup.ScopeWithoutUploads)
	if err == nil {
		t.Fatal("a backup that fails its check let the upgrade go on")
	}
	if got.Scope != backup.ScopeWithoutUploads || got.Reason != "upgrade" {
		t.Errorf("Create got %+v", got)
	}
	if inst.State.Upgrade.Backup != "" {
		t.Errorf("the broken backup was recorded as %s", inst.State.Upgrade.Backup)
	}
	if fileExists(bad) {
		t.Error("the broken backup was left on disk")
	}
	if inst.State.Upgrade.Phase != release.PhasePulled {
		t.Errorf("phase = %s, want %s", inst.State.Upgrade.Phase, release.PhasePulled)
	}
}

func TestReexecKeepsBackupFlag(t *testing.T) {
	args := resumeArgs([]string{"upgrade", "--yes", "--backup", "without-uploads", "--dir", "/opt/fluxer"}, release.PhaseSelfUpdated, "")
	if i := slices.Index(args, "--backup"); i < 0 || i+1 >= len(args) || args[i+1] != "without-uploads" {
		t.Errorf("resume args = %q", args)
	}
	r := newRecoveryRun(t, recoveryLatest)
	prevArgs := os.Args
	t.Cleanup(func() { os.Args = prevArgs })
	os.Args = []string{"rexctl", "upgrade", "--yes", "--backup=without-uploads"}
	var handed []string
	reexec = func(args []string) error {
		handed = args
		return nil
	}
	if err := runUpgrade(context.Background(), r.app, &upgradeFlags{Backup: backup.ScopeWithoutUploads}); err != nil {
		t.Fatal(err)
	}
	if !slices.Contains(handed, "--backup=without-uploads") || !slices.Contains(handed, release.ResumeRecovery) {
		t.Errorf("rexctl started again with %q", handed)
	}
}

func TestBackupFlagConflictsWithNoBackup(t *testing.T) {
	r := newRecoveryRun(t, "")
	for _, f := range []upgradeFlags{
		{Backup: backup.ScopeWithoutUploads, NoBackup: true},
		{Backup: backup.ScopeFull, NoBackup: true},
		{Backup: backup.ScopeWithoutUploads, Check: true},
		{Backup: backup.ScopeWithoutUploads, Prefetch: true},
		{Backup: backup.ScopeFull, Prefetch: true},
		{Backup: "uploads"},
	} {
		err := runUpgrade(context.Background(), r.app, &f)
		var ee *ui.ExitError
		if !errors.As(err, &ee) || ee.Code != ui.ExitUsage {
			t.Errorf("runUpgrade(%+v) = %v, want a usage error", f, err)
		}
	}
}

func TestSwitchingRexctlBackDropsBackupFlag(t *testing.T) {
	r := newRecoveryRun(t, "2026.1022.100000")
	r.app.UI = ui.New(ui.Options{Stdin: strings.NewReader(""), Stdout: r.out, Stderr: r.out, Yes: true})
	inst, err := instance.Find(r.dir)
	if err != nil {
		t.Fatal(err)
	}
	inst.State.Upgrade = nil
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	scheduled := 0
	prevDrop := dropScheduledBackup
	t.Cleanup(func() { dropScheduledBackup = prevDrop })
	dropScheduledBackup = func(context.Context, *instance.Instance, *engine.Engine) (host.AutoUpgradeState, bool, error) {
		scheduled++
		return host.AutoUpgradeState{On: true, At: autoUpgradeDefault, Backup: backup.ScopeWithoutUploads}, true, nil
	}
	prevArgs := os.Args
	t.Cleanup(func() { os.Args = prevArgs })
	for _, args := range [][]string{
		{"rexctl", "upgrade", "--to", "2026.1022.100000", "--backup", "without-uploads", "--yes"},
		{"rexctl", "upgrade", "--to", "2026.1022.100000", "--backup=without-uploads", "--yes"},
	} {
		os.Args = args
		var handed []string
		reexec = func(args []string) error {
			handed = args
			return nil
		}
		r.out.Reset()
		if err := runUpgrade(context.Background(), r.app, &upgradeFlags{To: "2026.1022.100000", Backup: backup.ScopeWithoutUploads}); err != nil {
			t.Fatalf("%q: %v", args, err)
		}
		if handed == nil || slices.ContainsFunc(handed, func(a string) bool { return strings.HasPrefix(a, "--backup") }) || slices.Contains(handed, "without-uploads") {
			t.Errorf("%q started the older rexctl with %q", args, handed)
		}
		if !slices.Contains(handed, "--to") || !slices.Contains(handed, release.PhaseSelfUpdated) {
			t.Errorf("%q lost other flags, started with %q", args, handed)
		}
		for _, want := range []string{"This upgrade makes a full backup.", "Automatic upgrades make full backups from now on as well.", "Switched rexctl back to"} {
			if !strings.Contains(r.out.String(), want) {
				t.Errorf("%q printed %q, want %q", args, r.out.String(), want)
			}
		}
		inst, err := instance.Find(r.dir)
		if err != nil {
			t.Fatal(err)
		}
		inst.State.Upgrade = nil
		if err := inst.Save(); err != nil {
			t.Fatal(err)
		}
	}
	if scheduled != 2 {
		t.Errorf("the schedule was checked %d times, want 2", scheduled)
	}
}

func TestSwitchBackRefusesWhenScheduleCantChange(t *testing.T) {
	r := newRecoveryRun(t, "")
	prevDrop := dropScheduledBackup
	t.Cleanup(func() { dropScheduledBackup = prevDrop })
	dropScheduledBackup = func(context.Context, *instance.Instance, *engine.Engine) (host.AutoUpgradeState, bool, error) {
		return host.AutoUpgradeState{}, false, host.ErrNeedsRoot
	}
	_, err := switchBack(context.Background(), r.app, nil, nil, []string{"upgrade", "--to", "2026.1022.100000"}, "2026.1022")
	var ee *ui.ExitError
	if !errors.As(err, &ee) || ee.Code != ui.ExitRefused || !strings.Contains(ee.Fix, "sudo") {
		t.Errorf("switchBack = %v, want a refusal that asks for root", err)
	}
	dropScheduledBackup = func(context.Context, *instance.Instance, *engine.Engine) (host.AutoUpgradeState, bool, error) {
		return host.AutoUpgradeState{}, false, nil
	}
	r.out.Reset()
	dropScheduledBackup = func(context.Context, *instance.Instance, *engine.Engine) (host.AutoUpgradeState, bool, error) {
		return host.AutoUpgradeState{On: true, At: autoUpgradeDefault, Backup: backup.ScopeWithoutUploads}, true, nil
	}
	if _, err := switchBack(context.Background(), r.app, nil, nil, []string{"upgrade", "--to", "2026.1022.100000"}, "2026.1022"); err != nil || !strings.Contains(r.out.String(), "rexctl 2026.1022 may not know --backup. Automatic upgrades make full backups from now on.") {
		t.Errorf("switchBack without --backup = %v, printed %q", err, r.out.String())
	}
	dropScheduledBackup = func(context.Context, *instance.Instance, *engine.Engine) (host.AutoUpgradeState, bool, error) {
		return host.AutoUpgradeState{}, false, nil
	}
	r.out.Reset()
	args, err := switchBack(context.Background(), r.app, nil, nil, []string{"upgrade", "--backup", "full", "--to", "2026.1022.100000"}, "2026.1022")
	if err != nil || slices.Contains(args, "--backup") || strings.Contains(r.out.String(), "full backup") {
		t.Errorf("switchBack with --backup full = %q, %v, printed %q", args, err, r.out.String())
	}
}

func TestSwitchBackKeepsTheWholeSchedule(t *testing.T) {
	r := newRecoveryRun(t, "")
	inst := recoveryInstance(t, r)
	inst.State.NotifyURL = "https://ntfy.sh/fluxer-upgrades"
	prevDrop := dropScheduledBackup
	t.Cleanup(func() { dropScheduledBackup = prevDrop })
	dropScheduledBackup = func(context.Context, *instance.Instance, *engine.Engine) (host.AutoUpgradeState, bool, error) {
		return host.AutoUpgradeState{On: true, Day: "Sun", At: "03:30", Backup: backup.ScopeWithoutUploads, FullBackup: host.FullBackupMonthly}, true, nil
	}
	if _, err := switchBack(context.Background(), r.app, inst, nil, []string{"upgrade", "--backup", "without-uploads", "--to", "2026.1022.100000"}, "2026.1022"); err != nil {
		t.Fatal(err)
	}
	out := r.out.String()
	for _, want := range []string{
		"run rexctl auto-upgrade on --day sun --at 03:30 --backup without-uploads --full-backup monthly again.",
		"Add --notify with your address to that command to keep the notifications.",
		"The monthly full backup keeps running. rexctl 2026.1022 doesn't know about it, and rexctl auto-upgrade off from rexctl 2026.1022 leaves it on.",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("switchBack printed %q, want %q", out, want)
		}
	}
	r.out.Reset()
	dropScheduledBackup = func(context.Context, *instance.Instance, *engine.Engine) (host.AutoUpgradeState, bool, error) {
		return host.AutoUpgradeState{On: true, At: autoUpgradeDefault, Backup: backup.ScopeFull, FullBackup: host.FullBackupMonthly}, false, nil
	}
	if _, err := switchBack(context.Background(), r.app, inst, nil, []string{"upgrade", "--to", "2026.1022.100000"}, "2026.1022"); err != nil {
		t.Fatal(err)
	}
	if out := r.out.String(); !strings.Contains(out, "The monthly full backup keeps running.") || strings.Contains(out, "--notify") {
		t.Errorf("switchBack with full backups before upgrades printed %q", out)
	}
	r.out.Reset()
	dropScheduledBackup = func(context.Context, *instance.Instance, *engine.Engine) (host.AutoUpgradeState, bool, error) {
		return host.AutoUpgradeState{On: true, At: autoUpgradeDefault, Backup: backup.ScopeWithoutUploads}, true, nil
	}
	inst.State.NotifyURL = ""
	if _, err := switchBack(context.Background(), r.app, inst, nil, []string{"upgrade", "--backup", "without-uploads", "--to", "2026.1022.100000"}, "2026.1022"); err != nil {
		t.Fatal(err)
	}
	if out := r.out.String(); !strings.Contains(out, "run rexctl auto-upgrade on --backup without-uploads again.") || strings.Contains(out, "monthly") || strings.Contains(out, "weekly") {
		t.Errorf("switchBack with a daily schedule printed %q", out)
	}
}

func TestSwitchBackWarnsAboutWeeklySchedule(t *testing.T) {
	r := newRecoveryRun(t, "")
	inst := recoveryInstance(t, r)
	inst.State.NotifyURL = "https://ntfy.sh/fluxer-upgrades"
	prevDrop := dropScheduledBackup
	t.Cleanup(func() { dropScheduledBackup = prevDrop })
	dropScheduledBackup = func(context.Context, *instance.Instance, *engine.Engine) (host.AutoUpgradeState, bool, error) {
		return host.AutoUpgradeState{On: true, Day: "Sun", At: autoUpgradeDefault, Backup: backup.ScopeFull, FullBackup: host.FullBackupMonthly}, false, nil
	}
	if _, err := switchBack(context.Background(), r.app, inst, nil, []string{"upgrade", "--to", "2026.1022.100000"}, "2026.1022"); err != nil {
		t.Fatal(err)
	}
	out := r.out.String()
	for _, want := range []string{
		"rexctl 2026.1022 shows this weekly schedule as every day, and it keeps running every Sunday. Once rexctl is newer than 2026.1022, run rexctl auto-upgrade to see it again.",
		"rexctl auto-upgrade on from rexctl 2026.1022 makes the schedule daily. Once rexctl is newer than 2026.1022, run rexctl auto-upgrade on --day sun --full-backup monthly to make it weekly again.",
		"Add --notify with your address to that command to keep the notifications.",
		"The monthly full backup keeps running.",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("switchBack with a weekly schedule and full backups printed %q, want %q", out, want)
		}
	}
	r.out.Reset()
	dropScheduledBackup = func(context.Context, *instance.Instance, *engine.Engine) (host.AutoUpgradeState, bool, error) {
		return host.AutoUpgradeState{On: true, Day: "Sun", At: "03:30", Backup: backup.ScopeWithoutUploads}, true, nil
	}
	if _, err := switchBack(context.Background(), r.app, inst, nil, []string{"upgrade", "--to", "2026.1022.100000"}, "2026.1022"); err != nil {
		t.Fatal(err)
	}
	out = r.out.String()
	if !strings.Contains(out, "run rexctl auto-upgrade on --day sun --at 03:30 --backup without-uploads again.") || !strings.Contains(out, "it keeps running every Sunday") || strings.Contains(out, "makes the schedule daily") || strings.Count(out, "--notify") != 1 {
		t.Errorf("switchBack with a weekly lean schedule printed %q", out)
	}
}

func TestDryRunShowsForcedFullBackup(t *testing.T) {
	r := newRecoveryRun(t, "")
	inst := recoveryInstance(t, r)
	images, err := stack.EmbeddedImages()
	if err != nil {
		t.Fatal(err)
	}
	inst.State.Images = instance.Images{Release: recoveryFrom, Refs: maps.Clone(images.Images)}
	if _, err := secrets.Mint(inst.State.Secrets, false); err != nil {
		t.Fatal(err)
	}
	if err := release.WriteRelease(inst, testSet(t, recoveryFrom, nil)); err != nil {
		t.Fatal(err)
	}
	var scopes []string
	prevEstimate := estimateBackup
	t.Cleanup(func() { estimateBackup = prevEstimate })
	estimateBackup = func(_ context.Context, _ *instance.Instance, _ *engine.Project, scope string) (uint64, error) {
		scopes = append(scopes, scope)
		return 0, nil
	}
	lean := &upgradeFlags{Backup: backup.ScopeWithoutUploads, DryRun: true}
	version := stack.Embedded().Release
	printUpgradePlan(context.Background(), r.app, inst, lean, false, recoveryFrom, version)
	if out := r.out.String(); !strings.Contains(out, "backed up first, without uploads") || strings.Contains(out, "changes how uploads are stored") || !slices.Equal(scopes, []string{backup.ScopeWithoutUploads}) {
		t.Errorf("a dry run that keeps seaweedfs estimated %q and printed %q", scopes, out)
	}
	r.out.Reset()
	scopes = nil
	inst.State.Images.Refs["seaweedfs"] = "chrislusf/seaweedfs:3.1"
	printUpgradePlan(context.Background(), r.app, inst, lean, false, recoveryFrom, version)
	out := r.out.String()
	if !strings.Contains(out, "changes how uploads are stored. The backup before it includes them.") || strings.Contains(out, "without uploads") || !strings.Contains(out, "Your instance is backed up first.") {
		t.Errorf("a dry run with a new seaweedfs printed %q", out)
	}
	if !slices.Equal(scopes, []string{backup.ScopeFull}) {
		t.Errorf("a dry run with a new seaweedfs estimated %q, want a full backup", scopes)
	}
	r.out.Reset()
	scopes = nil
	printUpgradePlan(context.Background(), r.app, inst, lean, true, recoveryFrom, recoveryLatest)
	if !slices.Equal(scopes, []string{backup.ScopeWithoutUploads, backup.ScopeWithoutUploads, backup.ScopeFull}) {
		t.Errorf("a dry run that updates rexctl first estimated %q, want both scopes", scopes)
	}
	if out := r.out.String(); !strings.Contains(out, "without uploads unless Fluxer") {
		t.Errorf("a dry run that updates rexctl first printed %q", out)
	}
}

func TestRollbackDryRunChecksUploads(t *testing.T) {
	r := newRecoveryRun(t, "")
	inst := recoveryInstance(t, r)
	inst.State.Upgrade = nil
	uploads := t.TempDir()
	if err := os.WriteFile(filepath.Join(uploads, backup.UploadsIDFile), []byte("made-again\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := release.WriteRelease(inst, testSet(t, recoveryFrom, nil)); err != nil {
		t.Fatal(err)
	}
	set, err := release.LoadSet(inst, recoveryFrom)
	if err != nil {
		t.Fatal(err)
	}
	day := time.Date(2026, 10, 25, 4, 0, 0, 0, time.UTC)
	ref := &backup.UploadsRef{Bind: uploads, ID: "made-before", Definition: backup.UploadsDefinition(set, inst.State)}
	if ref.Definition == "" {
		t.Fatal("no uploads definition for the test release")
	}
	made := writeManifestBackup(t, inst, backup.Manifest{Format: backup.FormatVersion, Release: recoveryFrom, Project: inst.Project(), Domain: inst.Domain(), CreatedAt: day, Reason: "upgrade", Scope: backup.ScopeWithoutUploads, Uploads: ref, Volumes: []string{}})
	inst.State.PreviousRelease, inst.State.PreviousBackup = recoveryFrom, made
	err = rollbackWithBackup(context.Background(), r.app, inst, nil, &rollbackFlags{DryRun: true, RestoreDB: true}, recoveryFrom, "2026.1020")
	var ee *ui.ExitError
	var why *backup.UploadsNotHereError
	if !errors.As(err, &ee) || ee.Code != ui.ExitRefused || !errors.As(err, &why) || !strings.Contains(why.Why, "made again") {
		t.Errorf("a dry run with uploads made again returned %v (%v)", err, why)
	}
	if strings.Contains(r.out.String(), "Uploads stay as they are") {
		t.Errorf("the dry run promised a restore without uploads: %q", r.out.String())
	}
	if err := os.WriteFile(filepath.Join(uploads, backup.UploadsIDFile), []byte("made-before\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	ref.Inode = uploadsInode(t, uploads)
	made = writeManifestBackup(t, inst, backup.Manifest{Format: backup.FormatVersion, Release: recoveryFrom, Project: inst.Project(), Domain: inst.Domain(), CreatedAt: day.Add(time.Minute), Reason: "upgrade", Scope: backup.ScopeWithoutUploads, Uploads: ref, Volumes: []string{}})
	inst.State.PreviousBackup = made
	r.out.Reset()
	if err := rollbackWithBackup(context.Background(), r.app, inst, nil, &rollbackFlags{DryRun: true, RestoreDB: true}, recoveryFrom, "2026.1020"); err != nil {
		t.Fatalf("a dry run with the uploads in place returned %v", err)
	}
	out := r.out.String()
	if !strings.Contains(out, "Messages and changes since then are lost. Uploads stay as they are.") || strings.Contains(out, "Anything that changed") {
		t.Errorf("the dry run of a restore without uploads says %q", out)
	}
}
