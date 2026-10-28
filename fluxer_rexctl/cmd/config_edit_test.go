// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func editTest(t *testing.T, yes, stopped bool, edits ...func(path string) error) (*App, *instance.Instance, *bytes.Buffer, *int) {
	t.Helper()
	app, inst, out := testInstance(t, yes)
	if stopped {
		inst.State.Stopped = true
		if err := inst.Save(); err != nil {
			t.Fatal(err)
		}
	}
	opened := 0
	prev, prevAsk := openEditor, askEdit
	t.Cleanup(func() { openEditor, askEdit = prev, prevAsk })
	openEditor = func(_ context.Context, path string) error {
		opened++
		if opened > len(edits) {
			t.Errorf("opened the editor %d times", opened)
			return errors.New("too many edits")
		}
		return edits[opened-1](path)
	}
	return app, inst, out, &opened
}

func write(data string) func(string) error {
	return func(path string) error { return os.WriteFile(path, []byte(data), 0o644) }
}

func keep(string) error { return nil }

func editedCopies(t *testing.T, inst *instance.Instance) [][]byte {
	t.Helper()
	entries, _ := os.ReadDir(inst.Path(instance.StateDirName, instance.EditedDir))
	var out [][]byte
	for _, e := range entries {
		if strings.HasPrefix(e.Name(), instance.CustomCompose) {
			data, err := os.ReadFile(inst.Path(instance.StateDirName, instance.EditedDir, e.Name()))
			if err != nil {
				t.Fatal(err)
			}
			out = append(out, data)
		}
	}
	return out
}

func noCustom(t *testing.T, inst *instance.Instance) {
	t.Helper()
	if _, err := os.Stat(inst.Path(instance.CustomCompose)); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("%s was written: %v", instance.CustomCompose, err)
	}
}

func TestConfigEditUnderYesStopsAtBrokenDraft(t *testing.T) {
	app, inst, out, opened := editTest(t, true, true, write("services: [\n"))
	err := runConfigEdit(context.Background(), app)
	if exitCode(err) != ui.ExitRefused || *opened != 1 {
		t.Fatalf("err %v after %d edits", err, *opened)
	}
	noCustom(t, inst)
	if copies := editedCopies(t, inst); len(copies) != 1 || string(copies[0]) != "services: [\n" {
		t.Fatalf("kept %q", copies)
	}
	if !strings.Contains(out.String(), "Your draft is in") {
		t.Fatalf("output doesn't say where the draft is:\n%s", out)
	}
}

func TestConfigEditStopsWhenDraftUnchanged(t *testing.T) {
	app, inst, _, opened := editTest(t, true, true, write("services: [\n"), keep)
	askEdit = func(*ui.UI, string, bool) (bool, error) { return true, nil }
	err := runConfigEdit(context.Background(), app)
	if *opened != 2 || err == nil || !strings.Contains(err.Error(), "Kept "+instance.CustomCompose+" as it was.") {
		t.Fatalf("err %v after %d edits", err, *opened)
	}
	noCustom(t, inst)
}

func TestConfigEditTakesLockBeforeEditor(t *testing.T) {
	app, inst, _, opened := editTest(t, true, true, write("services: {}\n"))
	l, err := inst.Lock(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	defer l.Release()
	err = runConfigEdit(context.Background(), app)
	if *opened != 0 || exitCode(err) != ui.ExitRefused || !strings.Contains(err.Error(), "Another rexctl command") {
		t.Fatalf("err %v after %d edits", err, *opened)
	}
}

func TestConfigEditKeepsDraftWhenFileChangedMeanwhile(t *testing.T) {
	var inst *instance.Instance
	var draftPath string
	byHand := []byte("services: {}\n# by hand\n")
	app, inst, out, _ := editTest(t, true, true, func(path string) error {
		draftPath = path
		if err := os.WriteFile(inst.Path(instance.CustomCompose), byHand, 0o644); err != nil {
			return err
		}
		return os.WriteFile(path, []byte("services:\n  edge:\n    labels:\n      team: chat\n"), 0o644)
	})
	swap := filepath.Join(inst.StateDir(), editDir, ".custom.compose.yml.swp")
	if err := os.MkdirAll(filepath.Dir(swap), 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(swap, []byte("swap"), 0o600); err != nil {
		t.Fatal(err)
	}
	err := runConfigEdit(context.Background(), app)
	if exitCode(err) != ui.ExitRefused || !strings.Contains(err.Error(), "changed while you were editing") {
		t.Fatalf("err %v\n%s", err, out)
	}
	if got, _ := os.ReadFile(inst.Path(instance.CustomCompose)); !bytes.Equal(got, byHand) {
		t.Fatalf("overwrote the hand edit with %q", got)
	}
	if _, err := os.Stat(swap); err != nil {
		t.Fatalf("deleted a file rexctl didn't create: %v", err)
	}
	if draftPath == filepath.Join(inst.StateDir(), editDir, instance.CustomCompose) {
		t.Fatal("every run edits the same draft")
	}
	if copies := editedCopies(t, inst); len(copies) != 1 || !bytes.Contains(copies[0], []byte("team: chat")) {
		t.Fatalf("kept %q", copies)
	}
}

func TestConfigEditKeepsDraftWhenEditorFails(t *testing.T) {
	saved := "services:\n  edge:\n    labels:\n      team: chat\n"
	app, inst, out, _ := editTest(t, true, true, func(path string) error {
		if err := os.WriteFile(path, []byte(saved), 0o644); err != nil {
			return err
		}
		return errors.New("the editor was killed")
	})
	if err := runConfigEdit(context.Background(), app); err == nil {
		t.Fatal("a failed editor counted as a change")
	}
	noCustom(t, inst)
	if copies := editedCopies(t, inst); len(copies) != 1 || string(copies[0]) != saved {
		t.Fatalf("lost what you saved before the editor failed, kept %q\n%s", copies, out)
	}
}

func TestConfigEditAsksBeforeDroppingFluxerVolumes(t *testing.T) {
	drop := "services:\n  postgres:\n    volumes: !reset []\n"
	app, inst, out, _ := editTest(t, true, true, write(drop))
	err := runConfigEdit(context.Background(), app)
	if exitCode(err) != ui.ExitRefused || !strings.Contains(out.String(), "loses the volume") {
		t.Fatalf("err %v\n%s", err, out)
	}
	noCustom(t, inst)

	app, inst, out, _ = editTest(t, true, true, write(drop))
	askEdit = func(*ui.UI, string, bool) (bool, error) { return true, nil }
	if err := runConfigEdit(context.Background(), app); err != nil {
		t.Fatalf("%v\n%s", err, out)
	}
	if got, _ := os.ReadFile(inst.Path(instance.CustomCompose)); string(got) != drop {
		t.Fatalf("saved %q", got)
	}
}

func TestConfigEditWarnsAboutImagesAndManagedVariables(t *testing.T) {
	change := "services:\n  api:\n    image: example/api:1\n    environment:\n      FLUXER_DOMAIN: other.example.com\n"
	app, inst, out, _ := editTest(t, true, true, write(change))
	if err := runConfigEdit(context.Background(), app); err != nil {
		t.Fatalf("%v\n%s", err, out)
	}
	for _, want := range []string{"api runs example/api:1", "FLUXER_DOMAIN", "rexctl config set"} {
		if !strings.Contains(out.String(), want) {
			t.Fatalf("output doesn't say %q:\n%s", want, out)
		}
	}
	if got, _ := os.ReadFile(inst.Path(instance.CustomCompose)); string(got) != change {
		t.Fatalf("saved %q", got)
	}
}

func TestConfigEditReportsDownloadFailureAccurately(t *testing.T) {
	app, inst, out, _ := editTest(t, true, false, write("services:\n  edge:\n    labels:\n      team: chat\n"))
	err := runConfigEdit(context.Background(), app)
	if err == nil || !strings.Contains(err.Error(), "Couldn't download") || strings.Contains(out.String(), "didn't start") {
		t.Fatalf("err %v\n%s", err, out)
	}
	noCustom(t, inst)
}

func TestStartFailureSaysWhatHappenedToTheFile(t *testing.T) {
	if strings.Contains(startFailure(false), "put the old") || !strings.Contains(startFailure(true), "put the old") {
		t.Fatalf("%q / %q", startFailure(false), startFailure(true))
	}
}

func TestPlanLinesSayStartsForNewServices(t *testing.T) {
	got := planLines(&engine.Plan{Create: []string{"worker"}, Recreate: []string{"api"}})
	if want := []string{"This restarts api.", "This starts worker."}; !slices.Equal(got, want) {
		t.Fatalf("%q, want %q", got, want)
	}
}

func TestYAMLProblemNamesTheLine(t *testing.T) {
	_, inst, _ := testInstance(t, true)
	got := checkCustom(context.Background(), inst, "", []byte("services: [\n"))
	if !strings.HasPrefix(got, "Line 2 isn't valid YAML.\n") {
		t.Fatalf("%q", got)
	}
}

func TestQuickEditorGetsHint(t *testing.T) {
	app, _, out, _ := editTest(t, true, true, keep)
	if err := runConfigEdit(context.Background(), app); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(out.String(), "If your editor opens a window, use code --wait.") {
		t.Fatalf("no hint:\n%s", out)
	}
}

func TestEditorRunsThroughTheShell(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("uses sh")
	}
	dir := filepath.Join(t.TempDir(), "my editor")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	script := filepath.Join(dir, "edit.sh")
	if err := os.WriteFile(script, []byte("#!/bin/sh\nprintf '%s' \"$1\" > \"$2\"\n"), 0o755); err != nil {
		t.Fatal(err)
	}
	target := filepath.Join(t.TempDir(), "file")
	if err := editorCmd(context.Background(), "'"+script+"' edited", target).Run(); err != nil {
		t.Fatal(err)
	}
	if got, _ := os.ReadFile(target); string(got) != "edited" {
		t.Fatalf("editor wrote %q", got)
	}
}

func TestSplitCommandKeepsQuotedPaths(t *testing.T) {
	got := splitCommand(`"C:\Program Files\Notepad++\notepad++.exe" -multiInst`)
	if want := []string{`C:\Program Files\Notepad++\notepad++.exe`, "-multiInst"}; !slices.Equal(got, want) {
		t.Fatalf("%q", got)
	}
}

func TestNoEditorFound(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("notepad is always there")
	}
	t.Setenv("VISUAL", "")
	t.Setenv("EDITOR", "")
	t.Setenv("PATH", t.TempDir())
	if editor, ok := editorCommand(); ok {
		t.Fatalf("found %q", editor)
	}
}

func TestNotificationSaysWhichFilesWerePutBack(t *testing.T) {
	var body []byte
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		body, _ = io.ReadAll(r.Body)
	}))
	defer srv.Close()
	app, inst, _ := testInstance(t, true)
	inst.State.NotifyURL = srv.URL
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	path := inst.Generated(stack.ComposeFile)
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, append(data, "# mine\n"...), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := app.drift(inst, true, false); err != nil {
		t.Fatal(err)
	}
	app.outcome = "Upgraded to Fluxer 1."
	notifyUpgrade(context.Background(), app, nil)
	var n upgradeNotice
	if err := json.Unmarshal(body, &n); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(n.Text, "rexctl put back its own "+stack.ComposeFile+". Your copy is in "+inst.Path(instance.StateDirName, instance.EditedDir)) {
		t.Fatalf("notice %q", n.Text)
	}
	if len(instance.UnseenEdits(inst)) != 1 {
		t.Fatal("an unattended put back isn't left for status and doctor to show")
	}
}

func TestConfigRepairsSettingAfterUpgradePulled(t *testing.T) {
	app, inst, out := testInstance(t, true)
	inst.State.Upgrade = &instance.Upgrade{From: inst.State.Release, To: "2099.1.0", Phase: release.PhasePulled}
	inst.State.Settings[settings.EdgeHTTPPort] = "not-a-port"
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	if err := runConfigChange(context.Background(), app, []configChange{{name: settings.EdgeHTTPPort}}); err != nil {
		t.Fatalf("%v\n%s", err, out)
	}
	found, err := instance.Find(inst.Dir)
	if err != nil {
		t.Fatal(err)
	}
	if _, ok := found.State.Settings[settings.EdgeHTTPPort]; ok {
		t.Fatal("the invalid setting is still there")
	}
	v := "8080"
	if err := runConfigChange(context.Background(), app, []configChange{{name: settings.EdgeHTTPPort, value: &v}}); exitCode(err) != ui.ExitRefused {
		t.Fatalf("changed a valid setting halfway through an upgrade: %v", err)
	}
}

func TestConfigSetDuringUpgradeChecksPorts(t *testing.T) {
	app, inst, out := testInstance(t, true)
	inst.State.Upgrade = &instance.Upgrade{From: inst.State.Release, To: "2099.1.0", Phase: release.PhaseSelfUpdated}
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	v := "7881"
	err := runConfigChange(context.Background(), app, []configChange{{name: settings.EdgeHTTPPort, value: &v}})
	if exitCode(err) != ui.ExitRefused || !strings.Contains(err.Error(), "already uses") {
		t.Fatalf("err %v\n%s", err, out)
	}
}

func TestStatusPointsToCopiesUntilDoctorShowsThem(t *testing.T) {
	app, inst, out := testInstance(t, true)
	dir := inst.Path(instance.StateDirName, instance.EditedDir)
	if err := os.MkdirAll(dir, 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, stack.ComposeFile+".20260101-000000"), []byte("mine"), 0o600); err != nil {
		t.Fatal(err)
	}
	_ = runStatus(context.Background(), app)
	if !strings.Contains(out.String(), "Your copies are in "+dir) {
		t.Fatalf("status doesn't point to the copies:\n%s", out)
	}
	instance.SeenEdits(inst)
	out.Reset()
	_ = runStatus(context.Background(), app)
	if strings.Contains(out.String(), "Your copies are in") {
		t.Fatalf("status still points to copies you saw:\n%s", out)
	}
}
