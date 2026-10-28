// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func testInstance(t *testing.T, yes bool) (*App, *instance.Instance, *bytes.Buffer) {
	t.Helper()
	inst, err := instance.Create(t.TempDir(), "fluxer")
	if err != nil {
		t.Fatal(err)
	}
	set := stack.Embedded()
	inst.State.Release = set.Release
	inst.State.Settings[settings.Domain] = "chat.example.com"
	if _, err := secrets.Mint(inst.State.Secrets, false); err != nil {
		t.Fatal(err)
	}
	r, err := stack.Render(set, nil, inst.State.Images.Refs, inst.Project(), inst.Dir)
	if err != nil {
		t.Fatal(err)
	}
	inst.State.Rendered = r.Hashes
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	out := &bytes.Buffer{}
	app := &App{
		Flags:  GlobalFlags{Dir: inst.Dir, Yes: yes},
		UI:     ui.New(ui.Options{Yes: yes, Stdin: strings.NewReader(""), Stdout: out, Stderr: out}),
		engine: &engine.Engine{Kind: engine.Docker},
	}
	return app, inst, out
}

func exitCode(err error) int {
	var ee *ui.ExitError
	if errors.As(err, &ee) {
		return ee.Code
	}
	return -1
}

func TestEjectExplainsAndRefusesWithoutYes(t *testing.T) {
	app, inst, out := testInstance(t, false)
	err := runEject(context.Background(), app)
	if code := exitCode(err); code != ui.ExitUsage {
		t.Fatalf("exit code %d (%v), want %d", code, err, ui.ExitUsage)
	}
	for _, want := range []string{"You probably don't need to eject.", "rexctl config set changes settings", "rexctl config list shows them", "FLUXER_*", "run rexctl takeover, rexctl upgrade and rexctl eject", "rexctl config edit", instance.CaddyDropInDir, changeHowFluxerRunsURL, "stops managing the instance"} {
		if !strings.Contains(out.String(), want) {
			t.Fatalf("output doesn't say %q:\n%s", want, out)
		}
	}
	if strings.Contains(out.String(), "config list and") || strings.Contains(out.String(), "upgrade Fluxer with") {
		t.Fatalf("output says config list changes settings or that you upgrade with compose yourself:\n%s", out)
	}
	found, err := instance.Find(inst.Dir)
	if err != nil || found.State.Ejected != nil {
		t.Fatalf("ejected without asking: %v", err)
	}
	if _, err := os.Stat(inst.Path(stack.ComposeFile)); !errors.Is(err, os.ErrNotExist) {
		t.Fatal("eject wrote files before asking")
	}
}

func TestConfigUnsetRepairsSettingDuringUnfinishedUpgrade(t *testing.T) {
	app, inst, out := testInstance(t, true)
	inst.State.Upgrade = &instance.Upgrade{From: inst.State.Release, To: "2099.1.0", Phase: release.PhaseSelfUpdated}
	inst.State.Settings[settings.EdgeHTTPPort] = "not-a-port"
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	if _, err := app.Instance(); exitCode(err) != ui.ExitRefused {
		t.Fatalf("loaded an invalid setting: %v", err)
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
	if found.State.Upgrade == nil || found.State.Upgrade.Phase != release.PhaseSelfUpdated {
		t.Fatal("the repair changed the upgrade")
	}
	if !strings.Contains(out.String(), "rexctl upgrade uses the new setting when it finishes.") || strings.Contains(out.String(), "the next time it starts") {
		t.Fatalf("output doesn't say once that the upgrade uses it:\n%s", out)
	}
}

func TestConfigEditKeepsFileUntilItWorks(t *testing.T) {
	app, inst, out := testInstance(t, true)
	inst.State.Stopped = true
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	edits := []string{"services: [\n", "services:\n  edge:\n    build: .\n", "services:\n  edge:\n    labels:\n      team: chat\n"}
	var opened int
	prev, prevAsk := openEditor, askEdit
	t.Cleanup(func() { openEditor, askEdit = prev, prevAsk })
	askEdit = func(*ui.UI, string, bool) (bool, error) { return true, nil }
	openEditor = func(_ context.Context, path string) error {
		before, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if opened == 0 && string(before) != instance.CustomTemplate {
			t.Errorf("first edit started from %q", before)
		}
		if _, err := os.Stat(inst.Path(instance.CustomCompose)); !errors.Is(err, os.ErrNotExist) {
			t.Errorf("the file changed before it passed the check")
		}
		opened++
		return os.WriteFile(path, []byte(edits[opened-1]), 0o644)
	}
	if err := runConfigEdit(context.Background(), app); err != nil {
		t.Fatalf("%v\n%s", err, out)
	}
	if opened != len(edits) {
		t.Fatalf("opened the editor %d times, want %d", opened, len(edits))
	}
	got, err := os.ReadFile(inst.Path(instance.CustomCompose))
	if err != nil || string(got) != edits[len(edits)-1] {
		t.Fatalf("saved %q, %v", got, err)
	}
	if left, _ := os.ReadDir(filepath.Join(inst.StateDir(), editDir)); len(left) > 0 {
		t.Fatalf("left the draft behind: %v", left)
	}
}

func TestConfigEditWithoutAnswerKeepsFile(t *testing.T) {
	app, inst, _ := testInstance(t, false)
	mine := []byte("services: {}\n")
	if err := os.WriteFile(inst.Path(instance.CustomCompose), mine, 0o644); err != nil {
		t.Fatal(err)
	}
	prev := openEditor
	t.Cleanup(func() { openEditor = prev })
	openEditor = func(_ context.Context, path string) error {
		return os.WriteFile(path, []byte("services: [\n"), 0o644)
	}
	if err := runConfigEdit(context.Background(), app); err == nil {
		t.Fatal("applied a broken file")
	}
	if got, _ := os.ReadFile(inst.Path(instance.CustomCompose)); !bytes.Equal(got, mine) {
		t.Fatalf("file changed to %q", got)
	}
}
