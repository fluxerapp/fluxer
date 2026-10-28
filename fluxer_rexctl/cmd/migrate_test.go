// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func oldInstance(t *testing.T, yes bool) (*App, *instance.Instance, *bytes.Buffer) {
	t.Helper()
	app, inst, out := testInstance(t, yes)
	for name := range inst.State.Rendered {
		plain := stack.Plain(name, readFile(t, inst.Generated(name)))
		if err := os.WriteFile(inst.Path(name), plain, 0o644); err != nil {
			t.Fatal(err)
		}
		inst.State.Rendered[name] = stack.Hash(plain)
	}
	if err := os.RemoveAll(stack.GeneratedPath(inst.Dir)); err != nil {
		t.Fatal(err)
	}
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	data := bytes.Replace(readFile(t, inst.StatePath()), []byte(`"schema": 3`), []byte(`"schema": 2`), 1)
	if err := os.WriteFile(inst.StatePath(), data, 0o600); err != nil {
		t.Fatal(err)
	}
	old, err := instance.Find(inst.Dir)
	if err != nil || !old.OldLayout() {
		t.Fatalf("the fixture isn't on the old layout: %v", err)
	}
	stubUnits(t, func() error { return nil })
	return app, old, out
}

func stubUnits(t *testing.T, fn func() error) {
	t.Helper()
	prev := refreshUnitBinaries
	t.Cleanup(func() { refreshUnitBinaries = prev })
	refreshUnitBinaries = func(context.Context, *instance.Instance) error { return fn() }
}

func readFile(t *testing.T, path string) []byte {
	t.Helper()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	return data
}

func schemaOnDisk(t *testing.T, inst *instance.Instance) int {
	t.Helper()
	var head struct{ Schema int }
	if err := json.Unmarshal(readFile(t, inst.StatePath()), &head); err != nil {
		t.Fatal(err)
	}
	return head.Schema
}

func requireOldLayout(t *testing.T, inst *instance.Instance) {
	t.Helper()
	if s := schemaOnDisk(t, inst); s != 2 {
		t.Fatalf("schema on disk %d, want 2", s)
	}
	if _, err := os.Stat(inst.Path(stack.ComposeFile)); err != nil {
		t.Fatalf("the compose file left the instance folder: %v", err)
	}
	if _, err := os.Stat(stack.GeneratedPath(inst.Dir)); !errors.Is(err, os.ErrNotExist) {
		t.Fatal("a read made the generated folder")
	}
}

func serveRelease(t *testing.T, rel release.Release) {
	t.Helper()
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		_ = json.NewEncoder(w).Encode(rel)
	}))
	t.Cleanup(srv.Close)
	t.Setenv(release.ResolverURLEnv, srv.URL)
}

func TestReadsLeaveTheOldLayout(t *testing.T) {
	app, inst, out := oldInstance(t, true)
	serveRelease(t, release.Release{Version: "2099.101.0", StateSchema: instance.StateSchema, Assets: []release.Asset{{OS: runtime.GOOS, Arch: runtime.GOARCH}}})
	ctx := context.Background()
	for name, run := range map[string]func() error{
		"instance":          func() error { _, err := app.Instance(); return err },
		"config get":        func() error { _, err := loadConfigView(app, nil); return err },
		"upgrade --check":   func() error { return runUpgrade(ctx, app, &upgradeFlags{Check: true}) },
		"upgrade --dry-run": func() error { return runUpgrade(ctx, app, &upgradeFlags{DryRun: true}) },
		"declined upgrade": func() error {
			app.UI = ui.New(ui.Options{Stdin: strings.NewReader("n\n"), Stdout: out, Stderr: out})
			return runUpgrade(ctx, app, &upgradeFlags{})
		},
	} {
		if err := run(); err != nil && exitCode(err) == -1 {
			t.Fatalf("%s: %v\n%s", name, err, out)
		}
		requireOldLayout(t, inst)
	}
}

func TestOldLayoutLoadsWhileAnotherCommandHoldsIt(t *testing.T) {
	app, inst, _ := oldInstance(t, true)
	l, err := inst.Lock(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	defer l.Release()
	if _, err := app.Instance(); err != nil {
		t.Fatalf("status refused while another command holds the instance: %v", err)
	}
	found, err := bootInstance(app)
	if err != nil || found == nil {
		t.Fatalf("boot start gave up before waiting for the lock: %v", err)
	}
	requireOldLayout(t, inst)
}

func TestChangeMovesAfterRefreshingUnitCopies(t *testing.T) {
	app, inst, _ := oldInstance(t, true)
	var schemaThen int
	stubUnits(t, func() error {
		schemaThen = schemaOnDisk(t, inst)
		return nil
	})
	if err := app.Locked(context.Background(), inst, func() error { return nil }); err != nil {
		t.Fatal(err)
	}
	if schemaThen != 2 {
		t.Fatalf("refreshed the unit copies with schema %d on disk, want 2", schemaThen)
	}
	if s := schemaOnDisk(t, inst); s != instance.StateSchema {
		t.Fatalf("schema on disk %d after a change, want %d", s, instance.StateSchema)
	}
	if _, err := os.Stat(inst.Generated(stack.ComposeFile)); err != nil || inst.OldLayout() {
		t.Fatalf("the compose file didn't move: %v", err)
	}
}

func TestChangeKeepsOldLayoutWhenUnitCopiesCantBeRefreshed(t *testing.T) {
	app, inst, _ := oldInstance(t, true)
	stubUnits(t, func() error { return errors.New("sudo: a password is required") })
	ran := false
	if err := app.Locked(context.Background(), inst, func() error { ran = true; return nil }); err != nil {
		t.Fatal(err)
	}
	if !ran {
		t.Fatal("the command didn't run")
	}
	requireOldLayout(t, inst)
}

func TestRootGuardRunsBeforeMoving(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("no root on Windows")
	}
	app, inst, _ := oldInstance(t, true)
	if uid, ok := inst.OwnerUID(); !ok || uid == 0 {
		t.Skip("the test folder belongs to root")
	}
	inst.State.Engine = instance.EngineRef{Kind: string(engine.Podman)}
	prev := euid
	t.Cleanup(func() { euid = prev })
	euid = func() int { return 0 }
	stubUnits(t, func() error { t.Error("refreshed unit copies as root"); return nil })
	if err := app.Locked(context.Background(), inst, func() error { return nil }); exitCode(err) != ui.ExitRefused {
		t.Fatalf("got %v, want the rootless refusal", err)
	}
	requireOldLayout(t, inst)
}

func TestUpgradeToAnOlderSchemaReleaseKeepsTheOldLayout(t *testing.T) {
	app, inst, out := oldInstance(t, true)
	app.Version = "2099.100.0"
	serveRelease(t, release.Release{Version: "2099.101.0", StateSchema: 2, Assets: []release.Asset{{OS: runtime.GOOS, Arch: runtime.GOARCH, URL: "https://example.invalid/rexctl.tar.gz", SHA256: strings.Repeat("0", 64)}}})
	prevUpdate, prevReexec := selfUpdate, reexec
	t.Cleanup(func() { selfUpdate, reexec = prevUpdate, prevReexec })
	updated := false
	selfUpdate = func(context.Context, release.Asset) error {
		updated = true
		requireOldLayout(t, inst)
		return nil
	}
	reexec = func([]string) error { return nil }
	if err := runUpgrade(context.Background(), app, &upgradeFlags{}); err != nil {
		t.Fatalf("%v\n%s", err, out)
	}
	if !updated {
		t.Fatalf("rexctl didn't switch to the release:\n%s", out)
	}
	requireOldLayout(t, inst)
	if _, err := os.Stat(filepath.Join(inst.Dir, stack.Caddyfile)); err != nil {
		t.Fatal(err)
	}
}

func TestStatusAndDoctorSayAnEditedRootCaddyfileIsUnused(t *testing.T) {
	app, inst, out := oldInstance(t, true)
	ctx := context.Background()
	if err := app.Locked(ctx, inst, func() error { return nil }); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(inst.Path(stack.Caddyfile), []byte(":443 {\n}\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	compose := inst.Generated(stack.ComposeFile)
	if err := os.WriteFile(compose, stack.Managed(stack.ComposeFile, stack.Plain(stack.ComposeFile, readFile(t, compose))), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := inst.DropRootCopies(ctx, true); err != nil {
		t.Fatal(err)
	}
	warnUnusedRootFiles(ctx, app, inst)
	lines := strings.Split(strings.TrimSpace(out.String()), "\n")
	last := lines[len(lines)-1]
	if len(lines) == 0 || !strings.Contains(last, "no longer uses "+inst.Path(stack.Caddyfile)) || !strings.Contains(last, instance.CaddyDropInDir+"/") {
		t.Fatalf("output:\n%s", out)
	}
}
