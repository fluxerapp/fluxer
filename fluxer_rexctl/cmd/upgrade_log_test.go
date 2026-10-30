// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"context"
	"errors"
	"os"
	"strings"
	"testing"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func pendingUpgrade(t *testing.T, phase string) (*App, *instance.Instance) {
	t.Helper()
	inst, err := instance.Create(t.TempDir(), "fluxer")
	if err != nil {
		t.Fatal(err)
	}
	inst.State.Release = recoveryFrom
	inst.State.Upgrade = &instance.Upgrade{From: recoveryFrom, To: recoveryLatest, Phase: phase, StartedAt: time.Now().UTC().Add(-time.Minute)}
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	out := &bytes.Buffer{}
	app := &App{
		Version: recoveryLatest,
		Flags:   GlobalFlags{Dir: inst.Dir},
		UI:      ui.New(ui.Options{Stdin: strings.NewReader(""), Stdout: out, Stderr: out}),
		engine:  &engine.Engine{Kind: engine.Docker},
	}
	return app, inst
}

func upgradeLogText(t *testing.T, inst *instance.Instance) string {
	t.Helper()
	data, err := os.ReadFile(inst.Path(instance.StateDirName, upgradeLog(recoveryLatest)))
	if err != nil {
		t.Fatalf("no upgrade log: %v", err)
	}
	return string(data)
}

func TestAbortedUpgradeWritesItsLogWhenGoingBackFails(t *testing.T) {
	app, inst := pendingUpgrade(t, release.PhaseMigrated)
	cause := errors.New("couldn't write docker-compose.yml")
	err := abortUpgrade(context.Background(), app, inst, app.engine, cause, ui.Wrap(cause, "Couldn't write the files.", ""))
	if err == nil {
		t.Fatal("going back worked without an engine")
	}
	if got := upgradeLogText(t, inst); !strings.Contains(got, cause.Error()) {
		t.Fatalf("upgrade log = %q, want the cause", got)
	}
}

func TestRecoveringAnInterruptedUpgradeWritesItsLog(t *testing.T) {
	app, inst := pendingUpgrade(t, release.PhaseRecreated)
	_ = recoverUpgrade(context.Background(), app, inst, true)
	if got := upgradeLogText(t, inst); !strings.Contains(got, "didn't finish") {
		t.Fatalf("upgrade log = %q", got)
	}
}

func TestRecoveringKeepsTheLogOfTheFailedAttempt(t *testing.T) {
	app, inst := pendingUpgrade(t, release.PhaseRecreated)
	path := inst.Path(instance.StateDirName, upgradeLog(recoveryLatest))
	if err := os.WriteFile(path, []byte("rexctl: api isn't ready\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	_ = recoverUpgrade(context.Background(), app, inst, true)
	if got := upgradeLogText(t, inst); !strings.HasPrefix(got, "rexctl: api isn't ready\n") {
		t.Fatalf("upgrade log = %q", got)
	}
}

func TestAPanicDuringAnUpgradeWritesItsLog(t *testing.T) {
	_, inst := pendingUpgrade(t, release.PhaseRecreated)
	err := func() (err error) {
		defer guardUpgrade(inst, recoveryLatest, &err)
		panic("interface conversion: interface {} is nil")
	}()
	var ee *ui.ExitError
	if !errors.As(err, &ee) || !strings.Contains(ee.Fix, upgradeLog(recoveryLatest)) {
		t.Fatalf("err = %#v, want a fix that points at the log", err)
	}
	if got := upgradeLogText(t, inst); !strings.Contains(got, "interface conversion") || !strings.Contains(got, "goroutine") {
		t.Fatalf("upgrade log = %q", got)
	}
}
