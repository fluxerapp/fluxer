// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
	"runtime"
	"slices"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
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
