// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
)

func TestPlanFixStagesNothing(t *testing.T) {
	env := &Env{
		Instance:   &instance.Instance{Dir: t.TempDir(), State: &instance.State{Settings: map[string]string{settings.VoicePublicIP: "198.51.100.4"}}},
		publicIP:   "203.0.113.7",
		publicDone: true,
	}
	p, err := PlanFix(context.Background(), env, "livekit-ip")
	if err != nil {
		t.Fatal(err)
	}
	if len(p.Steps) != 1 {
		t.Fatalf("got %d steps, want 1", len(p.Steps))
	}
	if got, want := p.Steps[0].Command, "rexctl config set voice.public_ip 203.0.113.7"; got != want {
		t.Fatalf("command %q, want %q", got, want)
	}
	if got, want := p.Steps[0].Undo, "rexctl config set voice.public_ip 198.51.100.4"; got != want {
		t.Fatalf("undo %q, want %q", got, want)
	}
	if !p.Asks {
		t.Fatal("the plan says rexctl doesn't ask first")
	}
	if env.Staged() {
		t.Fatal("planning the fix left a staged change behind")
	}
}

func TestPlanFixRunsNothing(t *testing.T) {
	const id = "plan-test"
	Register(Check{ID: id, Fix: func(ctx context.Context, e *Env) error {
		return e.run(ctx, host.Step{Do: "Changes this machine.", Run: func(context.Context) error {
			t.Error("a dry run ran a step")
			return nil
		}})
	}})
	t.Cleanup(func() { delete(registry, id) })
	p, err := PlanFix(context.Background(), &Env{}, id)
	if err != nil {
		t.Fatal(err)
	}
	if len(p.Steps) != 1 || p.Steps[0].Do != "Changes this machine." {
		t.Fatalf("got steps %+v", p.Steps)
	}
}
