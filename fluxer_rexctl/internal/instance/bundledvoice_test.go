// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"context"
	"slices"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

func loadWithVoice(t *testing.T, bundled string) *engine.Project {
	t.Helper()
	inst, err := Create(t.TempDir(), "fluxer")
	if err != nil {
		t.Fatal(err)
	}
	set := stack.Embedded()
	inst.State.Release = set.Release
	inst.State.Images = Images{Release: set.Release, Refs: fluxerRefs("a")}
	inst.State.Settings[settings.Domain] = "chat.example.com"
	if bundled != "" {
		inst.State.Settings[settings.VoiceBundled] = bundled
	}
	if _, err := secrets.Mint(inst.State.Secrets, false); err != nil {
		t.Fatal(err)
	}
	if _, err := inst.Render(set, stack.OverlaysFor(inst.State.Settings), inst.State.Images.Refs); err != nil {
		t.Fatal(err)
	}
	env, err := inst.Environment()
	if err != nil {
		t.Fatal(err)
	}
	var none engine.Engine
	p, err := none.Load(context.Background(), engine.Spec{Name: "fluxer", Dir: inst.Dir, Files: inst.ComposeFiles(), Env: env})
	if err != nil {
		t.Fatalf("voice.bundled %q: %v", bundled, err)
	}
	return p
}

func TestWithoutBundledVoiceLiveKitDoesNotRun(t *testing.T) {
	for _, tc := range []struct {
		bundled string
		runs    bool
		env     string
	}{
		{"", true, "true"},
		{"true", true, "true"},
		{"false", false, "false"},
	} {
		p := loadWithVoice(t, tc.bundled)
		if got := slices.Contains(p.Services(), "livekit"); got != tc.runs {
			t.Errorf("voice.bundled %q: livekit runs = %v, want %v", tc.bundled, got, tc.runs)
		}
		if !slices.Contains(p.Services(), "api") || !slices.Contains(p.Services(), "edge") {
			t.Errorf("voice.bundled %q: services %v", tc.bundled, p.Services())
		}
		got := p.Model.Services["api"].Environment["FLUXER_LIVEKIT_BUNDLED"]
		if got == nil || *got != tc.env {
			t.Errorf("voice.bundled %q: api FLUXER_LIVEKIT_BUNDLED = %v, want %s", tc.bundled, got, tc.env)
		}
	}
}
