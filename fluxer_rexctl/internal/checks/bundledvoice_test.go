// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

func TestInstallWithoutBundledVoiceWantsNoVoicePorts(t *testing.T) {
	e := &Env{prepared: true, Settings: map[string]string{stack.VoiceBundledKey: "false"}}
	for _, p := range e.wantedPorts() {
		if p.Number == 7881 || p.Number == 7882 {
			t.Fatalf("wanted ports %v include a voice port", e.wantedPorts())
		}
	}
	e = &Env{prepared: true, Settings: map[string]string{}}
	voice := 0
	for _, p := range e.wantedPorts() {
		if p.Number == 7881 || p.Number == 7882 {
			voice++
		}
	}
	if voice != 2 {
		t.Fatalf("wanted ports %v, want both voice ports", e.wantedPorts())
	}
}

func TestLiveKitIPSkipsWithoutTheBundledLiveKit(t *testing.T) {
	e := &Env{prepared: true, psDone: true, Instance: &instance.Instance{State: &instance.State{}},
		ps: []engine.Container{{Service: "api", State: "running"}, {Service: "edge", State: "running"}}}
	if r := runLiveKitIP(context.Background(), e); r.Status != Skip {
		t.Fatalf("result = %+v, want a skip", r)
	}
}
