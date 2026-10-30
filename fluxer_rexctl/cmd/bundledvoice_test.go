// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"errors"
	"slices"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func TestInstallWithoutBundledVoiceOpensNoVoicePorts(t *testing.T) {
	in := &installer{f: &installFlags{NoVoice: true}}
	for _, p := range in.firewallPorts() {
		if p.Number == 7881 || p.Number == 7882 {
			t.Fatalf("firewall ports %v include a voice port", in.firewallPorts())
		}
	}
	if got := portList(in.firewallPorts()); got != "80 and 443" {
		t.Fatalf("firewall ports %s", got)
	}
	in = &installer{f: &installFlags{}}
	if got := in.firewallPorts(); !slices.Contains(got, host.Port{Number: 7881, Proto: "tcp"}) || !slices.Contains(got, host.Port{Number: 7882, Proto: "udp"}) {
		t.Fatalf("firewall ports %v leave out the bundled LiveKit", got)
	}
}

func TestInstallRefusesVoicePortsWithoutBundledVoice(t *testing.T) {
	err := validateInstallFlags(&installFlags{Project: defaultProject, NoVoice: true, VoiceTCPPort: 7883})
	var ee *ui.ExitError
	if !errors.As(err, &ee) || ee.Code != ui.ExitUsage || !strings.Contains(ee.Msg, "--no-bundled-voice") {
		t.Fatalf("got %v", err)
	}
	if err := validateInstallFlags(&installFlags{Project: defaultProject, NoVoice: true}); err != nil {
		t.Fatalf("--no-bundled-voice on its own: %v", err)
	}
}

func TestBundledServerIDStaysTakenForVoiceAdd(t *testing.T) {
	inst := &instance.Instance{Dir: t.TempDir(), State: &instance.State{Settings: map[string]string{}}}
	if got := takenVoiceIDs(nil, inst); !slices.Contains(got, "default-server-1") {
		t.Fatalf("taken IDs %v leave out the bundled server", got)
	}
	inst.State.Settings["FLUXER_LIVEKIT_DEFAULT_REGION"] = `{"id":"fra","name":"Frankfurt","emoji":"x","latitude":50,"longitude":8}`
	if got := takenVoiceIDs(nil, inst); !slices.Contains(got, "fra-server-1") {
		t.Fatalf("taken IDs %v leave out the bundled server of the fra region", got)
	}
	inst.State.Settings[settings.VoiceBundled] = "false"
	if got := takenVoiceIDs(nil, inst); slices.Contains(got, "fra-server-1") {
		t.Fatalf("taken IDs %v keep the bundled server after it is turned off", got)
	}
}

func TestLogsWithoutBundledVoiceRefusesTheVoicePart(t *testing.T) {
	inst := &instance.Instance{Dir: t.TempDir(), State: &instance.State{Settings: map[string]string{settings.VoiceBundled: "false"}}}
	_, err := runningPart(inst, []string{"voice"}, []string{"livekit"})
	var ee *ui.ExitError
	if !errors.As(err, &ee) || ee.Code != ui.ExitRefused || !strings.Contains(ee.Msg, "bundled LiveKit") {
		t.Fatalf("got %v", err)
	}
	if got, err := runningPart(inst, []string{"api"}, []string{"api"}); err != nil || !slices.Equal(got, []string{"api"}) {
		t.Fatalf("api part = %v, %v", got, err)
	}
	inst.State.Settings[settings.VoiceBundled] = "true"
	if got, err := runningPart(inst, []string{"voice"}, []string{"livekit"}); err != nil || !slices.Equal(got, []string{"livekit"}) {
		t.Fatalf("voice part with the bundled LiveKit = %v, %v", got, err)
	}
}
