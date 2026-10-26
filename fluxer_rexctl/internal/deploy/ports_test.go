// SPDX-License-Identifier: AGPL-3.0-or-later

package deploy

import (
	"errors"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func TestPortOfDefaults(t *testing.T) {
	for _, v := range portVars {
		if got := portOf(nil, v); got != v.def {
			t.Errorf("portOf(%s) = %d, want default %d", v.name, got, v.def)
		}
		for _, bad := range []string{"", "0", "-5", "70000", "x"} {
			if got := portOf(map[string]string{v.name: bad}, v); got != v.def {
				t.Errorf("portOf(%s=%q) = %d, want default %d", v.name, bad, got, v.def)
			}
		}
		if got := portOf(map[string]string{v.name: "7999"}, v); got != 7999 {
			t.Errorf("portOf(%s=7999) = %d, want 7999", v.name, got)
		}
	}
}

func TestPortDiffVoiceOnly(t *testing.T) {
	old := map[string]string{}
	next := map[string]string{"FLUXER_LIVEKIT_TCP_PORT": "7883", "FLUXER_LIVEKIT_UDP_PORT": "7884"}
	added, removed := portDiff(old, next)
	wantAdded := []host.Port{{Number: 7883, Proto: "tcp"}, {Number: 7884, Proto: "udp"}}
	wantRemoved := []host.Port{{Number: 7881, Proto: "tcp"}, {Number: 7882, Proto: "udp"}}
	if !samePorts(added, wantAdded) || !samePorts(removed, wantRemoved) {
		t.Errorf("portDiff = added %v removed %v, want added %v removed %v", added, removed, wantAdded, wantRemoved)
	}
	if a, _ := portDiff(next, next); len(a) != 0 {
		t.Errorf("portDiff of equal envs added %v, want none", a)
	}
}

func TestPortKeyChanged(t *testing.T) {
	cases := map[bool][]string{
		true:  {settings.VoiceTCPPort},
		false: {settings.Domain, settings.MemoryProfile},
	}
	for want, changed := range cases {
		if got := portKeyChanged(changed); got != want {
			t.Errorf("portKeyChanged(%v) = %v, want %v", changed, got, want)
		}
	}
	for _, k := range []string{settings.EdgeHTTPPort, settings.EdgeHTTPSPort, settings.VoiceTCPPort, settings.VoiceUDPPort} {
		if !portKeyChanged([]string{settings.Domain, k}) {
			t.Errorf("portKeyChanged should be true for %s", k)
		}
	}
}

func TestCheckPortsDistinctDefaults(t *testing.T) {
	for _, mode := range []string{settings.EdgeBundled, settings.EdgeProxy, settings.EdgeTunnel} {
		if err := checkPortsDistinct(effectivePorts(nil, mode)); err != nil {
			t.Errorf("default %s install should have distinct ports, got %v", mode, err)
		}
	}
}

func TestCheckPortsDistinctVoiceHitsWeb(t *testing.T) {
	env := map[string]string{"FLUXER_LIVEKIT_TCP_PORT": "443"}
	err := checkPortsDistinct(effectivePorts(env, settings.EdgeBundled))
	if err == nil {
		t.Fatal("voice.tcp_port 443 in bundled mode should be refused")
	}
	if !strings.Contains(err.Error(), "the web address") || !strings.Contains(err.Error(), "443") {
		t.Errorf("message should name the web address and the port, got %q", err.Error())
	}
	var ee *ui.ExitError
	if errors.As(err, &ee) && !strings.Contains(ee.Fix, settings.VoiceTCPPort) {
		t.Errorf("fix should point at %s, got %q", settings.VoiceTCPPort, ee.Fix)
	}
}

func TestCheckPortsDistinctProxyModeDropsEdge(t *testing.T) {
	env := map[string]string{"FLUXER_LIVEKIT_TCP_PORT": "443"}
	if err := checkPortsDistinct(effectivePorts(env, settings.EdgeProxy)); err != nil {
		t.Errorf("voice.tcp_port 443 is fine in proxy mode, got %v", err)
	}
}

func samePorts(a, b []host.Port) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
