// SPDX-License-Identifier: AGPL-3.0-or-later

package stack

import (
	"slices"
	"strings"
	"testing"

	"go.yaml.in/yaml/v4"
)

func TestPortsDefaults(t *testing.T) {
	for _, env := range []map[string]string{nil, {}, {VoiceTCPPortKey: "", VoiceUDPPortKey: "  "}} {
		got := Ports(env)
		if !slices.Equal(got, PublishedPorts) {
			t.Errorf("Ports(%v) = %v, want defaults %v", env, got, PublishedPorts)
		}
	}
}

func TestPortsOverride(t *testing.T) {
	got := Ports(map[string]string{VoiceTCPPortKey: "7883", VoiceUDPPortKey: "7884"})
	want := []Port{
		{Number: 80, Proto: "tcp"},
		{Number: 443, Proto: "tcp"},
		{Number: 443, Proto: "udp"},
		{Number: 7883, Proto: "tcp"},
		{Number: 7884, Proto: "udp"},
	}
	if !slices.Equal(got, want) {
		t.Errorf("Ports override = %v, want %v", got, want)
	}
}

func TestPortsInvalidKeepsDefault(t *testing.T) {
	for _, bad := range []string{"0", "-1", "70000", "notaport"} {
		got := Ports(map[string]string{VoiceTCPPortKey: bad, VoiceUDPPortKey: bad})
		if got[3].Number != 7881 || got[4].Number != 7882 {
			t.Errorf("Ports with %q = %v, want voice defaults 7881/7882", bad, got)
		}
	}
}

func TestComposeVoicePortsShareOneVar(t *testing.T) {
	data, err := ReadFile(Embedded(), "docker-compose.yml")
	if err != nil {
		t.Fatal(err)
	}
	s := string(data)
	wants := []string{
		"tcp_port: ${FLUXER_LIVEKIT_TCP_PORT:-7881}",
		"udp_port: ${FLUXER_LIVEKIT_UDP_PORT:-7882}",
		`"${FLUXER_LIVEKIT_TCP_PORT:-7881}:${FLUXER_LIVEKIT_TCP_PORT:-7881}"`,
		`"${FLUXER_LIVEKIT_UDP_PORT:-7882}:${FLUXER_LIVEKIT_UDP_PORT:-7882}/udp"`,
	}
	for _, want := range wants {
		if !strings.Contains(s, want) {
			t.Errorf("docker-compose.yml is missing %q", want)
		}
	}
	if !strings.Contains(s, "port: 7880") {
		t.Error("docker-compose.yml should keep the livekit signal port 7880 fixed and internal")
	}
}

func TestNatsClientsWaitForHealthyNats(t *testing.T) {
	data, err := ReadFile(Embedded(), "docker-compose.yml")
	if err != nil {
		t.Fatal(err)
	}
	var compose struct {
		Services map[string]struct {
			DependsOn map[string]struct {
				Condition string `yaml:"condition"`
			} `yaml:"depends_on"`
			Healthcheck struct {
				Test []string `yaml:"test"`
			} `yaml:"healthcheck"`
		} `yaml:"services"`
	}
	if err := yaml.Unmarshal(data, &compose); err != nil {
		t.Fatal(err)
	}
	for _, name := range []string{"api", "worker", "gateway", "push", "media-proxy", "snowflakes", "users", "gifs", "messages", "unfurl"} {
		service, ok := compose.Services[name]
		if !ok {
			t.Errorf("docker-compose.yml has no %s service", name)
			continue
		}
		if got := service.DependsOn["nats"].Condition; got != "service_healthy" {
			t.Errorf("%s depends on nats with condition %q, want service_healthy", name, got)
		}
	}
	check := strings.Join(compose.Services["gateway"].Healthcheck.Test, " ")
	if !strings.Contains(check, "GET /_health/ready ") {
		t.Errorf("gateway healthcheck %q should ask /_health/ready, which fails while the gateway's NATS subscriptions are missing", check)
	}
}

func TestPortsWithoutBundledVoice(t *testing.T) {
	got := Ports(map[string]string{VoiceBundledKey: "false", VoiceTCPPortKey: "7883"})
	want := []Port{
		{Number: 80, Proto: "tcp"},
		{Number: 443, Proto: "tcp"},
		{Number: 443, Proto: "udp"},
	}
	if !slices.Equal(got, want) {
		t.Errorf("Ports without bundled voice = %v, want %v", got, want)
	}
	if got := Ports(map[string]string{VoiceBundledKey: "true"}); !slices.Equal(got, PublishedPorts) {
		t.Errorf("Ports with bundled voice = %v", got)
	}
}

func TestOverlaysForBundledVoice(t *testing.T) {
	for value, want := range map[string]bool{"": false, "true": false, "1": false, "false": true, "0": true, "nonsense": false} {
		got := slices.Contains(OverlaysFor(map[string]string{"voice.bundled": value}), OverlayExternalVoice)
		if got != want {
			t.Errorf("voice.bundled %q: external voice overlay = %v, want %v", value, got, want)
		}
	}
	data, err := ReadFile(Embedded(), OverlayFiles[OverlayExternalVoice])
	if err != nil {
		t.Fatal(err)
	}
	var overlay struct {
		Services map[string]struct {
			Profiles []string `yaml:"profiles"`
		} `yaml:"services"`
	}
	if err := yaml.Unmarshal(data, &overlay); err != nil {
		t.Fatal(err)
	}
	if got := overlay.Services["livekit"].Profiles; !slices.Equal(got, []string{"bundled-voice"}) {
		t.Errorf("livekit profiles = %v", got)
	}
}
