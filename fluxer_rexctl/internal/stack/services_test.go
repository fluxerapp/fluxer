// SPDX-License-Identifier: AGPL-3.0-or-later

package stack

import (
	"slices"
	"strings"
	"testing"
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
