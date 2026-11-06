// SPDX-License-Identifier: AGPL-3.0-or-later

package main

import (
	"strings"
	"testing"
	"testing/fstest"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

func composeSet(main, voice string) *stack.Set {
	return &stack.Set{Files: fstest.MapFS{
		stack.ComposeFile:              {Data: []byte(main)},
		"voice/voice-edge.compose.yml": {Data: []byte(voice)},
	}}
}

func TestThirdPartyIncludesVoiceComposeImages(t *testing.T) {
	got, err := thirdParty(composeSet(
		"services:\n  edge:\n    image: ${FLUXER_CADDY_IMAGE:-caddy:2.11-alpine}\n",
		"services:\n  edge:\n    image: ${FLUXER_CADDY_IMAGE:-caddy:2.11-alpine}\n  turn:\n    image: coturn/coturn:4.7\n",
	))
	if err != nil {
		t.Fatal(err)
	}
	if got["edge"] != "caddy:2.11-alpine" || got["turn"] != "coturn/coturn:4.7" {
		t.Fatalf("thirdParty() = %v", got)
	}
	if _, err := thirdParty(stack.Embedded()); err != nil {
		t.Fatalf("the shipped compose files: %v", err)
	}
}

func TestThirdPartyRefusesAServiceWithTwoImages(t *testing.T) {
	_, err := thirdParty(composeSet(
		"services:\n  edge:\n    image: ${FLUXER_CADDY_IMAGE:-caddy:2.11-alpine}\n",
		"services:\n  edge:\n    image: ${FLUXER_CADDY_IMAGE:-caddy:2.12-alpine}\n",
	))
	if err == nil || !strings.Contains(err.Error(), "edge") {
		t.Fatalf("thirdParty() error = %v", err)
	}
}
