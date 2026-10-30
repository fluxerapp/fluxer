// SPDX-License-Identifier: AGPL-3.0-or-later

package stack

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestBundlePinsTheVoiceImages(t *testing.T) {
	compose, err := ReadFile(Embedded(), ComposeFile)
	if err != nil {
		t.Fatal(err)
	}
	found, err := imageLines(compose)
	if err != nil {
		t.Fatal(err)
	}
	images := map[string]string{}
	for _, img := range found {
		images[img.service] = "registry.example.com/" + img.service + ":pinned"
	}
	dir := t.TempDir()
	if err := Bundle(dir, images); err != nil {
		t.Fatal(err)
	}
	for file, want := range map[string]string{
		"voice.compose.yml":      "${FLUXER_LIVEKIT_IMAGE:-registry.example.com/livekit:pinned}",
		"voice-edge.compose.yml": "${FLUXER_CADDY_IMAGE:-registry.example.com/edge:pinned}",
	} {
		data, err := os.ReadFile(filepath.Join(dir, "voice", file))
		if err != nil {
			t.Fatal(err)
		}
		if !strings.Contains(string(data), want) {
			t.Errorf("%s doesn't pin %s", file, want)
		}
	}
}
