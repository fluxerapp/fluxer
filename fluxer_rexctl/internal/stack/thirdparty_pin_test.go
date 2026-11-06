// SPDX-License-Identifier: AGPL-3.0-or-later

package stack

import (
	"path"
	"strings"
	"testing"
)

func pinnedSet(t *testing.T) (shipped []byte, images map[string]string) {
	t.Helper()
	shipped, err := ReadFile(Embedded(), ComposeFile)
	if err != nil {
		t.Fatal(err)
	}
	found, err := imageLines(shipped)
	if err != nil {
		t.Fatal(err)
	}
	digest := "@sha256:" + strings.Repeat("ab", 32)
	images = map[string]string{}
	for _, img := range found {
		if strings.HasPrefix(img.value, "${FLUXER_REGISTRY") {
			head, _, _ := strings.Cut(img.value, ":${FLUXER_IMAGE_TAG")
			images[img.service] = "ghcr.io/fluxerapp/" + path.Base(head) + digest
			continue
		}
		def, ok := ImageDefault(img.value)
		if !ok {
			t.Fatalf("%s has no default image: %s", img.service, img.value)
		}
		images[img.service] = def + digest
	}
	return shipped, images
}

func TestPinImagesPinsThirdPartyDefaultsAndKeepsTheOverride(t *testing.T) {
	shipped, images := pinnedSet(t)
	got, err := pinImages(shipped, "test", images)
	if err != nil {
		t.Fatal(err)
	}
	pinned := ComposeImages(got)
	want := "${FLUXER_POSTGRES_IMAGE:-" + images["postgres"] + "}"
	if pinned["postgres"] != want {
		t.Fatalf("postgres image = %q, want %q", pinned["postgres"], want)
	}
	for _, svc := range []string{"nats", "edge", "valkey"} {
		if !strings.Contains(pinned[svc], "@sha256:") {
			t.Fatalf("%s isn't pinned: %q", svc, pinned[svc])
		}
	}
	def, _ := ImageDefault(pinned["postgres"])
	if major, err := PostgresMajor(def); err != nil || major != 16 {
		t.Fatalf("PostgresMajor(%q) = %d, %v", def, major, err)
	}
}

func TestUnpinImagesRestoresPinnedThirdPartyImages(t *testing.T) {
	shipped, images := pinnedSet(t)
	pinned, err := pinImages(shipped, "test", images)
	if err != nil {
		t.Fatal(err)
	}
	if got := UnpinImages(pinned); string(got) != string(shipped) {
		t.Fatalf("unpinning didn't give back the shipped compose file:\n%s", got)
	}
}

func TestUnpinImagesKeepsAnImageTheUserChose(t *testing.T) {
	shipped, _ := pinnedSet(t)
	mine := strings.Replace(string(shipped), "${FLUXER_POSTGRES_IMAGE:-postgres:16-alpine}", "${FLUXER_POSTGRES_IMAGE:-postgres:16.9-alpine}", 1)
	if got := UnpinImages([]byte(mine)); string(got) != mine {
		t.Fatal("unpinning changed an image that wasn't pinned by digest")
	}
}

func TestRegistryServicesLeavesOutThirdPartyImages(t *testing.T) {
	got := RegistryServices()
	if !got["api"] || got["postgres"] || got["edge"] {
		t.Fatalf("RegistryServices() = %v", got)
	}
}

func TestPinImagesLeavesAnImageWhosePinIsForAnotherTag(t *testing.T) {
	shipped, images := pinnedSet(t)
	images["postgres"] = "postgres:17-alpine@sha256:" + strings.Repeat("cd", 32)
	got, err := pinImages(shipped, "test", images)
	if err != nil {
		t.Fatal(err)
	}
	if pinned := ComposeImages(got)["postgres"]; pinned != "${FLUXER_POSTGRES_IMAGE:-postgres:16-alpine}" {
		t.Fatalf("postgres image = %q", pinned)
	}
}
