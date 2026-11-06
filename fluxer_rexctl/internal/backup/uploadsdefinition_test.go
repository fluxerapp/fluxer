// SPDX-License-Identifier: AGPL-3.0-or-later

package backup

import (
	"maps"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

func TestUploadsDefinitionIgnoresThePinnedSeaweedfsDigest(t *testing.T) {
	set := stack.Embedded()
	digest := "@sha256:" + strings.Repeat("ab", 32)
	fluxer := map[string]string{}
	for svc := range stack.RegistryServices() {
		fluxer[svc] = "ghcr.io/fluxerapp/fluxer-" + svc + digest
	}
	pinned := maps.Clone(fluxer)
	pinned["seaweedfs"] = "chrislusf/seaweedfs:4.47" + digest
	pinned["seaweedfs-init"] = pinned["seaweedfs"]
	moved := maps.Clone(pinned)
	moved["seaweedfs"] = "chrislusf/seaweedfs:4.47@sha256:" + strings.Repeat("cd", 32)
	moved["seaweedfs-init"] = moved["seaweedfs"]
	state := func(refs map[string]string) *instance.State {
		return &instance.State{Settings: map[string]string{}, Images: instance.Images{Refs: refs}}
	}
	want := UploadsDefinition(set, state(fluxer))
	if want == "" {
		t.Fatal("no uploads definition")
	}
	for name, refs := range map[string]map[string]string{"pinned": pinned, "moved": moved} {
		if got := UploadsDefinition(set, state(refs)); got != want {
			t.Fatalf("%s seaweedfs image changed the uploads definition: %s, want %s", name, got, want)
		}
	}
}
