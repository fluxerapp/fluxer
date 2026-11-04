// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"fmt"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const amd = "linux/amd64"

func imageSet(images map[string][]stack.Layer) stack.ImageSet {
	s := stack.ImageSet{Layers: map[string]stack.Layer{}, Platforms: map[string]map[string][]string{amd: {}}}
	for ref, layers := range images {
		for _, l := range layers {
			digest := "sha256:c-" + l.DiffID
			s.Layers[digest] = l
			s.Platforms[amd][ref] = append(s.Platforms[amd][ref], digest)
		}
	}
	return s
}

func TestImageNeedCountsEachLayerOnce(t *testing.T) {
	base := stack.Layer{DiffID: "base", Compressed: 30 << 20, Unpacked: 80 << 20}
	images := map[string][]stack.Layer{}
	var refs []string
	for i := range 10 {
		ref := fmt.Sprintf("ghcr.io/fluxerapp/fluxer-%d@sha256:%d", i, i)
		images[ref] = []stack.Layer{base, {DiffID: fmt.Sprint("own", i), Compressed: 1 << 20, Unpacked: 2 << 20}}
		refs = append(refs, ref, ref)
	}
	set := imageSet(images)

	got := ImageNeed(set, amd, refs, nil, true)
	want := base.Compressed + base.Unpacked + 10*(3<<20)
	if got != want {
		t.Fatalf("containerd store: got %d, want %d", got, want)
	}

	got = ImageNeed(set, amd, refs, map[string]bool{"base": true}, true)
	if want := uint64(10 * (3 << 20)); got != want {
		t.Fatalf("with the shared layer local: got %d, want %d", got, want)
	}

	got = ImageNeed(set, amd, refs, nil, false)
	want = base.Unpacked + 10*(2<<20) + base.Compressed + 2*(1<<20)
	if got != want {
		t.Fatalf("overlay store: got %d, want %d", got, want)
	}
}

func TestImageNeedGuessesUnknownImagesOnce(t *testing.T) {
	got := ImageNeed(stack.ImageSet{}, amd, []string{"postgres:16-alpine", "postgres:16-alpine", "nats:2.14-alpine"}, nil, true)
	if got != 2*imageGuess {
		t.Fatalf("got %d, want %d", got, 2*imageGuess)
	}
}

func TestLayersForMatchesAMirroredRef(t *testing.T) {
	set := imageSet(map[string][]stack.Layer{"ghcr.io/fluxerapp/fluxer-api@sha256:abc": {{DiffID: "a", Unpacked: 1}}})
	if _, ok := set.LayersFor(amd, "mirror.example.com/fluxerapp/fluxer-api@sha256:abc"); !ok {
		t.Fatal("a mirrored ref with the same digest has no layers")
	}
	if _, ok := set.LayersFor("linux/arm64", "ghcr.io/fluxerapp/fluxer-api@sha256:abc"); ok {
		t.Fatal("another platform returned layers")
	}
}

func TestSpacesCountsASharedDiskOnce(t *testing.T) {
	dir := t.TempDir()
	got := spaces([]Space{{Path: filepath.Join(dir, "images"), Images: true, Need: 5 << 30}, {Path: dir, Need: 1 << 20}})
	if len(got) != 1 {
		t.Fatalf("got %d disks, want 1", len(got))
	}
	if want := uint64(5<<30 + 1<<20 + diskReserve); got[0].Need != want {
		t.Fatalf("need %d, want %d", got[0].Need, want)
	}
}

func TestRunningRefsLeavesOutServicesThatDontRun(t *testing.T) {
	set := stack.Embedded()
	chosen := map[string]string{settings.ImagesPrefix + "postgres": "postgres:16.9-alpine"}
	all := RunningRefs(set, nil, Running{Voice: true, Chosen: func(k string) string { return chosen[k] }})
	lean := RunningRefs(set, nil, Running{ExternalStore: true})
	has := func(refs []string, prefix string) bool {
		return slices.ContainsFunc(refs, func(r string) bool { return strings.HasPrefix(r, prefix) })
	}
	if !has(all, "livekit/") || !has(all, "chrislusf/seaweedfs") || !slices.Contains(all, "postgres:16.9-alpine") {
		t.Fatalf("full stack refs: %v", all)
	}
	if has(lean, "livekit/") || has(lean, "chrislusf/seaweedfs") {
		t.Fatalf("a stack without voice or object store still runs them: %v", lean)
	}
}
