// SPDX-License-Identifier: AGPL-3.0-or-later

package stack

import (
	"fmt"
	"testing"
)

const amd = "linux/amd64"

func imageSet(images map[string][]Layer) ImageSet {
	s := ImageSet{Layers: map[string]Layer{}, Platforms: map[string]map[string][]string{amd: {}}}
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
	base := Layer{DiffID: "base", Compressed: 30 << 20, Unpacked: 80 << 20}
	images := map[string][]Layer{}
	var refs []string
	for i := range 10 {
		ref := fmt.Sprintf("ghcr.io/fluxerapp/fluxer-%d@sha256:%d", i, i)
		images[ref] = []Layer{base, {DiffID: fmt.Sprint("own", i), Compressed: 1 << 20, Unpacked: 2 << 20}}
		refs = append(refs, ref, ref)
	}
	set := imageSet(images)

	if got, want := set.Need(amd, refs, nil), base.Compressed+base.Unpacked+10*(3<<20); got != want {
		t.Fatalf("got %d, want %d", got, want)
	}
	if got, want := set.Need(amd, refs, map[string]bool{"base": true}), uint64(10*(3<<20)); got != want {
		t.Fatalf("with the shared layer local: got %d, want %d", got, want)
	}
}

func TestImageNeedGuessesUnknownImagesOnce(t *testing.T) {
	got := ImageSet{}.Need(amd, []string{"postgres:16-alpine", "postgres:16-alpine", "nats:2.14-alpine"}, nil)
	if got != 2*ImageGuess {
		t.Fatalf("got %d, want %d", got, 2*ImageGuess)
	}
}

func TestLayersForMatchesAMirroredRef(t *testing.T) {
	set := imageSet(map[string][]Layer{"ghcr.io/fluxerapp/fluxer-api@sha256:abc": {{DiffID: "a", Unpacked: 1}}})
	if _, ok := set.LayersFor(amd, "mirror.example.com/fluxerapp/fluxer-api@sha256:abc"); !ok {
		t.Fatal("a mirrored ref with the same digest has no layers")
	}
	if _, ok := set.LayersFor("linux/arm64", "ghcr.io/fluxerapp/fluxer-api@sha256:abc"); ok {
		t.Fatal("another platform returned layers")
	}
}
