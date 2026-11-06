// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

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

func TestRunningRefsUsesThePinned3rdPartyImage(t *testing.T) {
	set := stack.Embedded()
	pinned := "postgres:16-alpine@sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
	refs := RunningRefs(set, map[string]string{"postgres": pinned}, Running{})
	if !slices.Contains(refs, pinned) || slices.Contains(refs, "postgres:16-alpine") {
		t.Fatalf("didn't use the pinned postgres image: %v", refs)
	}
	chosen := map[string]string{"FLUXER_POSTGRES_IMAGE": "postgres:16.9-alpine"}
	refs = RunningRefs(set, map[string]string{"postgres": pinned}, Running{Chosen: func(k string) string { return chosen[k] }})
	if !slices.Contains(refs, "postgres:16.9-alpine") || slices.Contains(refs, pinned) {
		t.Fatalf("used the pinned image instead of FLUXER_POSTGRES_IMAGE: %v", refs)
	}
}
