// SPDX-License-Identifier: AGPL-3.0-or-later

package main

import (
	"context"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

type fakeResolver map[string]string

func (f fakeResolver) digest(_ context.Context, ref string) (string, error) {
	return f[ref], nil
}

func TestPinThirdPartyAddsTagAndDigest(t *testing.T) {
	set := stack.ImageSet{Images: map[string]string{"api": "ghcr.io/fluxerapp/fluxer-api@sha256:1"}}
	third := map[string]string{
		"postgres":       "postgres:16-alpine",
		"seaweedfs":      "chrislusf/seaweedfs:4.47",
		"seaweedfs-init": "chrislusf/seaweedfs:4.47",
		"nats":           "nats:2.14-alpine@sha256:9",
	}
	calls := 0
	r := fakeResolver{"postgres:16-alpine": "sha256:pg", "chrislusf/seaweedfs:4.47": "sha256:sw"}
	counting := countingResolver{r, &calls}
	if err := pinThirdParty(context.Background(), counting, &set, third); err != nil {
		t.Fatal(err)
	}
	want := map[string]string{
		"api":            "ghcr.io/fluxerapp/fluxer-api@sha256:1",
		"postgres":       "postgres:16-alpine@sha256:pg",
		"seaweedfs":      "chrislusf/seaweedfs:4.47@sha256:sw",
		"seaweedfs-init": "chrislusf/seaweedfs:4.47@sha256:sw",
		"nats":           "nats:2.14-alpine@sha256:9",
	}
	for svc, ref := range want {
		if set.Images[svc] != ref {
			t.Fatalf("%s = %q, want %q", svc, set.Images[svc], ref)
		}
	}
	if calls != 2 {
		t.Fatalf("looked up %d digests, want 2", calls)
	}
}

func TestPinThirdPartyRefusesAServiceTheSetAlreadyHas(t *testing.T) {
	set := stack.ImageSet{Images: map[string]string{"postgres": "x"}}
	if err := pinThirdParty(context.Background(), fakeResolver{}, &set, map[string]string{"postgres": "postgres:16-alpine"}); err == nil {
		t.Fatal("pinned over an existing image")
	}
}

type countingResolver struct {
	fakeResolver
	calls *int
}

func (c countingResolver) digest(ctx context.Context, ref string) (string, error) {
	*c.calls++
	return c.fakeResolver.digest(ctx, ref)
}
