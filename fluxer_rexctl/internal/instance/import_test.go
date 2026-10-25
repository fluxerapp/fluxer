// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"os"
	"path/filepath"
	"slices"
	"testing"
)

func TestDropMergedShards(t *testing.T) {
	dir := t.TempDir()
	in := `x-local-env: &local-env
  FLUXER_S3_ENDPOINT: http://storage:8333

services:
  # Keep SeaweedFS from starting
  seaweedfs:
    profiles: ["seaweedfs-disabled"]
  api:
    environment: *local-env
    depends_on:
      seaweedfs-init: !reset null
  gifs:
    environment: *local-env
  gifs-shard:
    environment: *local-env

  # messages
  messages:
    environment: *local-env
  messages-shard:
    environment:
      FLUXER_S3_ENDPOINT: http://storage:8333
  users-shard:
    environment:
      OTHER: "1"
  unfurl:
    environment: *local-env
  unfurl-shard: {environment: *local-env}
`
	want := `x-local-env: &local-env
  FLUXER_S3_ENDPOINT: http://storage:8333

services:
  # Keep SeaweedFS from starting
  seaweedfs:
    profiles: ["seaweedfs-disabled"]
  api:
    environment: *local-env
    depends_on:
      seaweedfs-init: !reset null
  gifs:
    environment: *local-env

  # messages
  messages:
    environment: *local-env
  users-shard:
    environment:
      OTHER: "1"
  unfurl:
    environment: *local-env
`
	path := filepath.Join(dir, CustomCompose)
	if err := os.WriteFile(path, []byte(in), 0o644); err != nil {
		t.Fatal(err)
	}
	dropped, err := DropMergedShards(dir)
	if err != nil {
		t.Fatal(err)
	}
	if !slices.Equal(dropped, []string{"gifs-shard", "messages-shard", "unfurl-shard"}) {
		t.Fatalf("dropped %v", dropped)
	}
	got, _ := os.ReadFile(path)
	if string(got) != want {
		t.Fatalf("got\n%s", got)
	}
	if got := RetiredService(dir); got != "users-shard" {
		t.Fatalf("retired %q", got)
	}
}

func TestDropMergedShardsKeepsSharedAnchor(t *testing.T) {
	dir := t.TempDir()
	in := `services:
  gifs-shard: &shared
    environment:
      A: "1"
  gifs: *shared
`
	path := filepath.Join(dir, CustomCompose)
	if err := os.WriteFile(path, []byte(in), 0o644); err != nil {
		t.Fatal(err)
	}
	if dropped, err := DropMergedShards(dir); err != nil || dropped != nil {
		t.Fatalf("dropped %v, %v", dropped, err)
	}
	if got, _ := os.ReadFile(path); string(got) != in {
		t.Fatalf("changed to\n%s", got)
	}
}
