// SPDX-License-Identifier: AGPL-3.0-or-later

package engine

import (
	"slices"
	"testing"

	"github.com/compose-spec/compose-go/v2/types"
)

func TestDownKeepingRemovesOtherVolumes(t *testing.T) {
	label := func(short string) map[string]string { return map[string]string{"com.docker.compose.volume": short} }
	vols := []Volume{
		{Name: "fluxer_postgres-data", Project: "fluxer", Labels: label("postgres-data")},
		{Name: "fluxer_seaweedfs-data", Project: "fluxer", Labels: label("seaweedfs-data")},
		{Name: "fluxer_meilisearch-data", Project: "fluxer", Labels: label("meilisearch-data")},
		{Name: "custom-uploads", Project: "fluxer", Labels: label("seaweedfs-data")},
		{Name: "fluxer_valkey-data", Project: "fluxer", Labels: map[string]string{}},
		{Name: "fluxer_seaweedfs-old", Project: "", Labels: map[string]string{}},
		{Name: "blue_postgres-data", Project: "blue", Labels: label("postgres-data")},
	}
	got := volumesToDrop(nil, vols, "fluxer", []string{"seaweedfs-data"})
	want := []string{"fluxer_meilisearch-data", "fluxer_postgres-data", "fluxer_valkey-data"}
	if !slices.Equal(got, want) {
		t.Errorf("volumesToDrop = %v, want %v", got, want)
	}
	if got := volumesToDrop(nil, vols, "fluxer", nil); len(got) != 5 {
		t.Errorf("keeping nothing drops %v", got)
	}
}

func TestDownKeepingRemovesUnlabelledModelVolumes(t *testing.T) {
	model := &types.Project{Name: "fluxer", Volumes: types.Volumes{
		"postgres-data":  {Name: "fluxer_postgres-data"},
		"seaweedfs-data": {Name: "fluxer_seaweedfs-data"},
		"edge-data":      {Name: "fluxer_edge-data"},
		"edge-config":    {Name: "fluxer_edge-config"},
		"shared":         {Name: "shared", External: true},
	}}
	unlabelled := map[string]string{}
	vols := []Volume{
		{Name: "fluxer_edge-config", Labels: unlabelled},
		{Name: "fluxer_edge-data", Labels: unlabelled},
		{Name: "fluxer_postgres-data", Labels: unlabelled},
		{Name: "fluxer_seaweedfs-data", Labels: unlabelled},
		{Name: "shared", Labels: unlabelled},
	}
	got := volumesToDrop(model, vols, "fluxer", []string{"seaweedfs-data"})
	want := []string{"fluxer_edge-config", "fluxer_edge-data", "fluxer_postgres-data"}
	if !slices.Equal(got, want) {
		t.Errorf("volumesToDrop = %v, want %v", got, want)
	}
	renamed := &types.Project{Name: "fluxer", Volumes: types.Volumes{
		"postgres-data":  {Name: "fluxer_postgres-data"},
		"seaweedfs-data": {Name: "custom-uploads"},
	}}
	labelled := []Volume{
		{Name: "custom-uploads", Project: "fluxer", Labels: map[string]string{"com.docker.compose.volume": "uploads-old"}},
		{Name: "fluxer_postgres-data", Project: "fluxer", Labels: map[string]string{"com.docker.compose.volume": "postgres-data"}},
	}
	if got := volumesToDrop(renamed, labelled, "fluxer", []string{"seaweedfs-data"}); !slices.Equal(got, []string{"fluxer_postgres-data"}) {
		t.Errorf("volumesToDrop with a renamed uploads volume = %v", got)
	}
}
