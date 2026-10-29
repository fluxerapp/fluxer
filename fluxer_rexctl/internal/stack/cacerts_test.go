// SPDX-License-Identifier: AGPL-3.0-or-later

package stack

import (
	"slices"
	"strings"
	"testing"

	"go.yaml.in/yaml/v4"
)

func composeVolumes(t *testing.T) map[string][]string {
	t.Helper()
	data, err := ReadFile(Embedded(), "docker-compose.yml")
	if err != nil {
		t.Fatal(err)
	}
	var compose struct {
		Services map[string]struct {
			Volumes []string `yaml:"volumes"`
		} `yaml:"services"`
	}
	if err := yaml.Unmarshal(data, &compose); err != nil {
		t.Fatal(err)
	}
	out := map[string][]string{}
	for name, svc := range compose.Services {
		out[name] = svc.Volumes
	}
	return out
}

func TestNodeServicesMountOnlyTheCAFolder(t *testing.T) {
	vols := composeVolumes(t)
	want := "./ca:/etc/fluxer/ca:ro"
	for _, svc := range []string{"api", "worker"} {
		if !slices.Contains(vols[svc], want) {
			t.Errorf("%s volumes = %v, want %q so FLUXER_NODE_EXTRA_CA_CERTS can point at a CA file", svc, vols[svc], want)
		}
		for _, v := range vols[svc] {
			if strings.HasPrefix(v, "./certs") {
				t.Errorf("%s mounts %q, which holds the edge TLS private keys", svc, v)
			}
		}
	}
	if !slices.Contains(vols["edge"], "./certs:/certs:ro") {
		t.Errorf("edge volumes = %v, want the certs folder", vols["edge"])
	}
}

func TestMountedDirsMatchCompose(t *testing.T) {
	vols := composeVolumes(t)
	for dir, services := range MountedDirs {
		for name, mounts := range vols {
			uses := slices.ContainsFunc(mounts, func(v string) bool { return strings.HasPrefix(v, "./"+dir+":") })
			if uses != slices.Contains(services, name) {
				t.Errorf("%s mounts ./%s = %v, but MountedDirs lists %v", name, dir, uses, services)
			}
		}
	}
}
