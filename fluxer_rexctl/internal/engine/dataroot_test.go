// SPDX-License-Identifier: AGPL-3.0-or-later

package engine

import "testing"

func TestPlannedDataRoot(t *testing.T) {
	cases := []struct {
		daemon     string
		podmanOnly bool
		root       bool
		overlay2   string
		want       string
	}{
		{"", false, true, "", "/var/lib/containerd"},
		{`{"data-root": "/srv/docker"}`, false, true, "", "/var/lib/containerd"},
		{`{"data-root": "/srv/docker"}`, false, true, "/srv/docker/image/overlay2", "/srv/docker"},
		{`{"data-root": "/srv/docker", "features": {"containerd-snapshotter": false}}`, false, true, "", "/srv/docker"},
		{`{"graph": "/mnt/docker", "features": {"containerd-snapshotter": false}}`, false, false, "", "/mnt/docker"},
		{`{"data-root": "relative", "features": {"containerd-snapshotter": false}}`, false, true, "", "/var/lib/docker"},
		{"not json", false, true, "/var/lib/docker/image/overlay2", "/var/lib/docker"},
		{`{"data-root": "/srv/docker"}`, true, true, "", "/var/lib/containers/storage"},
		{"", true, false, "", "/home/rex/.local/share/containers/storage"},
	}
	for _, c := range cases {
		exists := func(p string) bool { return p == c.overlay2 }
		if got := plannedDataRoot([]byte(c.daemon), c.podmanOnly, c.root, "/home/rex/.local/share", exists); got != c.want {
			t.Errorf("plannedDataRoot(%q, podmanOnly=%v, root=%v, overlay2=%q) = %q, want %q", c.daemon, c.podmanOnly, c.root, c.overlay2, got, c.want)
		}
	}
}

func TestContainerdStore(t *testing.T) {
	if !containerdStore([][2]string{{"driver-type", "io.containerd.snapshotter.v1"}}) {
		t.Error("containerd snapshotter not detected")
	}
	if containerdStore([][2]string{{"Backing Filesystem", "extfs"}, {"Supports d_type", "true"}}) {
		t.Error("overlay2 reported as containerd")
	}
}
