// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"os"
	"path/filepath"
	"runtime"
	"testing"
)

func TestPodmanDockerWrapper(t *testing.T) {
	if runtime.GOOS != "linux" {
		t.Skip("the podman-docker wrapper only exists on Linux")
	}
	dir := t.TempDir()
	write := func(name, body string) string {
		p := filepath.Join(dir, name)
		if err := os.WriteFile(p, []byte(body), 0o755); err != nil {
			t.Fatal(err)
		}
		return p
	}
	podman := write("podman", "\x7fELF")
	link := filepath.Join(dir, "docker-link")
	if err := os.Symlink(podman, link); err != nil {
		t.Fatal(err)
	}
	cases := map[string]bool{
		write("docker-wrapper", "#!/bin/sh\n[ -e /etc/containers/nodocker ] || echo emulate\nexec /usr/bin/podman \"$@\"\n"): true,
		link:                                   true,
		write("docker", "\x7fELF real docker"): false,
		write("docker-other-script", "#!/bin/sh\nexec /usr/libexec/docker/cli \"$@\"\n"): false,
	}
	for path, want := range cases {
		if got := podmanDockerWrapper(path); got != want {
			t.Errorf("podmanDockerWrapper(%s) = %v, want %v", filepath.Base(path), got, want)
		}
	}
}
