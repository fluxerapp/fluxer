// SPDX-License-Identifier: AGPL-3.0-or-later

package engine

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"testing"
)

func TestPlannedDockerRoots(t *testing.T) {
	errDenied := errors.New("permission denied")
	cases := []struct {
		name      string
		daemon    string
		driverEnv string
		major     int
		conf      string
		prior     bool
		priorErr  error
		want      Roots
		remapSeen bool
	}{
		{name: "fresh install", want: Roots{Data: "/var/lib/docker", Images: "/var/lib/containerd"}},
		{name: "moved data root", daemon: `{"data-root": "/srv/docker"}`, want: Roots{Data: "/srv/docker", Images: "/var/lib/containerd"}},
		{name: "moved data root with overlay2 driver", daemon: `{"data-root": "/srv/docker", "storage-driver": "overlay2"}`, want: Roots{Data: "/srv/docker", Images: "/srv/docker"}},
		{name: "vfs driver", daemon: `{"storage-driver": "vfs"}`, want: Roots{Data: "/var/lib/docker", Images: "/var/lib/docker"}},
		{name: "btrfs driver", daemon: `{"storage-driver": "btrfs"}`, want: Roots{Data: "/var/lib/docker", Images: "/var/lib/docker"}},
		{name: "driver from DOCKER_DRIVER", driverEnv: "overlay2", want: Roots{Data: "/var/lib/docker", Images: "/var/lib/docker"}},
		{name: "snapshotter named as driver", daemon: `{"storage-driver": "zfs"}`, want: Roots{Data: "/var/lib/docker", Images: "/var/lib/containerd"}},
		{name: "snapshotter turned off", daemon: `{"data-root": "/srv/docker", "features": {"containerd-snapshotter": false}}`, want: Roots{Data: "/srv/docker", Images: "/srv/docker"}},
		{name: "snapshotter turned on beats overlay2", daemon: `{"storage-driver": "overlay2", "features": {"containerd-snapshotter": true}}`, want: Roots{Data: "/var/lib/docker", Images: "/var/lib/containerd"}},
		{name: "snapshotter turned on with older docker", daemon: `{"features": {"containerd-snapshotter": true}}`, major: 27, want: Roots{Data: "/var/lib/docker", Images: "/var/lib/containerd"}},
		{name: "older docker defaults to graph drivers", major: 28, want: Roots{Data: "/var/lib/docker", Images: "/var/lib/docker"}},
		{name: "docker 29 defaults to containerd", major: 29, want: Roots{Data: "/var/lib/docker", Images: "/var/lib/containerd"}},
		{name: "old graph driver data stays", daemon: `{"data-root": "/srv/docker"}`, prior: true, want: Roots{Data: "/srv/docker", Images: "/srv/docker"}},
		{name: "userns-remap looks under remapped roots", daemon: `{"userns-remap": "default"}`, prior: true, remapSeen: true, want: Roots{Data: "/var/lib/docker", Images: "/var/lib/docker"}},
		{name: "unreadable data root leaves images unknown", priorErr: errDenied, want: Roots{Data: "/var/lib/docker"}},
		{name: "legacy graph key", daemon: `{"graph": "/mnt/docker", "features": {"containerd-snapshotter": false}}`, want: Roots{Data: "/mnt/docker", Images: "/mnt/docker"}},
		{name: "relative data root ignored", daemon: `{"data-root": "relative", "features": {"containerd-snapshotter": false}}`, want: Roots{Data: "/var/lib/docker", Images: "/var/lib/docker"}},
		{name: "broken daemon.json", daemon: "not json", want: Roots{Data: "/var/lib/docker", Images: "/var/lib/containerd"}},
		{name: "containerd root moved", conf: "version = 3\nroot = \"/data/containerd\"\n[plugins]\nroot = \"/nope\"\n", want: Roots{Data: "/var/lib/docker", Images: "/data/containerd"}},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			var remapped bool
			p := plan{
				daemon:      []byte(c.daemon),
				driverEnv:   c.driverEnv,
				dockerMajor: c.major,
				containerd:  []byte(c.conf),
				prior: func(root string, r bool) (bool, error) {
					remapped = r
					return c.prior, c.priorErr
				},
			}
			if got := p.roots(); got != c.want {
				t.Errorf("roots() = %+v, want %+v", got, c.want)
			}
			if c.remapSeen && !remapped {
				t.Error("userns-remap set but remapped roots were not scanned")
			}
		})
	}
}

func TestPlannedPodmanRoots(t *testing.T) {
	expand := func(s string) string {
		return os.Expand(s, func(name string) string {
			return map[string]string{"HOME": "/home/rex", "UID": "1000"}[name]
		})
	}
	cases := []struct {
		name        string
		root        bool
		storage     string
		userStorage string
		dataHome    string
		want        string
	}{
		{name: "root default", root: true, want: "/var/lib/containers/storage"},
		{name: "root graphroot", root: true, storage: "[storage]\ndriver = \"overlay\"\ngraphroot = \"/srv/containers\"\n", want: "/srv/containers"},
		{name: "rootless default", dataHome: "/home/rex/.local/share", want: "/home/rex/.local/share/containers/storage"},
		{name: "rootless user graphroot", userStorage: "[storage]\ngraphroot = \"$HOME/big/storage\"\n", dataHome: "/home/rex/.local/share", want: "/home/rex/big/storage"},
		{name: "rootless system path", storage: "[storage]\nrootless_storage_path = \"/srv/rootless/$UID\"\n", dataHome: "/home/rex/.local/share", want: "/srv/rootless/1000"},
		{name: "root ignores user file", root: true, userStorage: "[storage]\ngraphroot = \"/home/rex/x\"\n", want: "/var/lib/containers/storage"},
		{name: "rootless unknown home", want: ""},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			p := plan{podman: true, root: c.root, storage: []byte(c.storage), userStorage: []byte(c.userStorage), dataHome: c.dataHome, expand: expand}
			if got := p.roots(); got != (Roots{Data: c.want, Images: c.want}) {
				t.Errorf("roots() = %+v, want both %q", got, c.want)
			}
		})
	}
}

func TestContainerdImageRoot(t *testing.T) {
	managed := "/srv/docker/containerd/daemon"
	cases := []struct {
		name    string
		address string
		conf    string
		managed bool
		want    string
	}{
		{name: "dockerd runs its own containerd", address: "/var/run/docker/containerd/containerd.sock", managed: true, want: managed},
		{name: "unknown address with managed data", managed: true, want: managed},
		{name: "system containerd", address: "/run/containerd/containerd.sock", managed: true, want: "/var/lib/containerd"},
		{name: "system containerd with moved root", address: "/run/containerd/containerd.sock", conf: "root = '/data/containerd'\n", want: "/data/containerd"},
		{name: "custom address without managed data", address: "/run/other.sock", want: "/var/lib/containerd"},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			exists := func(p string) bool { return c.managed && p == managed }
			if got := containerdImageRoot("/srv/docker", c.address, []byte(c.conf), exists); got != c.want {
				t.Errorf("containerdImageRoot = %q, want %q", got, c.want)
			}
		})
	}
}

func TestPriorDriver(t *testing.T) {
	root := t.TempDir()
	if prior, err := priorDriver(root, false); err != nil || prior {
		t.Fatalf("empty root: prior=%v err=%v", prior, err)
	}
	if err := os.Mkdir(filepath.Join(root, "overlay2"), 0o755); err != nil {
		t.Fatal(err)
	}
	if prior, _ := priorDriver(root, false); prior {
		t.Fatal("an empty overlay2 folder counted as prior data")
	}
	remapped := filepath.Join(root, "165536.165536", "overlay2", "l")
	if err := os.MkdirAll(remapped, 0o755); err != nil {
		t.Fatal(err)
	}
	if prior, _ := priorDriver(root, false); prior {
		t.Fatal("remapped data counted without userns-remap")
	}
	if prior, err := priorDriver(root, true); err != nil || !prior {
		t.Fatalf("remapped data missed: prior=%v err=%v", prior, err)
	}
	if os.Geteuid() == 0 {
		return
	}
	locked := filepath.Join(root, "locked")
	if err := os.MkdirAll(filepath.Join(locked, "overlay2"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.Chmod(locked, 0); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.Chmod(locked, 0o755) })
	if _, err := priorDriver(locked, false); !errors.Is(err, fs.ErrPermission) {
		t.Fatalf("unreadable root: err=%v, want a permission error", err)
	}
}

func TestTomlString(t *testing.T) {
	doc := []byte("# comment\nroot = \"/top\" # note\n[storage]\ngraphroot = '/g'\n[storage.options]\ngraphroot = \"/nested\"\n")
	if got := tomlString(doc, "", "root"); got != "/top" {
		t.Errorf("top-level root = %q", got)
	}
	if got := tomlString(doc, "storage", "graphroot"); got != "/g" {
		t.Errorf("storage graphroot = %q", got)
	}
	if got := tomlString(doc, "storage", "missing"); got != "" {
		t.Errorf("missing key = %q", got)
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

func TestStores(t *testing.T) {
	data := t.TempDir()
	images := t.TempDir()
	dir := t.TempDir()
	cases := []struct {
		name string
		e    *Engine
		want Roots
	}{
		{name: "split image root", e: &Engine{DataRoot: data, ImageRoot: images}, want: Roots{Data: data, Images: images}},
		{name: "same root", e: &Engine{DataRoot: data, ImageRoot: data}, want: Roots{Data: data, Images: data}},
		{name: "image root missing", e: &Engine{DataRoot: data, ImageRoot: filepath.Join(images, "gone")}, want: Roots{Data: data, Images: data}},
		{name: "no data root", e: &Engine{}, want: Roots{Data: dir, Images: dir}},
	}
	for _, c := range cases {
		if got := Stores(dir, c.e, ""); got != c.want {
			t.Errorf("%s: Stores = %+v, want %+v", c.name, got, c.want)
		}
	}
}
