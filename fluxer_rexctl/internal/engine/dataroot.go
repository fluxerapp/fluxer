// SPDX-License-Identifier: AGPL-3.0-or-later

package engine

import (
	"bufio"
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"io"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"runtime"
	"strconv"
	"strings"
	"time"
)

const (
	dockerDaemonConfig       = "/etc/docker/daemon.json"
	ContainerdConfig         = "/etc/containerd/config.toml"
	systemContainerdAddress  = "/run/containerd/containerd.sock"
	defaultDockerRoot        = "/var/lib/docker"
	defaultContainerdRoot    = "/var/lib/containerd"
	podmanRootStorage        = "/var/lib/containers/storage"
	podmanSystemStorage      = "/etc/containers/storage.conf"
	podmanDefaultStorage     = "/usr/share/containers/storage.conf"
	containerdDefaultRelease = 29
)

var (
	priorDrivers  = []string{"overlay2", "fuse-overlayfs", "btrfs", "zfs"}
	remappedRoot  = regexp.MustCompile(`^\d+\.\d+$`)
	dockerVersion = regexp.MustCompile(`version (\d+)\.`)
)

type Roots struct {
	Data   string
	Images string
}

func PlannedRoots(prefer Kind) Roots {
	if runtime.GOOS != "linux" {
		return Roots{}
	}
	return hostPlan(prefer).roots()
}

type plan struct {
	podman      bool
	root        bool
	daemon      []byte
	driverEnv   string
	dockerMajor int
	containerd  []byte
	storage     []byte
	userStorage []byte
	dataHome    string
	expand      func(string) string
	prior       func(root string, remapped bool) (bool, error)
}

func hostPlan(prefer Kind) plan {
	docker, dockerErr := exec.LookPath("docker")
	_, podmanErr := exec.LookPath("podman")
	dockerFound := dockerErr == nil && !PodmanDockerWrapper(docker)
	p := plan{
		podman:    podmanErr == nil && prefer != Docker && (!dockerFound || prefer == Podman),
		root:      os.Geteuid() == 0,
		driverEnv: os.Getenv("DOCKER_DRIVER"),
		expand:    expandStoragePath,
		prior:     priorDriver,
	}
	if p.podman {
		p.storage = readFirst(podmanSystemStorage, podmanDefaultStorage)
		home, _ := os.UserHomeDir()
		configHome := os.Getenv("XDG_CONFIG_HOME")
		if configHome == "" && home != "" {
			configHome = filepath.Join(home, ".config")
		}
		if configHome != "" {
			p.userStorage, _ = os.ReadFile(filepath.Join(configHome, "containers", "storage.conf"))
		}
		p.dataHome = os.Getenv("XDG_DATA_HOME")
		if p.dataHome == "" && home != "" {
			p.dataHome = filepath.Join(home, ".local", "share")
		}
		return p
	}
	p.daemon, _ = os.ReadFile(dockerDaemonConfig)
	p.containerd, _ = os.ReadFile(ContainerdConfig)
	if dockerFound {
		p.dockerMajor = installedDockerMajor()
	}
	return p
}

func (p plan) roots() Roots {
	if p.podman {
		root := p.podmanRoot()
		return Roots{Data: root, Images: root}
	}
	var cfg struct {
		DataRoot      string          `json:"data-root"`
		Graph         string          `json:"graph"`
		StorageDriver string          `json:"storage-driver"`
		UsernsRemap   string          `json:"userns-remap"`
		Features      map[string]bool `json:"features"`
	}
	data := defaultDockerRoot
	if json.Unmarshal(p.daemon, &cfg) == nil {
		for _, dir := range []string{cfg.DataRoot, cfg.Graph} {
			if dir = strings.TrimSpace(dir); filepath.IsAbs(dir) {
				data = dir
				break
			}
		}
	}
	driver := strings.TrimSpace(p.driverEnv)
	if driver == "" {
		driver = strings.TrimSpace(cfg.StorageDriver)
	}
	snapshotter, set := cfg.Features["containerd-snapshotter"]
	switch {
	case set && !snapshotter:
		return Roots{Data: data, Images: data}
	case set && snapshotter:
		return Roots{Data: data, Images: p.containerdRoot()}
	case p.dockerMajor > 0 && p.dockerMajor < containerdDefaultRelease:
		return Roots{Data: data, Images: data}
	case driver == "vfs" || driver == "overlay2" || driver == "btrfs":
		return Roots{Data: data, Images: data}
	case driver != "":
		return Roots{Data: data, Images: p.containerdRoot()}
	}
	prior, err := p.prior(data, strings.TrimSpace(cfg.UsernsRemap) != "")
	switch {
	case err != nil:
		return Roots{Data: data}
	case prior:
		return Roots{Data: data, Images: data}
	}
	return Roots{Data: data, Images: p.containerdRoot()}
}

func (p plan) containerdRoot() string {
	if root := tomlString(p.containerd, "", "root"); filepath.IsAbs(root) {
		return root
	}
	return defaultContainerdRoot
}

func (p plan) podmanRoot() string {
	if p.root {
		if root := tomlString(p.storage, "storage", "graphroot"); filepath.IsAbs(root) {
			return root
		}
		return podmanRootStorage
	}
	for _, root := range []string{tomlString(p.userStorage, "storage", "graphroot"), tomlString(p.storage, "storage", "rootless_storage_path")} {
		if root = p.expand(root); filepath.IsAbs(root) {
			return root
		}
	}
	if p.dataHome == "" {
		return ""
	}
	return filepath.Join(p.dataHome, "containers", "storage")
}

func containerdImageRoot(dataRoot, address string, conf []byte, exists func(string) bool) string {
	managed := filepath.Join(dataRoot, "containerd", "daemon")
	if address != systemContainerdAddress && exists(managed) {
		return managed
	}
	return plan{containerd: conf}.containerdRoot()
}

func priorDriver(root string, remapped bool) (bool, error) {
	roots := []string{root}
	if remapped {
		entries, err := os.ReadDir(root)
		if err != nil && !errors.Is(err, fs.ErrNotExist) {
			return false, err
		}
		for _, entry := range entries {
			if entry.IsDir() && remappedRoot.MatchString(entry.Name()) {
				roots = append(roots, filepath.Join(root, entry.Name()))
			}
		}
	}
	for _, dir := range roots {
		for _, driver := range priorDrivers {
			entries, err := os.ReadDir(filepath.Join(dir, driver))
			switch {
			case errors.Is(err, fs.ErrNotExist):
			case err != nil:
				return false, err
			case len(entries) > 0:
				return true, nil
			}
		}
	}
	return false, nil
}

func expandStoragePath(path string) string {
	return os.Expand(path, func(name string) string {
		if name == "UID" {
			return strconv.Itoa(os.Getuid())
		}
		return os.Getenv(name)
	})
}

func installedDockerMajor() int {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	for _, bin := range []string{"dockerd", "docker"} {
		out, err := exec.CommandContext(ctx, bin, "--version").Output()
		if err != nil {
			continue
		}
		if m := dockerVersion.FindSubmatch(out); m != nil {
			n, _ := strconv.Atoi(string(m[1]))
			return n
		}
	}
	return 0
}

func readFirst(paths ...string) []byte {
	for _, path := range paths {
		if b, err := os.ReadFile(path); err == nil {
			return b
		}
	}
	return nil
}

func tomlString(doc []byte, section, key string) string {
	current := ""
	sc := bufio.NewScanner(bytes.NewReader(doc))
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		if strings.HasPrefix(line, "[") {
			current = strings.Trim(strings.TrimSpace(strings.Trim(line, "[]")), `"'`)
			continue
		}
		if current != section {
			continue
		}
		name, value, ok := strings.Cut(line, "=")
		if !ok || strings.Trim(strings.TrimSpace(name), `"'`) != key {
			continue
		}
		value = strings.TrimSpace(value)
		if value == "" {
			return ""
		}
		quote := value[0]
		if quote != '"' && quote != '\'' {
			return ""
		}
		if end := strings.IndexByte(value[1:], quote); end >= 0 {
			return value[1 : end+1]
		}
		return ""
	}
	return ""
}

func containerdStore(status [][2]string) bool {
	for _, kv := range status {
		if kv[0] == "driver-type" && strings.Contains(kv[1], "containerd.snapshotter") {
			return true
		}
	}
	return false
}

func PodmanDockerWrapper(path string) bool {
	if runtime.GOOS != "linux" {
		return false
	}
	if target, err := filepath.EvalSymlinks(path); err == nil && filepath.Base(target) == "podman" {
		return true
	}
	f, err := os.Open(path)
	if err != nil {
		return false
	}
	defer f.Close()
	head := make([]byte, 512)
	n, _ := io.ReadFull(f, head)
	return bytes.HasPrefix(head[:n], []byte("#!")) && bytes.Contains(head[:n], []byte("podman"))
}

func exists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}
