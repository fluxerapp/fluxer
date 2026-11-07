// SPDX-License-Identifier: AGPL-3.0-or-later

package engine

import (
	"bytes"
	"encoding/json"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
)

const (
	dockerDaemonConfig = "/etc/docker/daemon.json"
	containerdRoot     = "/var/lib/containerd"
)

func PlannedDataRoot() string {
	if runtime.GOOS != "linux" {
		return ""
	}
	daemon, _ := os.ReadFile(dockerDaemonConfig)
	docker, dockerErr := exec.LookPath("docker")
	_, podmanErr := exec.LookPath("podman")
	podmanOnly := podmanErr == nil && (dockerErr != nil || PodmanDockerWrapper(docker))
	dataHome := os.Getenv("XDG_DATA_HOME")
	if dataHome == "" {
		if home, err := os.UserHomeDir(); err == nil {
			dataHome = filepath.Join(home, ".local", "share")
		}
	}
	return plannedDataRoot(daemon, podmanOnly, os.Geteuid() == 0, dataHome, exists)
}

func plannedDataRoot(daemon []byte, podmanOnly, root bool, dataHome string, exists func(string) bool) string {
	switch {
	case podmanOnly && root:
		return "/var/lib/containers/storage"
	case podmanOnly && dataHome != "":
		return filepath.Join(dataHome, "containers", "storage")
	case podmanOnly:
		return ""
	}
	var cfg struct {
		DataRoot string          `json:"data-root"`
		Graph    string          `json:"graph"`
		Features map[string]bool `json:"features"`
	}
	dataRoot := "/var/lib/docker"
	if json.Unmarshal(daemon, &cfg) == nil {
		for _, p := range []string{cfg.DataRoot, cfg.Graph} {
			if p = strings.TrimSpace(p); filepath.IsAbs(p) {
				dataRoot = p
				break
			}
		}
	}
	snapshotter, set := cfg.Features["containerd-snapshotter"]
	if (set && !snapshotter) || exists(filepath.Join(dataRoot, "image", "overlay2")) {
		return dataRoot
	}
	return containerdRoot
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
