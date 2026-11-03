// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"context"
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/user"
	"path/filepath"
	"slices"
	"time"
)

const DockerInstallScript = "https://get.docker.com"

var ErrRelogin = errors.New("host: log out and back in to use docker")

var dockerDistros = []string{"ubuntu", "debian", "raspbian", "fedora", "centos", "rhel"}

func CanInstallDocker(info *Info) bool {
	if info == nil || info.OS != "linux" || info.WSL {
		return false
	}
	if info.Arch != "amd64" && info.Arch != "arm64" {
		return false
	}
	if !info.Root && !has("sudo") {
		return false
	}
	return slices.Contains(dockerDistros, info.Distro)
}

func InstallDocker(ctx context.Context, info *Info, log func(string)) error {
	if !CanInstallDocker(info) {
		return errors.ErrUnsupported
	}
	script, err := downloadScript(ctx)
	if err != nil {
		return err
	}
	defer os.RemoveAll(filepath.Dir(script))
	if err := stream(ctx, log, "sh", script); err != nil {
		return err
	}
	if has("systemctl") {
		if err := stream(ctx, log, "systemctl", "enable", "--now", "docker"); err != nil {
			return err
		}
	}
	if info.Root {
		return nil
	}
	name := os.Getenv("USER")
	if u, err := user.Current(); err == nil && u.Username != "" {
		name = u.Username
	}
	if name == "" {
		return ErrRelogin
	}
	if err := stream(ctx, log, "usermod", "-aG", "docker", name); err != nil {
		return err
	}
	return ErrRelogin
}

func downloadScript(ctx context.Context) (string, error) {
	ctx, cancel := context.WithTimeout(ctx, 2*time.Minute)
	defer cancel()
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, DockerInstallScript, nil)
	if err != nil {
		return "", err
	}
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return "", fmt.Errorf("%s returned %d", DockerInstallScript, resp.StatusCode)
	}
	dir, err := os.MkdirTemp("", "rexctl-docker-")
	if err != nil {
		return "", err
	}
	path := filepath.Join(dir, "get-docker.sh")
	f, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o700)
	if err != nil {
		os.RemoveAll(dir)
		return "", err
	}
	n, err := io.Copy(f, io.LimitReader(resp.Body, 4<<20))
	if cerr := f.Close(); err == nil {
		err = cerr
	}
	if err == nil && n == 0 {
		err = fmt.Errorf("%s returned an empty script", DockerInstallScript)
	}
	if err != nil {
		os.RemoveAll(dir)
		return "", err
	}
	return path, nil
}
