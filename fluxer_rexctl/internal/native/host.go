// SPDX-License-Identifier: AGPL-3.0-or-later

package native

import (
	"bufio"
	"bytes"
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
)

const PackagesFile = "share/runtime-packages"

const createClusterConf = "/etc/postgresql-common/createcluster.conf"

var (
	createClusterSetting = regexp.MustCompile(`(?m)^#?[[:space:]]*create_main_cluster[[:space:]]*=.*$`)
	createClusterOff     = regexp.MustCompile(`(?m)^create_main_cluster = false$`)
)

var ErrNoApt = errors.New("native: the native runtime installs its packages with apt, which this machine doesn't have")

func runLogged(ctx context.Context, log func(string), env []string, name string, args ...string) error {
	cmd := exec.CommandContext(ctx, name, args...)
	cmd.Env = append(os.Environ(), env...)
	out, err := cmd.StdoutPipe()
	if err != nil {
		return err
	}
	cmd.Stderr = cmd.Stdout
	if err := cmd.Start(); err != nil {
		return err
	}
	sc := bufio.NewScanner(out)
	var last []string
	for sc.Scan() {
		line := sc.Text()
		if log != nil {
			log(line)
		}
		last = append(last, line)
		if len(last) > 20 {
			last = last[1:]
		}
	}
	if err := cmd.Wait(); err != nil {
		return fmt.Errorf("%s %s: %w: %s", name, strings.Join(args, " "), err, strings.Join(last, "\n"))
	}
	return nil
}

func (s *Stack) Packages(version string) ([]string, error) {
	data, err := os.ReadFile(filepath.Join(s.ReleaseDir(version), PackagesFile))
	if err != nil {
		return nil, fmt.Errorf("native: Fluxer %s doesn't list its system packages: %w", version, err)
	}
	return strings.Fields(string(data)), nil
}

func missingPackages(ctx context.Context, packages []string) []string {
	var missing []string
	for _, p := range packages {
		out, err := exec.CommandContext(ctx, "dpkg-query", "-W", "-f=${Status}", p).Output()
		if err != nil || !strings.Contains(string(out), "install ok installed") {
			missing = append(missing, p)
		}
	}
	return missing
}

func (s *Stack) EnsurePackages(ctx context.Context, version string, log func(string)) error {
	if os.Geteuid() != 0 {
		return ErrNotRoot
	}
	if _, err := exec.LookPath("apt-get"); err != nil {
		return ErrNoApt
	}
	packages, err := s.Packages(version)
	if err != nil {
		return err
	}
	missing := missingPackages(ctx, packages)
	if len(missing) == 0 {
		return nil
	}
	env := []string{"DEBIAN_FRONTEND=noninteractive"}
	if err := runLogged(ctx, log, env, "apt-get", "update"); err != nil {
		return err
	}
	if err := runLogged(ctx, log, env, "apt-get", "install", "-y", "--no-install-recommends", "postgresql-common"); err != nil {
		return err
	}
	if err := noMainCluster(createClusterConf); err != nil {
		return err
	}
	if err := runLogged(ctx, log, env, "apt-get", append([]string{"install", "-y", "--no-install-recommends"}, missing...)...); err != nil {
		return err
	}
	if out, err := exec.CommandContext(ctx, "pg_lsclusters", "-h").Output(); err == nil && len(bytes.TrimSpace(out)) == 0 {
		_ = runLogged(ctx, log, nil, "systemctl", "disable", "--now", "postgresql")
	}
	return nil
}

func noMainCluster(path string) error {
	data, err := os.ReadFile(path)
	if err != nil {
		return err
	}
	out := createClusterSetting.ReplaceAll(data, []byte("create_main_cluster = false"))
	if !createClusterOff.Match(out) {
		return fmt.Errorf("native: couldn't set create_main_cluster = false in %s", path)
	}
	return os.WriteFile(path, out, 0o644)
}

func EnsureUser(ctx context.Context) error {
	if _, err := lookupUser(ServiceUser); err == nil {
		return nil
	}
	if os.Geteuid() != 0 {
		return ErrNotRoot
	}
	return runLogged(ctx, nil, nil, "useradd", "--system", "--user-group", "--home-dir", "/nonexistent", "--no-create-home", "--shell", "/usr/sbin/nologin", ServiceUser)
}
