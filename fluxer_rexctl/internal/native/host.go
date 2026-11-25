// SPDX-License-Identifier: AGPL-3.0-or-later

package native

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"os/user"
	"path/filepath"
	"regexp"
	"strings"
)

const packagesFile = "share/runtime-packages"

const CreateClusterConf = "/etc/postgresql-common/createcluster.conf"

const postgresLibDir = "/usr/lib/postgresql"

var (
	createClusterSetting = regexp.MustCompile(`(?m)^#?[[:space:]]*create_main_cluster[[:space:]]*=.*$`)
	createClusterOff     = regexp.MustCompile(`(?m)^create_main_cluster = false$`)
)

var ErrNoApt = errors.New("native: the native runtime installs its packages with apt, which this machine doesn't have")

func runQuiet(ctx context.Context, name string, args ...string) error {
	cmd := exec.CommandContext(ctx, name, args...)
	ownGroup(cmd)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("%s %s: %w: %s", name, strings.Join(args, " "), err, strings.TrimSpace(string(out)))
	}
	return nil
}

func (s *Stack) Packages(version string) ([]string, error) {
	data, err := os.ReadFile(filepath.Join(s.ReleaseDir(version), packagesFile))
	if err != nil {
		return nil, fmt.Errorf("native: Fluxer %s doesn't list its system packages: %w", version, err)
	}
	return strings.Fields(string(data)), nil
}

func neededPackages(packages []string, here int) []string {
	if here == 0 {
		return packages
	}
	var out []string
	for _, p := range packages {
		if p != "postgresql" {
			out = append(out, p)
		}
	}
	return out
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

func (s *Stack) EnsurePackages(ctx context.Context, version string, r AptReporter) error {
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
	if missing := missingPackages(ctx, neededPackages(packages, PostgresHere())); len(missing) > 0 {
		if err := s.installPackages(ctx, missing, r); err != nil {
			return err
		}
	}
	return s.KeepPostgres(ctx, s.Postgres().Keep()...)
}

func (s *Stack) DownloadPackages(ctx context.Context, version string, r AptReporter) error {
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
	missing := missingPackages(ctx, append([]string{"postgresql-common"}, neededPackages(packages, PostgresHere())...))
	if len(missing) == 0 {
		return nil
	}
	a, closeLog := s.newApt(r)
	defer closeLog()
	if err := a.run(ctx, aptStage{title: "Updating the package lists"}, "update"); err != nil {
		return err
	}
	return a.run(ctx, aptStage{title: "Downloading the system packages Fluxer needs"}, append([]string{"install", "-y", "--download-only", "--no-install-recommends"}, missing...)...)
}

func (s *Stack) installPackages(ctx context.Context, missing []string, r AptReporter) error {
	a, closeLog := s.newApt(r)
	defer closeLog()
	if err := a.run(ctx, aptStage{title: "Updating the package lists"}, "update"); err != nil {
		return err
	}
	if err := a.run(ctx, aptStage{title: "Installing the Postgres tools", downloading: "Downloading the Postgres tools", installing: "Installing the Postgres tools"}, "install", "-y", "--no-install-recommends", "postgresql-common"); err != nil {
		return err
	}
	if err := noMainCluster(CreateClusterConf); err != nil {
		return err
	}
	noun := fmt.Sprintf("%d system packages", len(missing))
	if len(missing) == 1 {
		noun = "1 system package"
	}
	st := aptStage{title: "Installing " + noun, downloading: "Downloading " + noun, installing: "Installing " + noun}
	if err := a.run(ctx, st, append([]string{"install", "-y", "--no-install-recommends"}, missing...)...); err != nil {
		return err
	}
	if out, err := exec.CommandContext(ctx, "pg_lsclusters", "-h").Output(); err == nil && len(bytes.TrimSpace(out)) == 0 {
		_ = runQuiet(ctx, "systemctl", "disable", "--now", "postgresql")
	}
	return nil
}

func NoMainCluster() bool {
	data, err := os.ReadFile(CreateClusterConf)
	return err == nil && createClusterOff.Match(data)
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
	if _, err := user.Lookup(ServiceUser); err == nil {
		return nil
	}
	if os.Geteuid() != 0 {
		return ErrNotRoot
	}
	return runQuiet(ctx, "useradd", "--system", "--user-group", "--home-dir", "/nonexistent", "--no-create-home", "--shell", "/usr/sbin/nologin", ServiceUser)
}
