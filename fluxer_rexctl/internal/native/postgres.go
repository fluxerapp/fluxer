// SPDX-License-Identifier: AGPL-3.0-or-later

package native

import (
	"context"
	"errors"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
)

const (
	MinPostgres = 14
	PostgresEOL = 14
)

const markedFile = "apt-marked-postgres"

type Postgres struct {
	DataDir   string
	Data      int
	Installed []int
	Broken    bool
}

func installedPostgres(dir string) []int {
	entries, err := os.ReadDir(dir)
	if err != nil {
		return nil
	}
	var out []int
	for _, e := range entries {
		major, err := strconv.Atoi(e.Name())
		if err != nil || major <= 0 {
			continue
		}
		if info, err := os.Stat(filepath.Join(dir, e.Name(), "bin", "postgres")); err == nil && info.Mode().IsRegular() {
			out = append(out, major)
		}
	}
	slices.SortFunc(out, func(a, b int) int { return b - a })
	return out
}

func dataPostgres(dir string) int {
	data, err := os.ReadFile(filepath.Join(dir, "PG_VERSION"))
	if err != nil {
		return 0
	}
	major, _ := strconv.Atoi(strings.TrimSpace(string(data)))
	return major
}

func (s *Stack) PostgresDir() string { return filepath.Join(s.Root(), "postgres") }

func (s *Stack) OldPostgresDir(major int) string {
	return s.PostgresDir() + "." + strconv.Itoa(major)
}

func (s *Stack) OldPostgresDirs() []string {
	matches, _ := filepath.Glob(s.PostgresDir() + ".*")
	var out []string
	for _, m := range matches {
		if _, err := strconv.Atoi(strings.TrimPrefix(m, s.PostgresDir()+".")); err != nil {
			continue
		}
		if _, err := os.Stat(filepath.Join(m, "PG_VERSION")); err == nil {
			out = append(out, m)
		}
	}
	return out
}

func (s *Stack) Postgres() Postgres {
	return readPostgres(s.PostgresDir(), postgresLibDir)
}

func readPostgres(dataDir, libDir string) Postgres {
	return Postgres{DataDir: dataDir, Data: dataPostgres(dataDir), Installed: installedPostgres(libDir), Broken: linked(dataDir)}
}

func linked(dataDir string) bool {
	control := filepath.Join(dataDir, "global", "pg_control")
	if _, err := os.Stat(control); err == nil {
		return false
	}
	_, err := os.Stat(control + ".old")
	return err == nil
}

func (s *Stack) PostgresUpgradeLogs() string {
	return filepath.Join(s.Root(), "tmp", "pg-upgrade", "pg_upgrade_output.d")
}

func (p Postgres) Newest() int {
	if len(p.Installed) == 0 {
		return 0
	}
	return p.Installed[0]
}

func (p Postgres) HasData() bool { return p.Data > 0 }

func (p Postgres) DataInstalled() bool { return slices.Contains(p.Installed, p.Data) }

func (p Postgres) Upgradable() bool { return p.HasData() && p.DataInstalled() && p.Newest() > p.Data }

func PackageName(major int) string { return "postgresql-" + strconv.Itoa(major) }

func (s *Stack) markedPath() string { return s.Inst.Path(instance.StateDirName, markedFile) }

func (s *Stack) KeepPostgres(ctx context.Context, majors ...int) error {
	path := s.markedPath()
	marked := readMarked(path)
	for _, m := range majors {
		if slices.Contains(marked, m) || aptManual(ctx, PackageName(m)) {
			continue
		}
		if err := runQuiet(ctx, "apt-mark", "manual", PackageName(m)); err != nil {
			return err
		}
		marked = append(marked, m)
	}
	return writeMarked(path, marked)
}

func (s *Stack) ReleasePostgres(ctx context.Context, majors ...int) error {
	path := s.markedPath()
	clusters := clusterMajors(ctx)
	var keep []int
	var errs []error
	for _, m := range readMarked(path) {
		if !slices.Contains(majors, m) || slices.Contains(clusters, m) {
			keep = append(keep, m)
			continue
		}
		if err := runQuiet(ctx, "apt-mark", "auto", PackageName(m)); err != nil {
			errs = append(errs, err)
			keep = append(keep, m)
		}
	}
	return errors.Join(append(errs, writeMarked(path, keep))...)
}

func aptManual(ctx context.Context, pkg string) bool {
	out, err := exec.CommandContext(ctx, "apt-mark", "showmanual", pkg).Output()
	return err == nil && strings.TrimSpace(string(out)) == pkg
}

func clusterMajors(ctx context.Context) []int {
	out, err := exec.CommandContext(ctx, "pg_lsclusters", "-h").Output()
	if err != nil {
		return nil
	}
	return parseMajors(string(out))
}

func parseMajors(s string) []int {
	var out []int
	for _, line := range strings.Split(s, "\n") {
		fields := strings.Fields(line)
		if len(fields) == 0 {
			continue
		}
		if m, err := strconv.Atoi(fields[0]); err == nil && m > 0 && !slices.Contains(out, m) {
			out = append(out, m)
		}
	}
	return out
}

func readMarked(path string) []int {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil
	}
	return parseMajors(string(data))
}

func writeMarked(path string, majors []int) error {
	if len(majors) == 0 {
		if err := os.Remove(path); err != nil && !errors.Is(err, fs.ErrNotExist) {
			return err
		}
		return nil
	}
	slices.Sort(majors)
	var b strings.Builder
	for _, m := range majors {
		b.WriteString(strconv.Itoa(m) + "\n")
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return err
	}
	return os.WriteFile(path, []byte(b.String()), 0o644)
}

func PostgresHere() int {
	if majors := installedPostgres(postgresLibDir); len(majors) > 0 && majors[0] >= MinPostgres {
		return majors[0]
	}
	return 0
}

func ExpectedPostgres(ctx context.Context) int {
	if here := PostgresHere(); here > 0 {
		return here
	}
	if major := candidatePostgres(ctx); major > 0 {
		return major
	}
	return releasePostgres(host.OSRelease())
}

func releasePostgres(id, version string) int {
	return map[string]int{
		"ubuntu 22.04": 14,
		"debian 12":    15,
		"ubuntu 24.04": 16,
		"debian 13":    17,
		"ubuntu 26.04": 18,
	}[id+" "+version]
}

func candidatePostgres(ctx context.Context) int {
	out, err := exec.CommandContext(ctx, "apt-cache", "policy", "postgresql").Output()
	if err != nil {
		return 0
	}
	return parseCandidate(string(out))
}

func parseCandidate(policy string) int {
	for _, line := range strings.Split(policy, "\n") {
		v, ok := strings.CutPrefix(strings.TrimSpace(line), "Candidate:")
		if !ok {
			continue
		}
		v = strings.TrimSpace(v)
		end := strings.IndexFunc(v, func(r rune) bool { return r < '0' || r > '9' })
		if end >= 0 {
			v = v[:end]
		}
		major, _ := strconv.Atoi(v)
		return major
	}
	return 0
}

func (p Postgres) Keep() []int {
	var out []int
	if p.Newest() > 0 {
		out = append(out, p.Newest())
	}
	if p.DataInstalled() && p.Data != p.Newest() {
		out = append(out, p.Data)
	}
	return out
}
