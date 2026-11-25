// SPDX-License-Identifier: AGPL-3.0-or-later

package pg

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
)

const (
	upgradeSuffix = ".upgrade"
	markerSuffix  = ".upgrading"
	outputDir     = "pg_upgrade_output.d"
	checksumsOff  = 18
)

type UpgradeOptions struct {
	DataDir  string
	WorkDir  string
	User     string
	Password string
	From     Install
	To       Install
	Log      io.Writer
}

func (o UpgradeOptions) log() io.Writer {
	if o.Log == nil {
		return io.Discard
	}
	return o.Log
}

func Upgrade(ctx context.Context, o UpgradeOptions) error {
	if err := FinishUpgrade(o.DataDir); err != nil {
		return err
	}
	if Linked(o.DataDir) {
		return &BrokenError{DataDir: o.DataDir, Major: o.From.Major}
	}
	if o.To.Major <= o.From.Major {
		return fmt.Errorf("postgres: the database is already PostgreSQL %d, and %d isn't newer", o.From.Major, o.To.Major)
	}
	if _, err := os.Stat(OldDir(o.DataDir, o.From.Major)); err == nil {
		return fmt.Errorf("postgres: %s from an earlier move is in the way. Delete it with sudo rexctl postgres upgrade --cleanup, or fluxerd postgres-upgrade --cleanup without rexctl, and try again", OldDir(o.DataDir, o.From.Major))
	}
	for _, tool := range []struct {
		in   Install
		name string
	}{{o.From, "postgres"}, {o.From, "pg_ctl"}, {o.From, "pg_controldata"}, {o.To, "postgres"}, {o.To, "pg_ctl"}, {o.To, "initdb"}, {o.To, "pg_upgrade"}} {
		if _, err := os.Stat(filepath.Join(tool.in.BinDir, tool.name)); err != nil {
			return fmt.Errorf("postgres: PostgreSQL %d has no %s in %s. Install postgresql-%d", tool.in.Major, tool.name, tool.in.BinDir, tool.in.Major)
		}
	}
	on, err := checksumsOn(ctx, o.From.BinDir, o.DataDir)
	if err != nil {
		return err
	}
	staging := StagingDir(o.DataDir)
	if err := os.RemoveAll(staging); err != nil {
		return err
	}
	if err := os.MkdirAll(o.WorkDir, 0o700); err != nil {
		return err
	}
	fmt.Fprintf(o.log(), "creating a PostgreSQL %d database next to the PostgreSQL %d one\n", o.To.Major, o.From.Major)
	init := Options{BinDir: o.To.BinDir, User: o.User, Password: o.Password, Database: "postgres"}
	if err := initCluster(ctx, init, staging, checksumFlags(on, o.To.Major)...); err != nil {
		os.RemoveAll(staging)
		return err
	}
	args := []string{"-b", o.From.BinDir, "-B", o.To.BinDir, "-d", o.DataDir, "-D", staging, "-U", o.User,
		"--socketdir", o.WorkDir,
		"-o", "-c dynamic_shared_memory_type=mmap", "-O", "-c dynamic_shared_memory_type=mmap"}
	fmt.Fprintln(o.log(), "checking that pg_upgrade can move the database")
	if err := o.pgUpgrade(ctx, append(args, "--check")...); err != nil {
		return o.failed(staging, err)
	}
	fmt.Fprintln(o.log(), "moving the database with pg_upgrade --link")
	if err := o.pgUpgrade(ctx, append(args, "--link")...); err != nil {
		return o.failed(staging, err)
	}
	return Swap(o.DataDir, o.From.Major)
}

func (o UpgradeOptions) pgUpgrade(ctx context.Context, args ...string) error {
	cmd := exec.CommandContext(ctx, filepath.Join(o.To.BinDir, "pg_upgrade"), args...)
	cmd.Dir = o.WorkDir
	cmd.Env = append(os.Environ(), "PGPASSWORD="+o.Password)
	var out bytes.Buffer
	cmd.Stdout = io.MultiWriter(&out, o.log())
	cmd.Stderr = cmd.Stdout
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("pg_upgrade: %w: %s", err, lastLines(out.String(), 20))
	}
	return nil
}

func (o UpgradeOptions) failed(staging string, cause error) error {
	logs := filepath.Join(o.WorkDir, outputDir)
	os.RemoveAll(logs)
	kept := os.Rename(filepath.Join(staging, outputDir), logs) == nil
	var err error
	if Linked(o.DataDir) {
		err = fmt.Errorf("%w: %w", &BrokenError{DataDir: o.DataDir, Major: o.From.Major}, cause)
	} else {
		err = fmt.Errorf("postgres: the upgrade to PostgreSQL %d failed and the PostgreSQL %d database is unchanged: %w", o.To.Major, o.From.Major, errors.Join(cause, os.RemoveAll(staging)))
	}
	if kept {
		err = fmt.Errorf("%w. pg_upgrade's logs are in %s", err, logs)
	}
	return err
}

func Linked(dataDir string) bool {
	control := filepath.Join(dataDir, "global", "pg_control")
	if _, err := os.Stat(control); err == nil {
		return false
	}
	_, err := os.Stat(control + ".old")
	return err == nil
}

type BrokenError struct {
	DataDir string
	Major   int
}

func (e *BrokenError) Error() string {
	return fmt.Sprintf("postgres: pg_upgrade stopped after it began linking the PostgreSQL %d files in %s, so they can't start again. "+
		"Restore the backup from before the upgrade with sudo rexctl restore, or fluxerd restore without rexctl", e.Major, e.DataDir)
}

func checksumFlags(on bool, major int) []string {
	switch {
	case on:
		return []string{"--data-checksums"}
	case major >= checksumsOff:
		return []string{"--no-data-checksums"}
	}
	return nil
}

func checksumsOn(ctx context.Context, binDir, dataDir string) (bool, error) {
	out, err := exec.CommandContext(ctx, filepath.Join(binDir, "pg_controldata"), "-D", dataDir).Output()
	if err != nil {
		return false, fmt.Errorf("postgres: pg_controldata %s: %w", dataDir, err)
	}
	return parseChecksums(string(out))
}

func parseChecksums(controldata string) (bool, error) {
	for _, line := range strings.Split(controldata, "\n") {
		if v, ok := strings.CutPrefix(strings.TrimSpace(line), "Data page checksum version:"); ok {
			return strings.TrimSpace(v) != "0", nil
		}
	}
	return false, errors.New("postgres: pg_controldata doesn't say whether data checksums are on")
}

func StagingDir(dataDir string) string { return dataDir + upgradeSuffix }

func Swap(dataDir string, oldMajor int) error {
	if err := os.WriteFile(dataDir+markerSuffix, []byte(strconv.Itoa(oldMajor)+"\n"), 0o600); err == nil {
		return FinishUpgrade(dataDir)
	}
	if err := swap(dataDir, oldMajor); err != nil {
		return err
	}
	if err := os.Remove(dataDir + markerSuffix); err != nil && !errors.Is(err, fs.ErrNotExist) {
		return err
	}
	return nil
}

func FinishUpgrade(dataDir string) error {
	data, err := os.ReadFile(dataDir + markerSuffix)
	if errors.Is(err, fs.ErrNotExist) {
		return nil
	}
	if err != nil {
		return err
	}
	old, err := strconv.Atoi(strings.TrimSpace(string(data)))
	if err != nil {
		return fmt.Errorf("postgres: %s holds %q, not a PostgreSQL version", dataDir+markerSuffix, strings.TrimSpace(string(data)))
	}
	if err := swap(dataDir, old); err != nil {
		return err
	}
	return os.Remove(dataDir + markerSuffix)
}

func swap(dataDir string, old int) error {
	staging := StagingDir(dataDir)
	if _, err := os.Stat(staging); err != nil {
		return nil
	}
	if _, err := os.Stat(dataDir); err == nil {
		if err := os.Rename(dataDir, OldDir(dataDir, old)); err != nil {
			return err
		}
	}
	return os.Rename(staging, dataDir)
}

func OldDir(dataDir string, major int) string {
	return dataDir + "." + strconv.Itoa(major)
}

func OldDirs(dataDir string) map[int]string {
	matches, _ := filepath.Glob(dataDir + ".*")
	out := map[int]string{}
	for _, m := range matches {
		major, err := strconv.Atoi(strings.TrimPrefix(m, dataDir+"."))
		if err != nil {
			continue
		}
		if _, err := os.Stat(filepath.Join(m, "PG_VERSION")); err == nil {
			out[major] = m
		}
	}
	return out
}

func lastLines(s string, n int) string {
	lines := strings.Split(strings.TrimSpace(s), "\n")
	if len(lines) > n {
		lines = lines[len(lines)-n:]
	}
	return strings.Join(lines, "\n")
}
