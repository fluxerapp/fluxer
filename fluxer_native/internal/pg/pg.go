// SPDX-License-Identifier: AGPL-3.0-or-later

package pg

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"os/exec"
	"os/user"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

const (
	shmPath     = "/dev/shm"
	minPosixShm = 256 << 20
)

type Options struct {
	BinDir    string
	Major     int
	Newer     int
	Resolve   error
	DataDir   string
	SocketDir string
	Port      int
	User      string
	Password  string
	Database  string
	Settings  []string
}

func (o Options) bin(name string) string { return filepath.Join(o.BinDir, name) }

func Prepare(ctx context.Context, o Options) error {
	if err := FinishUpgrade(o.DataDir); err != nil {
		return err
	}
	_, err := os.Stat(filepath.Join(o.DataDir, "PG_VERSION"))
	if err == nil {
		if err := clearStaleLocks(o); err != nil {
			return err
		}
		return os.Chmod(o.DataDir, 0o700)
	}
	if !errors.Is(err, fs.ErrNotExist) {
		return err
	}
	if entries, err := os.ReadDir(o.DataDir); err == nil && len(entries) > 0 {
		return fmt.Errorf("postgres: %s has files but no PG_VERSION, so it is not a Postgres data directory. Move it away and start again", o.DataDir)
	}
	if _, err := user.LookupId(strconv.Itoa(os.Getuid())); err != nil {
		return fmt.Errorf("postgres: initdb needs a passwd entry for uid %d. Run fluxerd as uid 65532 (the image user) or add a user for this uid", os.Getuid())
	}
	staging := o.DataDir + ".init"
	if err := os.RemoveAll(staging); err != nil {
		return err
	}
	if err := initCluster(ctx, o, staging, "--data-checksums"); err != nil {
		return err
	}
	if err := os.Remove(o.DataDir); err != nil && !errors.Is(err, fs.ErrNotExist) {
		return err
	}
	return os.Rename(staging, o.DataDir)
}

func clearStaleLocks(o Options) error {
	paths := []string{filepath.Join(o.DataDir, "postmaster.pid")}
	if o.SocketDir != "" {
		locks, err := filepath.Glob(filepath.Join(o.SocketDir, ".s.PGSQL.*.lock"))
		if err != nil {
			return err
		}
		paths = append(paths, locks...)
	}
	for _, path := range paths {
		if err := clearStaleLock(path); err != nil {
			return err
		}
	}
	return nil
}

func clearStaleLock(path string) error {
	data, err := os.ReadFile(path)
	if errors.Is(err, fs.ErrNotExist) {
		return nil
	}
	if err != nil {
		return err
	}
	line, _, _ := strings.Cut(string(data), "\n")
	pid, err := strconv.Atoi(strings.TrimSpace(line))
	if err != nil || pid <= 0 {
		return nil
	}
	if _, err := os.Stat("/proc/self/comm"); err != nil {
		return nil
	}
	comm, err := os.ReadFile(filepath.Join("/proc", strconv.Itoa(pid), "comm"))
	if err == nil && strings.TrimSpace(string(comm)) == "postgres" {
		return nil
	}
	if err != nil && !errors.Is(err, fs.ErrNotExist) {
		return nil
	}
	return os.Remove(path)
}

func initCluster(ctx context.Context, o Options, dir string, extra ...string) error {
	pwfile, err := os.CreateTemp("", "fluxer-pg-pw-")
	if err != nil {
		return err
	}
	defer os.Remove(pwfile.Name())
	if _, err := pwfile.WriteString(o.Password + "\n"); err != nil {
		pwfile.Close()
		return err
	}
	if err := pwfile.Close(); err != nil {
		return err
	}
	argv := append([]string{o.bin("initdb"), "-D", dir, "-U", o.User,
		"--auth-host=scram-sha-256", "--auth-local=scram-sha-256",
		"--locale=C.UTF-8", "-E", "UTF8", "--pwfile=" + pwfile.Name()}, extra...)
	if err := run(ctx, nil, argv...); err != nil {
		return err
	}
	if o.Database == "postgres" {
		return nil
	}
	create := fmt.Sprintf("CREATE DATABASE %s;\n", quoteIdent(o.Database))
	return run(ctx, strings.NewReader(create), o.bin("postgres"), "--single", "-D", dir, "-c", "listen_addresses=", "postgres")
}

func Argv(o Options) []string {
	argv := []string{o.bin("postgres"), "-D", o.DataDir,
		"-c", "listen_addresses=" + spec.Loopback,
		"-c", "port=" + strconv.Itoa(o.Port),
		"-c", "unix_socket_directories=" + o.SocketDir,
		"-c", "dynamic_shared_memory_type=" + DSMType(shmPath),
		"-c", "max_logical_replication_workers=0",
	}
	for _, s := range o.Settings {
		argv = append(argv, "-c", s)
	}
	return argv
}

func ReadyProbe(o Options) spec.Probe {
	return spec.ExecProbe(o.bin("pg_isready"), "-q", "-h", o.SocketDir, "-p", strconv.Itoa(o.Port), "-U", o.User, "-d", o.Database)
}

func DSMType(path string) string {
	if size, ok := fsSize(path); ok && size >= minPosixShm {
		return "posix"
	}
	return "mmap"
}

func HasLibrary(binDir, name string) bool {
	dir := filepath.Join(binDir, "..", "lib")
	if out, err := exec.Command(filepath.Join(binDir, "pg_config"), "--pkglibdir").Output(); err == nil {
		dir = strings.TrimSpace(string(out))
	}
	for _, ext := range []string{".so", ".dylib"} {
		if _, err := os.Stat(filepath.Join(dir, name+ext)); err == nil {
			return true
		}
	}
	return false
}

func quoteIdent(s string) string {
	return `"` + strings.ReplaceAll(s, `"`, `""`) + `"`
}

func run(ctx context.Context, stdin *strings.Reader, argv ...string) error {
	cmd := exec.CommandContext(ctx, argv[0], argv[1:]...)
	if stdin != nil {
		cmd.Stdin = stdin
	}
	var out bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &out
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("%s: %w: %s", filepath.Base(argv[0]), err, bytes.TrimSpace(out.Bytes()))
	}
	return nil
}
