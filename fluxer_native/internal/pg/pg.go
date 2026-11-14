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

	"golang.org/x/sys/unix"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

const Major = 17

const (
	shmPath     = "/dev/shm"
	minPosixShm = 256 << 20
)

var DefaultBinDir = fmt.Sprintf("/usr/lib/postgresql/%d/bin", Major)

type Options struct {
	BinDir    string
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
	version, err := os.ReadFile(filepath.Join(o.DataDir, "PG_VERSION"))
	if err == nil {
		return checkVersion(strings.TrimSpace(string(version)), o.DataDir)
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
	if err := initCluster(ctx, o, staging); err != nil {
		return err
	}
	if err := os.Remove(o.DataDir); err != nil && !errors.Is(err, fs.ErrNotExist) {
		return err
	}
	return os.Rename(staging, o.DataDir)
}

func initCluster(ctx context.Context, o Options, dir string) error {
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
	if err := run(ctx, nil, o.bin("initdb"), "-D", dir, "-U", o.User,
		"--auth-host=scram-sha-256", "--auth-local=scram-sha-256",
		"--locale=C.UTF-8", "-E", "UTF8", "--pwfile="+pwfile.Name()); err != nil {
		return err
	}
	if o.Database == "postgres" {
		return nil
	}
	create := fmt.Sprintf("CREATE DATABASE %s;\n", quoteIdent(o.Database))
	return run(ctx, strings.NewReader(create), o.bin("postgres"), "--single", "-D", dir, "-c", "listen_addresses=", "postgres")
}

func checkVersion(version, dataDir string) error {
	if version == strconv.Itoa(Major) {
		return nil
	}
	return fmt.Errorf("postgres: %s holds a Postgres %s cluster, but this release runs Postgres %d. "+
		"Start the old release, run fluxerd backup, move %s away, start this release and run fluxerd restore with that backup",
		dataDir, version, Major, dataDir)
}

func Argv(o Options) []string {
	argv := []string{o.bin("postgres"), "-D", o.DataDir,
		"-c", "listen_addresses=" + spec.Loopback,
		"-c", "port=" + strconv.Itoa(o.Port),
		"-c", "unix_socket_directories=" + o.SocketDir,
		"-c", "dynamic_shared_memory_type=" + DSMType(shmPath),
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
	var st unix.Statfs_t
	if err := unix.Statfs(path, &st); err != nil {
		return "mmap"
	}
	if uint64(st.Bsize)*uint64(st.Blocks) >= minPosixShm {
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
