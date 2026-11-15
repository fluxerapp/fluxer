// SPDX-License-Identifier: AGPL-3.0-or-later

package backup

import (
	"archive/tar"
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"syscall"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

const (
	postgresReadyTimeout = 3 * time.Minute
	postgresStopTimeout  = time.Minute
)

type DumpError struct {
	Output string
	Err    error
}

func (e *DumpError) Error() string {
	if e.Err != nil {
		return "backup: pg_dump failed: " + e.Err.Error() + ": " + e.Output
	}
	return "backup: pg_dump output has no PGDMP header"
}

func (e *DumpError) Unwrap() error { return e.Err }

type RestoreError struct {
	Part   string
	Output string
	Err    error
}

func (e *RestoreError) Error() string {
	if e.Output == "" {
		return fmt.Sprintf("backup: restoring %s: %v", e.Part, e.Err)
	}
	return fmt.Sprintf("backup: restoring %s: %v: %s", e.Part, e.Err, e.Output)
}

func (e *RestoreError) Unwrap() error { return e.Err }

func (t Target) pgTool(name string) string {
	if t.PGBinDir != "" {
		return filepath.Join(t.PGBinDir, name)
	}
	return name
}

func (t Target) pgUser() string     { return t.envOr("FLUXER_POSTGRES_USERNAME", "fluxer") }
func (t Target) pgDatabase() string { return t.envOr("FLUXER_POSTGRES_DATABASE", "fluxer") }

func (t Target) envOr(key, def string) string {
	if v := t.Env[key]; v != "" {
		return v
	}
	return def
}

func (t Target) pgCommand(ctx context.Context, sock, tool string, args ...string) *exec.Cmd {
	cmd := exec.CommandContext(ctx, t.pgTool(tool), append([]string{"-h", sock, "-p", strconv.Itoa(spec.PortPostgres), "-U", t.pgUser()}, args...)...)
	cmd.Env = append(os.Environ(), "PGPASSWORD="+t.Env["POSTGRES_PASSWORD"], "PGCONNECT_TIMEOUT=10")
	return cmd
}

func (t Target) addDump(ctx context.Context, w *archiveWriter, sock, spool string) error {
	f, err := os.OpenFile(spool, os.O_CREATE|os.O_TRUNC|os.O_RDWR, 0o600)
	if err != nil {
		return err
	}
	defer os.Remove(spool)
	defer f.Close()
	var stderr bytes.Buffer
	cmd := t.pgCommand(ctx, sock, "pg_dump", "-d", t.pgDatabase(), "--format=custom")
	cmd.Stdout, cmd.Stderr = f, &stderr
	if err := cmd.Run(); err != nil {
		return &DumpError{Output: strings.TrimSpace(stderr.String()), Err: err}
	}
	head := make([]byte, 5)
	if _, err := f.ReadAt(head, 0); err != nil || string(head) != "PGDMP" {
		return &DumpError{Output: strings.TrimSpace(stderr.String())}
	}
	fi, err := f.Stat()
	if err != nil {
		return err
	}
	if _, err := f.Seek(0, io.SeekStart); err != nil {
		return err
	}
	return w.add(&tar.Header{Name: DumpName, Mode: 0o600, Size: fi.Size(), ModTime: time.Now(), Typeflag: tar.TypeReg}, f)
}

func (t Target) restoreDump(ctx context.Context, sock string, r io.Reader) error {
	var stderr bytes.Buffer
	cmd := t.pgCommand(ctx, sock, "pg_restore", "-d", t.pgDatabase(), "--clean", "--if-exists", "--single-transaction", "--exit-on-error", "--no-owner", "--no-acl")
	cmd.Stdin, cmd.Stderr = r, &stderr
	if err := cmd.Run(); err != nil {
		return &RestoreError{Part: "database", Output: strings.TrimSpace(stderr.String()), Err: err}
	}
	return nil
}

type postgresServer struct {
	cmd    *exec.Cmd
	sock   string
	out    *bytes.Buffer
	exited chan error
}

func (t Target) startPostgres(ctx context.Context, sock string) (*postgresServer, error) {
	dir := t.Layout.PostgresDir()
	if _, err := os.Stat(filepath.Join(dir, "PG_VERSION")); err != nil {
		return nil, fmt.Errorf("backup: %s holds no database yet, start Fluxer once and stop it again first: %w", dir, err)
	}
	if err := os.MkdirAll(sock, 0o700); err != nil {
		return nil, err
	}
	out := &bytes.Buffer{}
	cmd := exec.Command(t.pgTool("postgres"), "-D", dir,
		"-c", "listen_addresses=",
		"-c", "port="+strconv.Itoa(spec.PortPostgres),
		"-c", "unix_socket_directories="+sock,
		"-c", "dynamic_shared_memory_type=mmap")
	cmd.Stdout, cmd.Stderr = out, out
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	if err := cmd.Start(); err != nil {
		return nil, err
	}
	s := &postgresServer{cmd: cmd, sock: sock, out: out, exited: make(chan error, 1)}
	go func() { s.exited <- cmd.Wait() }()
	ctx, cancel := context.WithTimeout(ctx, postgresReadyTimeout)
	defer cancel()
	for {
		ready := exec.CommandContext(ctx, t.pgTool("pg_isready"), "-q", "-h", sock, "-p", strconv.Itoa(spec.PortPostgres))
		if ready.Run() == nil {
			return s, nil
		}
		select {
		case err := <-s.exited:
			s.exited <- err
			return nil, &RestoreError{Part: "database", Output: strings.TrimSpace(out.String()), Err: fmt.Errorf("postgres exited: %v", err)}
		case <-ctx.Done():
			s.stop()
			return nil, &RestoreError{Part: "database", Output: strings.TrimSpace(out.String()), Err: errors.New("postgres did not get ready")}
		case <-time.After(250 * time.Millisecond):
		}
	}
}

func (s *postgresServer) stop() error {
	s.cmd.Process.Signal(syscall.SIGINT)
	select {
	case err := <-s.exited:
		return err
	case <-time.After(postgresStopTimeout):
		syscall.Kill(-s.cmd.Process.Pid, syscall.SIGKILL)
		return fmt.Errorf("backup: postgres did not stop within %s: %v", postgresStopTimeout, <-s.exited)
	}
}
