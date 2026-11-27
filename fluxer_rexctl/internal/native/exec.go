// SPDX-License-Identifier: AGPL-3.0-or-later

package native

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"maps"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const hostPath = "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

func (s *Stack) fluxerdEnv(set *stack.Set) ([]string, error) {
	env, _, err := s.Environment(set)
	if err != nil {
		return nil, err
	}
	out := map[string]string{}
	maps.Copy(out, env)
	out["PATH"] = hostPath
	out["HOME"] = s.Root()
	out[dataDirEnv] = s.Root()
	out[runtimeDirEnv] = s.RuntimeDir()
	if lang := os.Getenv("LANG"); lang != "" {
		out["LANG"] = lang
	}
	return envList(out), nil
}

func envList(env map[string]string) []string {
	out := make([]string, 0, len(env))
	for k, v := range env {
		out = append(out, k+"="+v)
	}
	return out
}

func (s *Stack) fluxerdCmd(ctx context.Context, set *stack.Set, group bool, args ...string) (*exec.Cmd, error) {
	env, err := s.fluxerdEnv(set)
	if err != nil {
		return nil, err
	}
	cmd := exec.CommandContext(ctx, s.Fluxerd(), s.fluxerdArgs(args...)...)
	cmd.Env = env
	cmd.Dir = s.Root()
	cmd.WaitDelay = cancelGrace
	if err := asService(cmd, group); err != nil {
		return nil, fmt.Errorf("native: the account %s: %w", ServiceUser, err)
	}
	return cmd, nil
}

const cancelGrace = 30 * time.Second

func (s *Stack) fluxerdArgs(args ...string) []string {
	argv := []string{args[0], "--config", s.EnvFile(), "--caddy-dir", s.Inst.Path(instance.CaddyDropInDir), "--certs-dir", s.Inst.Path(instance.CertsDir)}
	return append(argv, args[1:]...)
}

func execArgs(service string, cmd ...string) []string {
	return append([]string{"exec", service, "--"}, cmd...)
}

func (s *Stack) Exec(ctx context.Context, set *stack.Set, service string, opts engine.ExecOptions, cmd ...string) error {
	c, err := s.fluxerdCmd(ctx, set, false, execArgs(service, cmd...)...)
	if err != nil {
		return err
	}
	c.Env = append(c.Env, opts.Env...)
	c.Stdin, c.Stdout, c.Stderr = opts.Stdin, opts.Stdout, opts.Stderr
	return c.Run()
}

func (s *Stack) run(ctx context.Context, set *stack.Set, log io.Writer, args ...string) (string, error) {
	c, err := s.fluxerdCmd(ctx, set, true, args...)
	if err != nil {
		return "", err
	}
	var out, stderr bytes.Buffer
	c.Stdout, c.Stderr = io.MultiWriter(&out, log), io.MultiWriter(&stderr, log)
	if err := c.Run(); err != nil {
		if strings.Contains(stderr.String(), "flag provided but not defined") {
			err = errors.Join(errUnknownFlag, err)
		}
		space := &SpaceError{What: "make the backup"}
		var exit *exec.ExitError
		if errors.As(err, &exit) && exit.ExitCode() == fluxerdNoSpace && json.Unmarshal(out.Bytes(), space) == nil && space.Dir != "" {
			err = errors.Join(space, err)
		}
		return "", fmt.Errorf("native: fluxerd %s: %w", args[0], err)
	}
	return strings.TrimSpace(out.String()), nil
}

var errUnknownFlag = errors.New("fluxerd doesn't know a flag rexctl passed")

func (s *Stack) runNewer(ctx context.Context, set *stack.Set, log io.Writer, args ...string) (string, error) {
	var buf bytes.Buffer
	out, err := s.run(ctx, set, &buf, args...)
	if !errors.Is(err, errUnknownFlag) {
		_, _ = log.Write(buf.Bytes())
	}
	return out, err
}

const fluxerdNoSpace = 4

func (s *Stack) spoolDir() string { return filepath.Join(s.Root(), backupsDirName) }

var errSpoolNotDir = errors.New("native: Fluxer's backup folder isn't a plain folder")

func (s *Stack) openSpool(create bool) (*os.Root, error) {
	data, err := os.OpenRoot(s.Root())
	if err != nil {
		return nil, err
	}
	defer data.Close()
	if create {
		if err := data.Mkdir(backupsDirName, 0o700); err != nil && !errors.Is(err, fs.ErrExist) {
			return nil, err
		}
	}
	fi, err := data.Lstat(backupsDirName)
	if err != nil {
		return nil, err
	}
	if !fi.IsDir() {
		return nil, fmt.Errorf("%w: %s", errSpoolNotDir, s.spoolDir())
	}
	return data.OpenRoot(backupsDirName)
}

func (s *Stack) Backup(ctx context.Context, set *stack.Set, dir, reason string, log io.Writer) (string, error) {
	spool, err := s.openSpool(true)
	if err != nil {
		return "", err
	}
	defer spool.Close()
	if err := s.chownData(s.spoolDir()); err != nil {
		return "", err
	}
	include, err := s.stageInstanceFiles(spool)
	defer spool.RemoveAll(instanceFilesDir)
	if err != nil {
		return "", err
	}
	args := []string{"backup", "--dir", s.spoolDir(), "--reason", reason}
	out, err := s.runNewer(ctx, set, log, append(args, "--include", include)...)
	if errors.Is(err, errUnknownFlag) {
		fmt.Fprintln(log, "This fluxerd is too old to put rexctl's own files in the backup. The backup holds Fluxer's data and settings, and restores into this instance.")
		out, err = s.run(ctx, set, log, args...)
	}
	if err != nil {
		return "", err
	}
	lines := strings.Split(out, "\n")
	spooled := filepath.Clean(lines[len(lines)-1])
	if filepath.Dir(spooled) != s.spoolDir() {
		return "", fmt.Errorf("native: fluxerd backup wrote %q, outside %s", spooled, s.spoolDir())
	}
	name := filepath.Base(spooled)
	defer spool.Remove(name)
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return "", err
	}
	dest := filepath.Join(dir, name)
	if err := copyOut(spool, name, dest); err != nil {
		return "", err
	}
	return dest, nil
}

func (s *Stack) FetchOwnBackups(dest string) ([]string, error) {
	spool, err := s.openSpool(false)
	if errors.Is(err, fs.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}
	defer spool.Close()
	ents, err := fs.ReadDir(spool.FS(), ".")
	if err != nil {
		return nil, err
	}
	var got []string
	for _, e := range ents {
		name := e.Name()
		if !e.Type().IsRegular() || strings.HasPrefix(name, ".") || strings.HasPrefix(name, "restore-") || !strings.HasSuffix(name, ".tar.zst") {
			continue
		}
		target := filepath.Join(dest, name)
		info, err := e.Info()
		if err != nil {
			return got, err
		}
		if fi, err := os.Lstat(target); err == nil && fi.Mode().IsRegular() && fi.Size() == info.Size() {
			continue
		}
		if err := os.MkdirAll(dest, 0o700); err != nil {
			return got, err
		}
		if err := copyOut(spool, name, target); err != nil {
			if errors.Is(err, errNotRegular) {
				continue
			}
			return got, err
		}
		got = append(got, target)
	}
	return got, nil
}

var errNotRegular = errors.New("native: not a regular file")

func copyOut(spool *os.Root, name, dst string) error {
	in, err := spool.Open(name)
	if err != nil {
		return err
	}
	defer in.Close()
	fi, err := in.Stat()
	if err != nil {
		return err
	}
	if !fi.Mode().IsRegular() {
		return fmt.Errorf("%w: %s", errNotRegular, name)
	}
	tmp := dst + partialSuffix
	if err := os.Remove(tmp); err != nil && !errors.Is(err, fs.ErrNotExist) {
		return err
	}
	out, err := os.OpenFile(tmp, os.O_CREATE|os.O_EXCL|os.O_WRONLY, 0o600)
	if err != nil {
		return err
	}
	if err := fillFile(out, in, 0, 0); err != nil {
		os.Remove(tmp)
		return err
	}
	if err := os.Remove(dst); err != nil && !errors.Is(err, fs.ErrNotExist) {
		os.Remove(tmp)
		return err
	}
	return os.Rename(tmp, dst)
}

func fillFile(out *os.File, in io.Reader, uid, gid int) error {
	if _, err := io.Copy(out, in); err != nil {
		out.Close()
		return err
	}
	if os.Geteuid() == 0 {
		if err := out.Chown(uid, gid); err != nil {
			out.Close()
			return err
		}
	}
	if err := out.Chmod(0o600); err != nil {
		out.Close()
		return err
	}
	if err := out.Sync(); err != nil {
		out.Close()
		return err
	}
	return out.Close()
}

type SpaceError struct {
	Dir  string `json:"dir"`
	Need uint64 `json:"need"`
	Free uint64 `json:"free"`
	What string `json:"-"`
}

func (e *SpaceError) Error() string {
	return fmt.Sprintf("native: %s has %d bytes free and needs %d to %s", e.Dir, e.Free, e.Need, e.What)
}

func (e *SpaceError) Short() int64 {
	if e.Need <= e.Free {
		return 0
	}
	return int64(e.Need - e.Free)
}

func (s *Stack) Restore(ctx context.Context, set *stack.Set, archive string, log io.Writer) error {
	spool, err := s.openSpool(true)
	if err != nil {
		return err
	}
	defer spool.Close()
	in, err := os.Open(archive)
	if err != nil {
		return err
	}
	defer in.Close()
	fi, err := in.Stat()
	if err != nil {
		return err
	}
	if free, err := host.FreeDisk(s.spoolDir()); err == nil && free < uint64(fi.Size()) {
		return &SpaceError{Dir: s.spoolDir(), Need: uint64(fi.Size()), Free: free, What: "stage the backup"}
	}
	name := "restore-" + filepath.Base(archive)
	if err := s.spoolIn(spool, name, in); err != nil {
		return err
	}
	defer spool.Remove(name)
	if err := s.chownData(s.spoolDir()); err != nil {
		return err
	}
	spooled := filepath.Join(s.spoolDir(), name)
	var said bytes.Buffer
	if err := s.CheckRestore(ctx, set, spooled, io.MultiWriter(log, &said)); err != nil {
		return &RestoreRefusedError{Reason: refusalReason(said.String()), Err: err}
	}
	if s.OwnsUnit(ctx) {
		if err := s.Halt(ctx); err != nil {
			return err
		}
	}
	_, err = s.run(ctx, set, log, "restore", spooled)
	return err
}

func (s *Stack) spoolIn(spool *os.Root, name string, in io.Reader) error {
	uid, gid := 0, 0
	if os.Geteuid() == 0 {
		var err error
		if uid, gid, err = serviceIDs(); err != nil {
			return fmt.Errorf("native: the account %s: %w", ServiceUser, err)
		}
	}
	if err := spool.Remove(name); err != nil && !errors.Is(err, fs.ErrNotExist) {
		return err
	}
	out, err := spool.OpenFile(name, os.O_CREATE|os.O_EXCL|os.O_WRONLY, 0o600)
	if err != nil {
		return err
	}
	if err := fillFile(out, in, uid, gid); err != nil {
		spool.Remove(name)
		return err
	}
	return nil
}

type RestoreRefusedError struct {
	Reason string
	Err    error
}

func (e *RestoreRefusedError) Error() string { return e.Err.Error() }

func (e *RestoreRefusedError) Unwrap() error { return e.Err }

func refusalReason(out string) string {
	lines := strings.Split(strings.TrimSpace(out), "\n")
	last := strings.TrimSpace(lines[len(lines)-1])
	for _, prefix := range []string{"fluxerd restore: ", "backup: ", "restore: "} {
		last = strings.TrimPrefix(last, prefix)
	}
	if last == "" {
		return ""
	}
	return strings.ToUpper(last[:1]) + last[1:] + "."
}

func (s *Stack) CheckRestore(ctx context.Context, set *stack.Set, archive string, log io.Writer) error {
	_, err := s.runNewer(ctx, set, log, "restore", "--check", archive)
	if errors.Is(err, errUnknownFlag) {
		return nil
	}
	return err
}

const instanceFilesDir = ".instance-files"

var InstanceFiles = []string{instance.StateDirName + "/" + instance.StateFileName, instance.CertsDir, instance.GeoIPDir, instance.CADir, instance.CaddyDropInDir}

func (s *Stack) stageInstanceFiles(spool *os.Root) (string, error) {
	dir := filepath.Join(s.spoolDir(), instanceFilesDir)
	if err := spool.RemoveAll(instanceFilesDir); err != nil {
		return dir, err
	}
	if err := spool.Mkdir(instanceFilesDir, 0o700); err != nil {
		return dir, err
	}
	staged, err := spool.OpenRoot(instanceFilesDir)
	if err != nil {
		return dir, err
	}
	defer staged.Close()
	uid, gid := -1, -1
	if os.Geteuid() == 0 {
		if uid, gid, err = serviceIDs(); err != nil {
			return dir, err
		}
		if err := staged.Lchown(".", uid, gid); err != nil {
			return dir, err
		}
	}
	for _, name := range InstanceFiles {
		if err := copyTree(s.Inst.Path(filepath.FromSlash(name)), staged, filepath.FromSlash(name), uid, gid); err != nil {
			return dir, err
		}
	}
	return dir, nil
}

func copyTree(src string, dst *os.Root, prefix string, uid, gid int) error {
	return filepath.WalkDir(src, func(p string, d fs.DirEntry, err error) error {
		if errors.Is(err, fs.ErrNotExist) && p == src {
			return nil
		}
		if err != nil {
			return err
		}
		rel, err := filepath.Rel(src, p)
		if err != nil {
			return err
		}
		target := filepath.Join(prefix, rel)
		info, err := d.Info()
		if err != nil {
			return err
		}
		switch {
		case d.IsDir():
			if err := mkdirOwned(dst, target, uid, gid); err != nil {
				return err
			}
			return dst.Chmod(target, info.Mode().Perm())
		case d.Type().IsRegular():
			if err := mkdirOwned(dst, filepath.Dir(target), uid, gid); err != nil {
				return err
			}
			in, err := os.Open(p)
			if err != nil {
				return err
			}
			defer in.Close()
			out, err := dst.OpenFile(target, os.O_CREATE|os.O_EXCL|os.O_WRONLY, 0o600)
			if err != nil {
				return err
			}
			if err := fillFile(out, in, uid, gid); err != nil {
				return err
			}
			return dst.Chmod(target, info.Mode().Perm())
		default:
			return nil
		}
	})
}

func mkdirOwned(r *os.Root, dir string, uid, gid int) error {
	if dir == "." {
		return nil
	}
	if err := mkdirOwned(r, filepath.Dir(dir), uid, gid); err != nil {
		return err
	}
	if err := r.Mkdir(dir, 0o700); err != nil {
		if errors.Is(err, fs.ErrExist) {
			return nil
		}
		return err
	}
	if uid < 0 {
		return nil
	}
	return r.Lchown(dir, uid, gid)
}

const partialSuffix = ".partial"

func (s *Stack) UpgradePostgres(ctx context.Context, set *stack.Set, to int, log io.Writer) (int, error) {
	args := []string{"postgres-upgrade"}
	if to > 0 {
		args = append(args, "--to", strconv.Itoa(to))
	}
	out, err := s.run(ctx, set, log, args...)
	if err != nil {
		return 0, err
	}
	lines := strings.Split(out, "\n")
	return strconv.Atoi(strings.TrimSpace(lines[len(lines)-1]))
}

func (s *Stack) CleanupPostgres(ctx context.Context, set *stack.Set, log io.Writer) ([]int, error) {
	out, err := s.run(ctx, set, log, "postgres-upgrade", "--cleanup")
	if err != nil {
		return nil, err
	}
	var majors []int
	for _, line := range strings.Fields(out) {
		if m, err := strconv.Atoi(line); err == nil {
			majors = append(majors, m)
		}
	}
	return majors, nil
}
