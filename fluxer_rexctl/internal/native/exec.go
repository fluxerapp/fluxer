// SPDX-License-Identifier: AGPL-3.0-or-later

package native

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"maps"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
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
	return EnvList(out), nil
}

func EnvList(env map[string]string) []string {
	out := make([]string, 0, len(env))
	for k, v := range env {
		out = append(out, k+"="+v)
	}
	return out
}

func (s *Stack) fluxerdCmd(ctx context.Context, set *stack.Set, args ...string) (*exec.Cmd, error) {
	env, err := s.fluxerdEnv(set)
	if err != nil {
		return nil, err
	}
	cmd := exec.CommandContext(ctx, "runuser", s.runuserArgs(args...)...)
	cmd.Env = env
	cmd.Dir = s.Root()
	cmd.WaitDelay = 10 * time.Second
	return cmd, nil
}

func (s *Stack) runuserArgs(args ...string) []string {
	argv := []string{"-u", ServiceUser, "--", s.Fluxerd(), args[0], "--config", s.EnvFile(), "--caddy-dir", s.Inst.Path(instance.CaddyDropInDir), "--certs-dir", s.Inst.Path(instance.CertsDir)}
	return append(argv, args[1:]...)
}

func ExecArgs(service string, cmd ...string) []string {
	return append([]string{"exec", service, "--"}, cmd...)
}

func (s *Stack) Exec(ctx context.Context, set *stack.Set, service string, opts engine.ExecOptions, cmd ...string) error {
	c, err := s.fluxerdCmd(ctx, set, ExecArgs(service, cmd...)...)
	if err != nil {
		return err
	}
	c.Env = append(c.Env, opts.Env...)
	c.Stdin, c.Stdout, c.Stderr = opts.Stdin, opts.Stdout, opts.Stderr
	return c.Run()
}

func (s *Stack) run(ctx context.Context, set *stack.Set, log io.Writer, args ...string) (string, error) {
	c, err := s.fluxerdCmd(ctx, set, args...)
	if err != nil {
		return "", err
	}
	var out, stderr bytes.Buffer
	c.Stdout, c.Stderr = io.MultiWriter(&out, log), io.MultiWriter(&stderr, log)
	if err := c.Run(); err != nil {
		if strings.Contains(stderr.String(), "flag provided but not defined") {
			err = errors.Join(errUnknownFlag, err)
		}
		return "", fmt.Errorf("native: fluxerd %s: %w", args[0], err)
	}
	return strings.TrimSpace(out.String()), nil
}

var errUnknownFlag = errors.New("fluxerd doesn't know a flag rexctl passed")

func (s *Stack) spoolDir() string { return filepath.Join(s.Root(), BackupsDirName) }

func (s *Stack) Backup(ctx context.Context, set *stack.Set, dir, reason string, log io.Writer) (string, error) {
	if err := os.MkdirAll(s.spoolDir(), 0o700); err != nil {
		return "", err
	}
	if err := s.chownData(s.spoolDir()); err != nil {
		return "", err
	}
	include, err := s.stageInstanceFiles()
	defer os.RemoveAll(include)
	if err != nil {
		return "", err
	}
	args := []string{"backup", "--dir", s.spoolDir(), "--reason", reason}
	out, err := s.run(ctx, set, log, append(args, "--include", include)...)
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
	defer os.Remove(spooled)
	dest := filepath.Join(dir, filepath.Base(spooled))
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return "", err
	}
	if err := os.Rename(spooled, dest); err != nil {
		if err := copyFile(spooled, dest); err != nil {
			return "", err
		}
	}
	if os.Geteuid() == 0 {
		if err := os.Chown(dest, 0, 0); err != nil {
			return "", err
		}
	}
	return dest, os.Chmod(dest, 0o600)
}

func (s *Stack) Restore(ctx context.Context, set *stack.Set, archive string, log io.Writer) error {
	if s.OwnsUnit(ctx) {
		if err := s.Stop(ctx); err != nil {
			return err
		}
	}
	if err := os.MkdirAll(s.spoolDir(), 0o700); err != nil {
		return err
	}
	spooled := filepath.Join(s.spoolDir(), "restore-"+filepath.Base(archive))
	defer os.Remove(spooled)
	if err := copyFile(archive, spooled); err != nil {
		return err
	}
	if err := s.chownData(s.spoolDir(), spooled); err != nil {
		return err
	}
	_, err := s.run(ctx, set, log, "restore", spooled)
	return err
}

const instanceFilesDir = ".instance-files"

var InstanceFiles = []string{instance.StateDirName + "/" + instance.StateFileName, instance.CertsDir, instance.CADir, instance.CaddyDropInDir}

func (s *Stack) stageInstanceFiles() (string, error) {
	dir := filepath.Join(s.spoolDir(), instanceFilesDir)
	if err := os.RemoveAll(dir); err != nil {
		return dir, err
	}
	if err := os.Mkdir(dir, 0o700); err != nil {
		return dir, err
	}
	for _, name := range InstanceFiles {
		if err := copyTree(s.Inst.Path(filepath.FromSlash(name)), filepath.Join(dir, filepath.FromSlash(name))); err != nil {
			return dir, err
		}
	}
	if os.Geteuid() != 0 {
		return dir, nil
	}
	uid, gid, err := serviceIDs()
	if err != nil {
		return dir, err
	}
	return dir, filepath.WalkDir(dir, func(p string, _ fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		return os.Lchown(p, uid, gid)
	})
}

func copyTree(src, dst string) error {
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
		target := filepath.Join(dst, rel)
		info, err := d.Info()
		if err != nil {
			return err
		}
		switch {
		case d.IsDir():
			if err := os.MkdirAll(target, 0o700); err != nil {
				return err
			}
		case d.Type().IsRegular():
			if err := os.MkdirAll(filepath.Dir(target), 0o700); err != nil {
				return err
			}
			if err := copyFile(p, target); err != nil {
				return err
			}
		default:
			return nil
		}
		return os.Chmod(target, info.Mode().Perm())
	})
}

func copyFile(src, dst string) error {
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()
	out, err := os.OpenFile(dst, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o600)
	if err != nil {
		return err
	}
	if _, err := io.Copy(out, in); err != nil {
		out.Close()
		return err
	}
	return out.Close()
}
