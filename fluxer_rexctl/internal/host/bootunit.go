// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"bytes"
	"context"
	"crypto/sha256"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
)

const LibexecBinary = "/usr/local/libexec/rexctl/rexctl"

const userWaitNetwork = "podman-user-wait-network-online.service"

type BootUnit struct {
	Project   string
	Dir       string
	Binary    string
	Podman    bool
	Rootless  bool
	StopGrace time.Duration
}

type BootUnitState struct {
	Supported bool
	Path      string
	Missing   bool
	Stale     bool
	CopyStale bool
	Disabled  bool
	NoLinger  bool
}

func (s BootUnitState) Needed() bool {
	return s.Supported && (s.Missing || s.Stale || s.CopyStale || s.Disabled || s.NoLinger)
}

func BootUnitSupported() bool {
	if runtime.GOOS != "linux" || !has("systemctl") {
		return false
	}
	_, err := os.Stat("/run/systemd/system")
	return err == nil
}

func (u BootUnit) Name() string { return u.Project + ".service" }

func (u BootUnit) Path() (string, error) {
	dir, err := unitDir(u.Rootless)
	if err != nil {
		return "", err
	}
	return filepath.Join(dir, u.Name()), nil
}

func unitDir(rootless bool) (string, error) {
	if !rootless {
		return "/etc/systemd/system", nil
	}
	dir := os.Getenv("XDG_CONFIG_HOME")
	if dir == "" {
		home, err := os.UserHomeDir()
		if err != nil {
			return "", err
		}
		dir = filepath.Join(home, ".config")
	}
	return filepath.Join(dir, "systemd", "user"), nil
}

func (u BootUnit) execPath() string {
	if u.Rootless {
		return u.Binary
	}
	return LibexecBinary
}

func BootStopTimeout(grace time.Duration) time.Duration {
	t := grace + time.Minute
	if t < 3*time.Minute {
		t = 3 * time.Minute
	}
	return t
}

func (u BootUnit) render(waitNetwork bool, runAs string) string {
	var after, wants, requires []string
	var target string
	if u.Rootless {
		target = "default.target"
		if u.Podman {
			after = append(after, "podman.socket", "podman-restart.service")
			requires = append(requires, "podman.socket")
		} else {
			after = append(after, "docker.service")
		}
		if waitNetwork {
			after = append(after, userWaitNetwork)
			wants = append(wants, userWaitNetwork)
		}
	} else {
		target = "multi-user.target"
		after = append(after, "network-online.target")
		wants = append(wants, "network-online.target")
		if u.Podman {
			after = append(after, "podman.socket", "podman-restart.service")
			requires = append(requires, "podman.socket")
		} else {
			after = append(after, "docker.service")
			requires = append(requires, "docker.service")
		}
	}
	bin := systemdQuote(u.execPath())
	dir := systemdQuote(u.Dir)
	var b strings.Builder
	b.WriteString("[Unit]\n")
	fmt.Fprintf(&b, "Description=Fluxer in %s\n", strings.ReplaceAll(u.Dir, "%", "%%"))
	fmt.Fprintf(&b, "After=%s\n", strings.Join(after, " "))
	if len(wants) > 0 {
		fmt.Fprintf(&b, "Wants=%s\n", strings.Join(wants, " "))
	}
	if len(requires) > 0 {
		fmt.Fprintf(&b, "Requires=%s\n", strings.Join(requires, " "))
	}
	b.WriteString("\n[Service]\n")
	b.WriteString("Type=oneshot\n")
	b.WriteString("RemainAfterExit=yes\n")
	if runAs != "" {
		fmt.Fprintf(&b, "User=%s\n", runAs)
	}
	fmt.Fprintf(&b, "ExecStart=%s start --boot --dir %s\n", bin, dir)
	fmt.Fprintf(&b, "ExecStop=%s stop --boot --dir %s\n", bin, dir)
	b.WriteString("TimeoutStartSec=15min\n")
	fmt.Fprintf(&b, "TimeoutStopSec=%d\n", int(BootStopTimeout(u.StopGrace).Seconds()))
	b.WriteString("\n[Install]\n")
	fmt.Fprintf(&b, "WantedBy=%s\n", target)
	return b.String()
}

func systemdQuote(s string) string {
	s = strings.ReplaceAll(s, "%", "%%")
	s = strings.ReplaceAll(s, "$", "$$")
	if !strings.ContainsAny(s, " \t\"'\\;") {
		return s
	}
	s = strings.ReplaceAll(s, `\`, `\\`)
	s = strings.ReplaceAll(s, `"`, `\"`)
	return `"` + s + `"`
}

func (u BootUnit) content(ctx context.Context) string {
	runAs := ""
	if !u.Rootless && !u.Podman {
		runAs = dirOwner(u.Dir)
	}
	return u.render(u.Rootless && unitExists(ctx, true, userWaitNetwork), runAs)
}

func unitExists(ctx context.Context, rootless bool, name string) bool {
	args := []string{"list-unit-files", "--no-legend", name}
	if rootless {
		args = append([]string{"--user"}, args...)
	}
	out, err := exec.CommandContext(ctx, "systemctl", args...).Output()
	return err == nil && strings.Contains(string(out), name)
}

func unitIs(ctx context.Context, rootless bool, name, state string) bool {
	args := []string{"is-" + state, name}
	if rootless {
		args = append([]string{"--user"}, args...)
	}
	out, _ := exec.CommandContext(ctx, "systemctl", args...).Output()
	return strings.TrimSpace(string(out)) == state
}

func CheckBootUnit(ctx context.Context, u BootUnit) (BootUnitState, error) {
	if !BootUnitSupported() {
		return BootUnitState{}, nil
	}
	path, err := u.Path()
	if err != nil {
		return BootUnitState{}, err
	}
	s := BootUnitState{Supported: true, Path: path}
	data, err := os.ReadFile(path)
	switch {
	case os.IsNotExist(err):
		s.Missing = true
	case err != nil:
		return s, err
	default:
		s.Stale = string(data) != u.content(ctx)
		s.Disabled = !unitIs(ctx, u.Rootless, u.Name(), "enabled")
	}
	if !u.Rootless {
		s.CopyStale = !sameFile(u.Binary, LibexecBinary)
	}
	if u.Rootless {
		s.NoLinger = !lingering(ctx)
	}
	return s, nil
}

func InstallBootUnit(ctx context.Context, u BootUnit) error {
	if !BootUnitSupported() {
		return nil
	}
	if err := writeBootUnit(ctx, u); err != nil {
		return err
	}
	if err := systemctlRun(ctx, u.Rootless, "enable", u.Name()); err != nil {
		return err
	}
	if u.Rootless && !lingering(ctx) {
		if err := enableLinger(ctx); err != nil {
			return err
		}
	}
	if !unitIs(ctx, u.Rootless, u.Name(), "active") {
		return systemctlRun(ctx, u.Rootless, "start", "--no-block", u.Name())
	}
	return nil
}

func RefreshBootUnit(ctx context.Context, u BootUnit) error {
	if !BootUnitSupported() {
		return nil
	}
	path, err := u.Path()
	if err != nil {
		return err
	}
	if _, err := os.Stat(path); os.IsNotExist(err) {
		return nil
	}
	return writeBootUnit(ctx, u)
}

func RemoveBootUnit(ctx context.Context, u BootUnit) error {
	if !BootUnitSupported() {
		return nil
	}
	path, err := u.Path()
	if err != nil {
		return err
	}
	if _, err := os.Stat(path); os.IsNotExist(err) {
		return nil
	}
	if unitIs(ctx, u.Rootless, u.Name(), "enabled") {
		if err := systemctlRun(ctx, u.Rootless, "disable", u.Name()); err != nil {
			return err
		}
	}
	if err := removeHostFiles(ctx, u.Rootless, path); err != nil {
		return err
	}
	return systemctlRun(ctx, u.Rootless, "daemon-reload")
}

func removeHostFiles(ctx context.Context, user bool, paths ...string) error {
	if !user {
		_, err := output(ctx, false, "rm", append([]string{"-f"}, paths...)...)
		return err
	}
	for _, p := range paths {
		if err := os.Remove(p); err != nil && !os.IsNotExist(err) {
			return err
		}
	}
	return nil
}

func writeBootUnit(ctx context.Context, u BootUnit) error {
	if !filepath.IsAbs(u.Binary) || !filepath.IsAbs(u.Dir) {
		return fmt.Errorf("host: boot unit needs absolute paths, got %q and %q", u.Binary, u.Dir)
	}
	path, err := u.Path()
	if err != nil {
		return err
	}
	if !u.Rootless {
		if err := installBinary(ctx, true, u.Binary, LibexecBinary); err != nil {
			return err
		}
	}
	changed, err := writeHostFile(ctx, u.Rootless, path, []byte(u.content(ctx)))
	if err != nil || !changed {
		return err
	}
	return systemctlRun(ctx, u.Rootless, "daemon-reload")
}

func installBinary(ctx context.Context, system bool, src, dst string) error {
	if sameFile(src, dst) {
		return nil
	}
	if !system {
		if out, err := exec.CommandContext(ctx, "install", "-D", "-m", "0755", src, dst).CombinedOutput(); err != nil {
			return &CommandError{Command: "install " + dst, Output: string(out), Err: err}
		}
		return nil
	}
	if _, err := output(ctx, false, "install", installArgs(src, dst)...); err != nil {
		return err
	}
	if has("restorecon") {
		_, _ = output(ctx, false, "restorecon", dst)
	}
	return nil
}

func installArgs(src, dst string) []string {
	args := []string{"-D", "-m", "0755"}
	if uid, gid, ok := fileOwner(dst); ok {
		args = append(args, "-o", strconv.Itoa(uid), "-g", strconv.Itoa(gid))
	}
	return append(args, src, dst)
}

var asRoot = isRoot

func unitFiles(project string) (map[string]bool, error) {
	boot, upgrade := BootUnit{Project: project}.Name(), AutoUpgrade{Project: project}.ServiceName()
	units := map[string]bool{
		filepath.Join("/etc/systemd/system", boot):    true,
		filepath.Join("/etc/systemd/system", upgrade): true,
		filepath.Join(cronDir, project+"-upgrade"):    true,
	}
	if asRoot() {
		return units, nil
	}
	userDir, err := unitDir(true)
	if err != nil {
		return nil, err
	}
	units[filepath.Join(userDir, boot)] = false
	units[filepath.Join(userDir, upgrade)] = false
	return units, nil
}

func RefreshUnitBinaries(ctx context.Context, project, bin string) error {
	if runtime.GOOS != "linux" {
		return nil
	}
	units, err := unitFiles(project)
	if err != nil {
		return err
	}
	for path, system := range units {
		data, err := os.ReadFile(path)
		if os.IsNotExist(err) {
			continue
		}
		if err != nil {
			return err
		}
		for _, b := range unitBinaries(data) {
			if err := installBinary(ctx, system, bin, b); err != nil {
				return err
			}
		}
	}
	return nil
}

func unitBinaries(data []byte) []string {
	var out []string
	for _, line := range strings.Split(string(data), "\n") {
		var bin string
		if v, ok := strings.CutPrefix(line, "ExecStart="); ok {
			bin = systemdWord(v)
		} else {
			bin = cronBinary(line)
		}
		if filepath.IsAbs(bin) && !slices.Contains(out, bin) {
			out = append(out, bin)
		}
	}
	return out
}

func systemdWord(v string) string {
	var word string
	if rest, ok := strings.CutPrefix(v, `"`); ok {
		var b strings.Builder
		for i := 0; i < len(rest) && rest[i] != '"'; i++ {
			if rest[i] == '\\' && i+1 < len(rest) {
				i++
			}
			b.WriteByte(rest[i])
		}
		word = b.String()
	} else {
		word, _, _ = strings.Cut(v, " ")
	}
	return strings.NewReplacer("%%", "%", "$$", "$").Replace(word)
}

func cronBinary(line string) string {
	f := strings.Fields(line)
	if len(f) < 7 || strings.Contains(f[0], "=") || strings.HasPrefix(f[0], "#") {
		return ""
	}
	i := strings.Index(line, " '")
	if i < 0 {
		return ""
	}
	var b strings.Builder
	rest := line[i+2:]
	for {
		end := strings.IndexByte(rest, '\'')
		if end < 0 {
			return ""
		}
		b.WriteString(rest[:end])
		rest = rest[end+1:]
		next, ok := strings.CutPrefix(rest, `\''`)
		if !ok {
			break
		}
		b.WriteByte('\'')
		rest = next
	}
	return strings.ReplaceAll(b.String(), `\%`, "%")
}

func writeHostFile(ctx context.Context, user bool, path string, data []byte) (bool, error) {
	if old, err := os.ReadFile(path); err == nil && bytes.Equal(old, data) {
		return false, nil
	}
	if user {
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			return false, err
		}
		return true, envfile.WriteFileAtomic(path, data, false)
	}
	tmp, err := os.CreateTemp("", "rexctl-unit-")
	if err != nil {
		return false, err
	}
	defer os.Remove(tmp.Name())
	_, werr := tmp.Write(data)
	if cerr := tmp.Close(); werr == nil {
		werr = cerr
	}
	if werr != nil {
		return false, werr
	}
	if _, err := output(ctx, false, "install", "-D", "-m", "0644", tmp.Name(), path); err != nil {
		return false, err
	}
	return true, nil
}

func systemctlRun(ctx context.Context, rootless bool, args ...string) error {
	if rootless {
		if out, err := exec.CommandContext(ctx, "systemctl", append([]string{"--user"}, args...)...).CombinedOutput(); err != nil {
			return &CommandError{Command: "systemctl --user " + strings.Join(args, " "), Output: string(out), Err: err}
		}
		return nil
	}
	_, err := output(ctx, false, "systemctl", args...)
	return err
}

func sameFile(a, b string) bool {
	ha, err := fileHash(a)
	if err != nil {
		return false
	}
	hb, err := fileHash(b)
	return err == nil && bytes.Equal(ha, hb)
}

func fileHash(path string) ([]byte, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	h := sha256.New()
	if _, err := io.Copy(h, f); err != nil {
		return nil, err
	}
	return h.Sum(nil), nil
}
