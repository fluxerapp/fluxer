// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"bytes"
	"context"
	"crypto/sha256"
	"errors"
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

const StandaloneNativeUnit = "fluxerd.service"

const nativeRuntimeRoot = "rexctl"

const (
	noNamespacesDropIn = "no-namespaces.conf"
	exitNamespace      = 226
)

var NoNamespacesOff = []string{"PrivateTmp", "ProtectSystem"}

func NativeRuntimeDir(project string) string {
	return filepath.Join("/run", nativeRuntimeRoot, project)
}

var (
	ErrNativeRootless   = errors.New("host: the native runtime needs a system unit")
	ErrStandaloneNative = errors.New("host: fluxerd.service runs or is enabled on this machine")
)

type BootUnit struct {
	Project   string
	Dir       string
	Binary    string
	Podman    bool
	Rootless  bool
	StopGrace time.Duration
	Native    bool
	User      string
	Fluxerd   string
	EnvFile   string
	DataDir   string
	CaddyDir  string
	CertsDir  string
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

var (
	systemUnitDir     = "/etc/systemd/system"
	bootUnitSupported = func() bool {
		if runtime.GOOS != "linux" || !has("systemctl") {
			return false
		}
		_, err := os.Stat("/run/systemd/system")
		return err == nil
	}
)

func BootUnitSupported() bool { return bootUnitSupported() }

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
		return systemUnitDir, nil
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

func (u BootUnit) NoNamespacesPath() string {
	return filepath.Join(systemUnitDir, u.Name()+".d", noNamespacesDropIn)
}

var namespacesFail = func(ctx context.Context, dataDir string) bool {
	if !has("systemd-run") {
		return false
	}
	err := exec.CommandContext(ctx, "systemd-run", "--quiet", "--wait", "--collect", "--service-type=exec",
		"-p", "PrivateTmp=yes", "-p", "ProtectSystem=strict", "-p", "ReadWritePaths="+dataDir, "/bin/true").Run()
	var ee *exec.ExitError
	return errors.As(err, &ee) && ee.ExitCode() == exitNamespace
}

func noNamespacesContent() []byte {
	var b strings.Builder
	b.WriteString("[Service]\n")
	for _, k := range NoNamespacesOff {
		fmt.Fprintf(&b, "%s=no\n", k)
	}
	b.WriteString("ReadWritePaths=\n")
	return []byte(b.String())
}

func (u BootUnit) namespaceStep(ctx context.Context) (Step, bool) {
	path := u.NoNamespacesPath()
	_, err := os.Stat(path)
	have := err == nil
	if namespacesFail(ctx, u.DataDir) == have {
		return Step{}, false
	}
	off := strings.Join(NoNamespacesOff, " and ")
	if have {
		return Step{
			Do:      "Deletes " + path + ", because this machine can give Fluxer's service its own mount namespace again, so " + off + " are back on.",
			Command: rootCmd("rm -f " + path),
			Run: func(ctx context.Context) error {
				if err := removeHostFiles(ctx, false, path); err != nil {
					return err
				}
				return systemctlRun(ctx, false, "daemon-reload")
			},
		}, true
	}
	return Step{
		Do: "Writes " + path + ", which turns off " + off + ", because this machine can't give Fluxer's service its own mount namespace.",
		Run: func(ctx context.Context) error {
			if _, err := writeHostFile(ctx, false, path, noNamespacesContent()); err != nil {
				return err
			}
			return systemctlRun(ctx, false, "daemon-reload")
		},
	}, true
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

func (u BootUnit) renderNative() string {
	var b strings.Builder
	b.WriteString("[Unit]\n")
	fmt.Fprintf(&b, "Description=Fluxer in %s\n", strings.ReplaceAll(u.Dir, "%", "%%"))
	b.WriteString("After=network-online.target\n")
	b.WriteString("Wants=network-online.target\n")
	b.WriteString("\n[Service]\n")
	b.WriteString("Type=notify\n")
	b.WriteString("NotifyAccess=main\n")
	if u.User != "" {
		fmt.Fprintf(&b, "User=%s\n", u.User)
		fmt.Fprintf(&b, "Group=%s\n", u.User)
	}
	fmt.Fprintf(&b, "Environment=%s\n", systemdQuote("FLUXER_DATA_DIR="+u.DataDir))
	fmt.Fprintf(&b, "ExecStart=%s run --config %s --caddy-dir %s --certs-dir %s\n", systemdQuote(u.Fluxerd), systemdQuote(u.EnvFile), systemdQuote(u.CaddyDir), systemdQuote(u.CertsDir))
	b.WriteString("ExecReload=/bin/kill -HUP $MAINPID\n")
	b.WriteString("Restart=on-failure\n")
	b.WriteString("RestartSec=5\n")
	b.WriteString("RestartSteps=6\n")
	b.WriteString("RestartMaxDelaySec=5min\n")
	b.WriteString("TimeoutStartSec=15min\n")
	b.WriteString("TimeoutStopSec=90\n")
	b.WriteString("KillMode=mixed\n")
	fmt.Fprintf(&b, "RuntimeDirectory=%s/%s\n", nativeRuntimeRoot, u.Project)
	b.WriteString("RuntimeDirectoryMode=0750\n")
	b.WriteString("AmbientCapabilities=CAP_NET_BIND_SERVICE\n")
	b.WriteString("LimitNOFILE=65536\n")
	b.WriteString("NoNewPrivileges=yes\n")
	b.WriteString("ProtectSystem=strict\n")
	fmt.Fprintf(&b, "ReadWritePaths=%s\n", systemdQuote(u.DataDir))
	b.WriteString("PrivateTmp=yes\n")
	b.WriteString("\n[Install]\n")
	b.WriteString("WantedBy=multi-user.target\n")
	return b.String()
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
	refresh := ""
	if runAs != "" {
		refresh = fmt.Sprintf("-+/usr/bin/install -D -m 0755 %s %s\n", systemdQuote(u.Binary), LibexecBinary)
	}
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
	if refresh != "" {
		b.WriteString("ExecStartPre=" + refresh)
	}
	fmt.Fprintf(&b, "ExecStart=%s start --boot --dir %s\n", bin, dir)
	if refresh != "" {
		b.WriteString("ExecStop=" + refresh)
	}
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

func (u BootUnit) runAs() string {
	if u.Native {
		return u.User
	}
	if u.Rootless || u.Podman {
		return ""
	}
	return dirOwner(u.Dir)
}

func (u BootUnit) content(ctx context.Context) string {
	if u.Native {
		return u.renderNative()
	}
	return u.render(u.Rootless && unitExists(ctx, true, userWaitNetwork), u.runAs())
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

func BootUnitStarting(ctx context.Context, project string) bool {
	if !BootUnitSupported() {
		return false
	}
	name := project + ".service"
	for _, user := range []bool{false, true} {
		args := []string{"is-active", name}
		if user {
			args = append([]string{"--user"}, args...)
		}
		out, _ := exec.CommandContext(ctx, "systemctl", args...).Output()
		if strings.TrimSpace(string(out)) == "activating" {
			return true
		}
	}
	return false
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
	if !u.Rootless && !u.Native && u.runAs() == "" {
		s.CopyStale = !sameFile(u.Binary, LibexecBinary)
	}
	if u.Rootless {
		s.NoLinger = !lingering(ctx)
	}
	return s, nil
}

func (u BootUnit) systemctl(args string) string {
	if u.Rootless {
		return "systemctl --user " + args
	}
	return rootCmd("systemctl " + args)
}

func (u BootUnit) InstallSteps(ctx context.Context) ([]Step, error) {
	if !BootUnitSupported() {
		return nil, nil
	}
	steps, path, err := u.writeSteps(ctx)
	if err != nil {
		return nil, err
	}
	if !unitIs(ctx, u.Rootless, u.Name(), "enabled") {
		steps = append(steps, Step{
			Do:      "Starts " + u.Name() + " with the machine from now on.",
			Command: u.systemctl("enable " + u.Name()),
			Undo:    u.removeSteps(path, true)[0].Command,
			Run:     func(ctx context.Context) error { return systemctlRun(ctx, u.Rootless, "enable", u.Name()) },
		})
	}
	if u.Rootless && !lingering(ctx) {
		if step, ok := lingerStep(); ok {
			steps = append(steps, step)
		}
	}
	if !unitIs(ctx, u.Rootless, u.Name(), "active") {
		steps = append(steps, Step{
			Do:      "Starts " + u.Name() + " now.",
			Command: u.systemctl("start --no-block " + u.Name()),
			Run:     func(ctx context.Context) error { return systemctlRun(ctx, u.Rootless, "start", "--no-block", u.Name()) },
		})
	}
	return steps, nil
}

func (u BootUnit) RefreshSteps(ctx context.Context) ([]Step, error) {
	if !BootUnitSupported() {
		return nil, nil
	}
	path, err := u.Path()
	if err != nil {
		return nil, err
	}
	if _, err := os.Stat(path); os.IsNotExist(err) {
		return nil, nil
	}
	steps, _, err := u.writeSteps(ctx)
	return steps, err
}

func (u BootUnit) RemoveSteps(ctx context.Context) ([]Step, error) {
	if !BootUnitSupported() {
		return nil, nil
	}
	path, err := u.Path()
	if err != nil {
		return nil, err
	}
	if _, err := os.Stat(path); os.IsNotExist(err) {
		return nil, nil
	}
	return u.removeSteps(path, unitIs(ctx, u.Rootless, u.Name(), "enabled")), nil
}

func (u BootUnit) removeSteps(path string, enabled bool) []Step {
	rm := "rm -f " + path
	if !u.Rootless {
		rm = rootCmd(rm)
	}
	steps := []Step{
		{
			Do:      "Stops starting " + u.Name() + " with the machine.",
			Command: u.systemctl("disable " + u.Name()),
			Run:     func(ctx context.Context) error { return systemctlRun(ctx, u.Rootless, "disable", u.Name()) },
		},
		{
			Do:      "Deletes " + path + ".",
			Command: rm,
			Run: func(ctx context.Context) error {
				if u.Native {
					return removeHostFiles(ctx, false, path, u.NoNamespacesPath())
				}
				return removeHostFiles(ctx, u.Rootless, path)
			},
		},
		{
			Do:      "Tells systemd that " + u.Name() + " is gone.",
			Command: u.systemctl("daemon-reload"),
			Run:     func(ctx context.Context) error { return systemctlRun(ctx, u.Rootless, "daemon-reload") },
		},
	}
	if !enabled {
		return steps[1:]
	}
	return steps
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

func (u BootUnit) writeSteps(ctx context.Context) ([]Step, string, error) {
	if u.Native {
		if err := u.nativeAllowed(ctx); err != nil {
			return nil, "", err
		}
	} else if !filepath.IsAbs(u.Binary) || !filepath.IsAbs(u.Dir) {
		return nil, "", fmt.Errorf("host: boot unit needs absolute paths, got %q and %q", u.Binary, u.Dir)
	}
	path, err := u.Path()
	if err != nil {
		return nil, "", err
	}
	content := []byte(u.content(ctx))
	old, readErr := os.ReadFile(path)
	write := readErr != nil || !bytes.Equal(old, content)
	var steps []Step
	if !u.Rootless && !u.Native && (write || u.runAs() == "") && !sameFile(u.Binary, LibexecBinary) {
		steps = append(steps, Step{
			Do:      "Copies this rexctl to " + LibexecBinary + " for the service to use.",
			Command: rootCmd("install " + strings.Join(installArgs(u.Binary, LibexecBinary), " ")),
			Run:     func(ctx context.Context) error { return installBinary(ctx, true, u.Binary, LibexecBinary) },
		})
	}
	if write {
		undo := ""
		if os.IsNotExist(readErr) {
			undo = joinCommands(u.removeSteps(path, false))
		}
		steps = append(steps, Step{
			Do:   "Writes " + path + ", which starts Fluxer with the machine.",
			Undo: undo,
			Run: func(ctx context.Context) error {
				changed, err := writeHostFile(ctx, u.Rootless, path, content)
				if err != nil || !changed {
					return err
				}
				return systemctlRun(ctx, u.Rootless, "daemon-reload")
			},
		})
	}
	if u.Native {
		if step, ok := u.namespaceStep(ctx); ok {
			steps = append(steps, step)
		}
	}
	return steps, path, nil
}

func (u BootUnit) nativeAllowed(ctx context.Context) error {
	if u.Rootless {
		return ErrNativeRootless
	}
	for _, p := range []string{u.Dir, u.Fluxerd, u.EnvFile, u.DataDir, u.CaddyDir, u.CertsDir} {
		if !filepath.IsAbs(p) {
			return fmt.Errorf("host: the native unit needs absolute paths, got %q", p)
		}
	}
	if StandaloneNativeInUse(ctx) {
		return ErrStandaloneNative
	}
	return nil
}

func StandaloneNativeInUse(ctx context.Context) bool {
	if unitIs(ctx, false, StandaloneNativeUnit, "enabled") {
		return true
	}
	out, _ := exec.CommandContext(ctx, "systemctl", "show", "-p", "ActiveState", "--value", StandaloneNativeUnit).Output()
	switch strings.TrimSpace(string(out)) {
	case "active", "activating", "reloading", "deactivating":
		return true
	}
	return false
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

func unitFiles(project string) (map[string]bool, error) {
	boot, upgrade := BootUnit{Project: project}.Name(), AutoUpgrade{Project: project}.ServiceName()
	units := map[string]bool{
		filepath.Join(systemUnitDir, boot):         true,
		filepath.Join(systemUnitDir, upgrade):      true,
		filepath.Join(cronDir, project+"-upgrade"): true,
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

func RefreshUnitBinarySteps(project, bin string) ([]Step, error) {
	if runtime.GOOS != "linux" {
		return nil, nil
	}
	units, err := unitFiles(project)
	if err != nil {
		return nil, err
	}
	return unitBinarySteps(units, bin)
}

func unitBinarySteps(units map[string]bool, bin string) ([]Step, error) {
	paths := make([]string, 0, len(units))
	for path := range units {
		paths = append(paths, path)
	}
	slices.Sort(paths)
	var steps []Step
	seen := map[string]bool{}
	for _, path := range paths {
		data, err := os.ReadFile(path)
		if os.IsNotExist(err) {
			continue
		}
		if err != nil {
			return nil, err
		}
		if bytes.Contains(data, []byte("\nType=notify\n")) {
			continue
		}
		system := units[path]
		for _, dst := range unitBinaries(data) {
			if seen[dst] || sameFile(bin, dst) {
				continue
			}
			seen[dst] = true
			command := "install " + strings.Join(installArgs(bin, dst), " ")
			if system {
				command = rootCmd(command)
			}
			steps = append(steps, Step{
				Do:      "Updates " + dst + ", the copy of rexctl that " + path + " runs.",
				Command: command,
				Run: func(ctx context.Context) error {
					return installBinary(ctx, system, bin, dst)
				},
			})
		}
	}
	return steps, nil
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

func LibexecRemoveSteps(ctx context.Context, u BootUnit) []Step {
	if u.Rootless {
		return nil
	}
	if _, err := os.Stat(LibexecBinary); err != nil {
		return nil
	}
	own, err := u.Path()
	if err != nil || libexecReferenced(systemUnitDir, own) {
		return nil
	}
	dir := filepath.Dir(LibexecBinary)
	return []Step{{
		Do:      "Deletes the copy of rexctl in " + LibexecBinary + ", which nothing else uses.",
		Command: rootCmd("rm -f " + LibexecBinary),
		Run: func(ctx context.Context) error {
			if err := removeHostFiles(ctx, false, LibexecBinary); err != nil {
				return err
			}
			_, _ = output(ctx, false, "rmdir", "--ignore-fail-on-non-empty", dir)
			return nil
		},
	}}
}

func libexecReferenced(unitDir, except string) bool {
	entries, err := os.ReadDir(unitDir)
	if err != nil {
		return true
	}
	for _, entry := range entries {
		path := filepath.Join(unitDir, entry.Name())
		if path == except || entry.IsDir() {
			continue
		}
		data, err := os.ReadFile(path)
		if err == nil && bytes.Contains(data, []byte(LibexecBinary)) {
			return true
		}
	}
	return false
}
