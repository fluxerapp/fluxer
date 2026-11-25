// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"bytes"
	"context"
	"errors"
	"net"
	"net/http"
	"os"
	"os/exec"
	"os/user"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
)

const healthcheckEventsFile = "fluxer-healthcheck-events.conf"

type PodmanSetup struct {
	Rootless      bool
	Systemd       bool
	SocketMissing bool
	NoLinger      bool
	EventsOn      bool
	NoSocketStart bool
}

func (s PodmanSetup) Needed() bool {
	return s.SocketMissing || s.NoSocketStart || s.NoLinger || s.EventsOn
}

func (s PodmanSetup) ManualFix() []string {
	if !s.Systemd {
		return nil
	}
	sys := "sudo systemctl"
	if s.Rootless {
		sys = "systemctl --user"
	}
	var cmds []string
	if s.SocketMissing || s.NoSocketStart {
		cmds = append(cmds, sys+" enable --now podman.socket")
	}
	if s.NoLinger {
		if u, err := user.Current(); err == nil {
			cmds = append(cmds, "sudo loginctl enable-linger "+u.Username)
		}
	}
	return cmds
}

func CheckPodman(ctx context.Context, rootless, socketMissing bool) PodmanSetup {
	s := PodmanSetup{Rootless: rootless, SocketMissing: socketMissing}
	if runtime.GOOS != "linux" {
		s.SocketMissing = false
		return s
	}
	_, err := os.Stat("/run/systemd/system")
	s.Systemd = err == nil && has("systemctl")
	if s.Systemd {
		out, _ := systemctl(ctx, rootless, "is-enabled", "podman.socket").Output()
		s.NoSocketStart = strings.TrimSpace(string(out)) != "enabled"
		if rootless {
			s.NoLinger = !lingering(ctx)
		}
	}
	s.EventsOn = !healthcheckEventsSet(rootless)
	return s
}

func lingering(ctx context.Context) bool {
	if !has("loginctl") {
		return true
	}
	u, err := user.Current()
	if err != nil {
		return true
	}
	out, _ := exec.CommandContext(ctx, "loginctl", "show-user", u.Username, "--property=Linger", "--value").Output()
	return strings.TrimSpace(string(out)) == "yes"
}

func lingerStep() (Step, bool) {
	u, err := user.Current()
	if err != nil {
		return Step{}, false
	}
	return Step{
		Do:      "Lets your account's services keep running after you sign out, so Fluxer starts with the machine.",
		Command: "loginctl enable-linger " + u.Username,
		Undo:    "loginctl disable-linger " + u.Username,
		Run: func(ctx context.Context) error {
			out, err := exec.CommandContext(ctx, "loginctl", "--no-ask-password", "enable-linger", u.Username).CombinedOutput()
			if err == nil {
				return nil
			}
			if isRoot() || !has("sudo") {
				return &CommandError{Command: "loginctl enable-linger", Output: string(out), Err: err}
			}
			if _, err := output(ctx, false, "loginctl", "enable-linger", u.Username); err != nil {
				return &CommandError{Command: "sudo loginctl enable-linger", Output: string(out), Err: err}
			}
			return nil
		},
	}, true
}

func MissingRootlessTools() []string {
	var out []string
	for _, t := range []string{"newuidmap", "newgidmap"} {
		if !has(t) {
			out = append(out, t)
		}
	}
	if !has("pasta") && !has("slirp4netns") {
		out = append(out, "pasta")
	}
	return out
}

func RootlessToolsFix(again string) string {
	if has("apt-get") {
		return "Run sudo apt-get install uidmap passt, then run " + again + "."
	}
	return "Install the packages that have newuidmap, newgidmap and pasta, then run " + again + "."
}

func HasSubIDs(ctx context.Context) bool {
	u, err := user.Current()
	if err != nil {
		return true
	}
	if has("getsubids") {
		return exec.CommandContext(ctx, "getsubids", u.Username).Run() == nil && exec.CommandContext(ctx, "getsubids", "-g", u.Username).Run() == nil
	}
	return subIDFileHas("/etc/subuid", u) && subIDFileHas("/etc/subgid", u)
}

func subIDFileHas(path string, u *user.User) bool {
	data, err := os.ReadFile(path)
	if err != nil {
		return false
	}
	for _, line := range strings.Split(string(data), "\n") {
		name, _, _ := strings.Cut(strings.TrimSpace(line), ":")
		if name != "" && (name == u.Username || name == u.Uid) {
			return true
		}
	}
	return false
}

func systemctl(ctx context.Context, rootless bool, args ...string) *exec.Cmd {
	if rootless {
		return exec.CommandContext(ctx, "systemctl", append([]string{"--user"}, args...)...)
	}
	bin, argv, err := privileged("systemctl", args...)
	if err != nil {
		return exec.CommandContext(ctx, "systemctl", args...)
	}
	return exec.CommandContext(ctx, bin, argv...)
}

func containersConfDirs(rootless bool) []string {
	var dirs []string
	if rootless {
		if home, err := os.UserHomeDir(); err == nil {
			dirs = append(dirs, filepath.Join(home, ".config", "containers"))
		}
	}
	return append(dirs, "/etc/containers", "/usr/share/containers")
}

func healthcheckEventsSet(rootless bool) bool {
	for _, d := range containersConfDirs(rootless) {
		files, _ := filepath.Glob(filepath.Join(d, "containers.conf.d", "*.conf"))
		files = append(files, filepath.Join(d, "containers.conf"))
		for _, f := range files {
			data, err := os.ReadFile(f)
			if err != nil {
				continue
			}
			for _, line := range bytes.Split(data, []byte("\n")) {
				if bytes.HasPrefix(bytes.TrimSpace(line), []byte("healthcheck_events")) {
					return true
				}
			}
		}
	}
	return false
}

func (s PodmanSetup) Steps() []Step {
	var steps []Step
	if (s.SocketMissing || s.NoSocketStart) && s.Systemd {
		sys := rootCmd("systemctl")
		if s.Rootless {
			sys = "systemctl --user"
		}
		undo := sys + " disable --now podman.socket"
		if !s.SocketMissing {
			undo = sys + " disable podman.socket"
		}
		steps = append(steps, Step{
			Do:      "Starts Podman's API socket, and starts it with the machine from now on.",
			Command: sys + " enable --now podman.socket",
			Undo:    undo,
			Run: func(ctx context.Context) error {
				if out, err := systemctl(ctx, s.Rootless, "--no-ask-password", "enable", "--now", "podman.socket").CombinedOutput(); err != nil {
					if s.Rootless && noUserBus(string(out)) {
						err = ErrNoUserSession
					}
					return &CommandError{Command: "systemctl enable --now podman.socket", Output: string(out), Err: err}
				}
				return waitPodmanSocket(ctx, s.Rootless)
			},
		})
	}
	if s.NoLinger {
		if step, ok := lingerStep(); ok {
			steps = append(steps, step)
		}
	}
	if s.EventsOn {
		dir := filepath.Join(containersConfDirs(s.Rootless)[0], "containers.conf.d")
		path := filepath.Join(dir, healthcheckEventsFile)
		rm := "rm " + path
		if !s.Rootless {
			rm = rootCmd(rm)
		}
		steps = append(steps, Step{
			Do:   "Writes healthcheck_events = false to " + path + ", so Podman stops writing every health check to the system log.",
			Undo: rm,
			Run: func(context.Context) error {
				if err := os.MkdirAll(dir, 0o755); err != nil {
					return err
				}
				return envfile.WriteFileAtomic(path, []byte("[engine]\nhealthcheck_events = false\n"), false)
			},
		})
	}
	return steps
}

func podmanSocketPath(rootless bool) string {
	if !rootless {
		return "/run/podman/podman.sock"
	}
	if dir := os.Getenv("XDG_RUNTIME_DIR"); dir != "" {
		return filepath.Join(dir, "podman", "podman.sock")
	}
	return filepath.Join("/run/user", strconv.Itoa(os.Getuid()), "podman", "podman.sock")
}

func podmanClient(rootless bool) *http.Client {
	path := podmanSocketPath(rootless)
	return &http.Client{Transport: &http.Transport{DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) {
		var d net.Dialer
		return d.DialContext(ctx, "unix", path)
	}}}
}

func pingPodman(ctx context.Context, hc *http.Client) bool {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, "http://d/_ping", nil)
	if err != nil {
		return false
	}
	res, err := hc.Do(req)
	if err != nil {
		return false
	}
	res.Body.Close()
	return res.StatusCode == http.StatusOK
}

func PodmanAPI(ctx context.Context, rootless bool) (PodmanSetup, error) {
	s := PodmanSetup{Rootless: rootless, Systemd: podmanSystemd()}
	if runtime.GOOS != "linux" {
		return s, nil
	}
	hc := podmanClient(rootless)
	defer hc.CloseIdleConnections()
	pingCtx, cancel := context.WithTimeout(ctx, 15*time.Second)
	defer cancel()
	if pingPodman(pingCtx, hc) {
		return s, nil
	}
	if out, _ := systemctlOutput(ctx, rootless, "is-active", "podman.socket"); strings.TrimSpace(string(out)) == "active" {
		if err := PodmanServiceFailed(ctx, rootless); err != nil {
			return s, err
		}
		return s, ErrPodmanAPIDown
	}
	s.SocketMissing = true
	if err := PodmanServiceFailed(ctx, rootless); err != nil {
		return s, err
	}
	return s, ErrPodmanAPIDown
}

func waitPodmanSocket(ctx context.Context, rootless bool) error {
	hc := podmanClient(rootless)
	defer hc.CloseIdleConnections()
	ctx, cancel := context.WithTimeout(ctx, 90*time.Second)
	defer cancel()
	for {
		if pingPodman(ctx, hc) {
			return nil
		}
		if err := PodmanServiceFailed(ctx, rootless); err != nil {
			return err
		}
		select {
		case <-ctx.Done():
			return &CommandError{Output: "Podman's API gave no reply on " + podmanSocketPath(rootless) + " within 90 seconds.", Err: ctx.Err()}
		case <-time.After(time.Second):
		}
	}
}

func PodmanServiceFailed(ctx context.Context, rootless bool) error {
	out, _ := systemctlOutput(ctx, rootless, "is-failed", podmanServiceUnit)
	if strings.TrimSpace(string(out)) != "failed" {
		return nil
	}
	return &CommandError{Output: serviceErrorLines(journal(ctx, rootless, podmanServiceUnit)), Err: ErrPodmanServiceFailed}
}

func journal(ctx context.Context, rootless bool, unit string) []byte {
	args := []string{"-u", unit, "-n", "20", "-o", "cat", "--no-pager"}
	if rootless {
		out, _ := exec.CommandContext(ctx, "journalctl", append([]string{"--user"}, args...)...).Output()
		return out
	}
	out, _ := output(ctx, true, "journalctl", args...)
	return out
}

func serviceErrorLines(out []byte) string {
	var program, unit []string
	for _, line := range strings.Split(strings.TrimSpace(string(out)), "\n") {
		line = strings.TrimSpace(line)
		switch {
		case line == "":
		case strings.HasPrefix(line, "Started ") || strings.HasPrefix(line, "Starting "):
			program, unit = nil, nil
		case strings.HasPrefix(line, podmanServiceUnit+": "):
			unit = append(unit, strings.TrimPrefix(line, podmanServiceUnit+": "))
		default:
			program = append(program, line)
		}
	}
	lines := program
	if len(lines) == 0 {
		lines = unit
	}
	if len(lines) > 3 {
		lines = lines[len(lines)-3:]
	}
	return strings.Join(lines, "\n")
}

type CommandError struct {
	Command string
	Output  string
	Err     error
}

func (e *CommandError) Error() string {
	out := strings.TrimSpace(e.Output)
	switch {
	case e.Command == "" && out == "":
		return e.Err.Error()
	case e.Command == "":
		return out
	case out == "":
		return e.Command + ": " + e.Err.Error()
	}
	return e.Command + ": " + out
}

func (e *CommandError) Unwrap() error { return e.Err }

const (
	podmanRestartUnit = "podman-restart.service"
	podmanServiceUnit = "podman.service"
)

var (
	ErrPodmanServiceFailed = errors.New("the podman.service unit failed")
	ErrPodmanAPIDown       = errors.New("the podman.socket unit isn't running")
	ErrNoUserSession       = errors.New("this shell has no systemd user session")
)

func noUserBus(out string) bool {
	return strings.Contains(out, "user scope bus") || strings.Contains(out, "Failed to connect to bus") || strings.Contains(out, "XDG_RUNTIME_DIR not defined")
}

func NoUserSession(next string) (msg, fix string) {
	name := "your account"
	if u, err := user.Current(); err == nil {
		name = u.Username
	}
	return "This shell has no systemd session for " + name + ", so Podman can't start.", "Sign in as " + name + " over SSH or at the console, then run " + next + "."
}

func podmanSystemd() bool {
	if runtime.GOOS != "linux" || !has("systemctl") {
		return false
	}
	_, err := os.Stat("/run/systemd/system")
	return err == nil
}

func PodmanRestartOn(ctx context.Context, rootless bool) bool {
	if !podmanSystemd() {
		return true
	}
	out, _ := systemctl(ctx, rootless, "is-enabled", podmanRestartUnit).Output()
	return strings.TrimSpace(string(out)) == "enabled"
}

func PodmanRestartStep(rootless bool) Step {
	sys := rootCmd("systemctl")
	if rootless {
		sys = "systemctl --user"
	}
	return Step{
		Do:      "Turns on Podman's own " + podmanRestartUnit + ", which starts Fluxer's container with the machine.",
		Command: sys + " enable " + podmanRestartUnit,
		Undo:    sys + " disable " + podmanRestartUnit,
		Run: func(ctx context.Context) error {
			if !podmanSystemd() {
				return nil
			}
			if out, err := systemctl(ctx, rootless, "enable", podmanRestartUnit).CombinedOutput(); err != nil {
				return &CommandError{Command: "systemctl enable " + podmanRestartUnit, Output: string(out), Err: err}
			}
			return nil
		},
	}
}
