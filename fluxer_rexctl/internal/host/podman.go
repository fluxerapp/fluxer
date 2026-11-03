// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"bytes"
	"context"
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

func enableLinger(ctx context.Context) error {
	u, err := user.Current()
	if err != nil {
		return nil
	}
	if out, err := exec.CommandContext(ctx, "loginctl", "enable-linger", u.Username).CombinedOutput(); err != nil {
		return &CommandError{Command: "loginctl enable-linger", Output: string(out), Err: err}
	}
	return nil
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

func FixPodman(ctx context.Context, s PodmanSetup) error {
	if (s.SocketMissing || s.NoSocketStart) && s.Systemd {
		if out, err := systemctl(ctx, s.Rootless, "enable", "--now", "podman.socket").CombinedOutput(); err != nil {
			return &CommandError{Command: "systemctl enable --now podman.socket", Output: string(out), Err: err}
		}
		waitPodmanSocket(ctx, s.Rootless)
	}
	if s.NoLinger {
		if err := enableLinger(ctx); err != nil {
			return err
		}
	}
	if s.EventsOn {
		dir := filepath.Join(containersConfDirs(s.Rootless)[0], "containers.conf.d")
		if err := os.MkdirAll(dir, 0o755); err != nil {
			return err
		}
		if err := envfile.WriteFileAtomic(filepath.Join(dir, healthcheckEventsFile), []byte("[engine]\nhealthcheck_events = false\n"), false); err != nil {
			return err
		}
	}
	return nil
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

func waitPodmanSocket(ctx context.Context, rootless bool) {
	path := podmanSocketPath(rootless)
	hc := &http.Client{Transport: &http.Transport{DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) {
		var d net.Dialer
		return d.DialContext(ctx, "unix", path)
	}}}
	defer hc.CloseIdleConnections()
	ctx, cancel := context.WithTimeout(ctx, 90*time.Second)
	defer cancel()
	for {
		req, err := http.NewRequestWithContext(ctx, http.MethodGet, "http://d/_ping", nil)
		if err != nil {
			return
		}
		if res, err := hc.Do(req); err == nil {
			res.Body.Close()
			if res.StatusCode == http.StatusOK {
				return
			}
		}
		select {
		case <-ctx.Done():
			return
		case <-time.After(time.Second):
		}
	}
}

type CommandError struct {
	Command string
	Output  string
	Err     error
}

func (e *CommandError) Error() string {
	out := strings.TrimSpace(e.Output)
	if out == "" {
		return e.Command + ": " + e.Err.Error()
	}
	return e.Command + ": " + out
}

func (e *CommandError) Unwrap() error { return e.Err }
