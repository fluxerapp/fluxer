// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"bufio"
	"bytes"
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func init() {
	Register(Check{ID: "fluxer-config", Phases: Installed, Run: nativeOnly(runFluxerConfig)})
	Register(Check{ID: "fluxer-namespaces", Phases: Installed, Run: nativeOnly(runFluxerNamespaces), Fix: fixFluxerNamespaces})
	Register(Check{ID: "fluxer-stopped", Phases: Installed, Run: notWhileRestoring(nativeOnly(runFluxerStopped)), Fix: fixFluxerStopped, SafeFix: true})
}

const checkConfigTimeout = 30 * time.Second

func (e *Env) unitRunning(ctx context.Context) bool {
	switch e.stack().ActiveState(ctx) {
	case "active", "activating", "reloading":
		return true
	}
	return false
}

func runFluxerConfig(ctx context.Context, e *Env) Result {
	if e.Instance == nil || os.Geteuid() != 0 {
		return skip("")
	}
	n := e.stack()
	if _, err := os.Stat(n.Fluxerd()); err != nil {
		return skip("")
	}
	ctx, cancel := context.WithTimeout(ctx, checkConfigTimeout)
	defer cancel()
	c := exec.CommandContext(ctx, "runuser", "-u", native.ServiceUser, "--", n.Fluxerd(), "check-config",
		"--config", n.EnvFile(), "--caddy-dir", e.Instance.Path(instance.CaddyDropInDir), "--certs-dir", e.Instance.Path(instance.CertsDir))
	c.Dir = n.Root()
	c.Env = []string{"PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin", "HOME=" + n.Root()}
	var out bytes.Buffer
	c.Stdout, c.Stderr = &out, &out
	err := c.Run()
	var ee *exec.ExitError
	switch {
	case err == nil:
		return pass("fluxerd accepts Fluxer's settings.")
	case !errors.As(err, &ee):
		return skip("")
	}
	r := fail("fluxerd doesn't accept Fluxer's settings, so Fluxer won't start.", "Run rexctl config edit and fix the setting named below.")
	for _, line := range strings.Split(strings.TrimSpace(out.String()), "\n") {
		if line = strings.TrimSpace(line); line != "" {
			r.Details = append(r.Details, line)
		}
	}
	return r
}

func (e *Env) noNamespacesPath() (string, bool) {
	path := e.stack().BootUnit().NoNamespacesPath()
	_, err := os.Stat(path)
	return path, err == nil
}

func runFluxerNamespaces(ctx context.Context, e *Env) Result {
	if e.Instance == nil {
		return skip("")
	}
	if _, ok := e.noNamespacesPath(); !ok {
		return pass("Fluxer's service runs in its own mount namespace.")
	}
	r := fixable(warn(
		fmt.Sprintf("Fluxer's service runs with %s turned off, because this machine couldn't give it its own mount namespace.", strings.Join(host.NoNamespacesOff, " and ")),
		"If this is an LXC container, turn on nesting, then run rexctl doctor --fix to turn them back on.",
	))
	r.Manual = "If this is an LXC container, turn on nesting, then run rexctl doctor --fix again."
	return r
}

func fixFluxerNamespaces(ctx context.Context, e *Env) error {
	if e.Instance == nil || e.Restart == nil {
		return errNoProject
	}
	if err := e.run(ctx, host.Step{
		Do:      "Checks whether this machine can give Fluxer's service its own mount namespace again, and restarts Fluxer with it if so.",
		Command: "rexctl restart",
		Run:     func(ctx context.Context) error { return e.Restart(ctx, nil) },
	}); err != nil || e.dryRun {
		return err
	}
	if _, ok := e.noNamespacesPath(); ok {
		return errors.New("checks: this machine still can't give Fluxer's service its own mount namespace")
	}
	return nil
}

type busyPort struct {
	port  int
	owner string
}

func runFluxerPorts(ctx context.Context, e *Env) Result {
	if e.Instance == nil || e.unitRunning(ctx) {
		return skip("")
	}
	wanted := e.wantedPorts()
	var busy []busyPort
	for _, p := range native.InternalPortsFree(e.stack().Loopback()) {
		_, owner, _ := host.PortInUse("tcp", p)
		busy = append(busy, busyPort{p, owner})
	}
	seen := map[stack.Port]bool{}
	for _, p := range wanted {
		if seen[p] {
			continue
		}
		seen[p] = true
		used, owner, err := host.PortInUse(p.Proto, p.Number)
		if err == nil && used {
			busy = append(busy, busyPort{p.Number, owner})
		}
	}
	if len(busy) == 0 {
		return pass("The ports Fluxer uses on this machine are free.")
	}
	var ports, details []string
	for _, b := range busy {
		ports = append(ports, strconv.Itoa(b.port))
		if b.owner != "" {
			details = append(details, fmt.Sprintf("Port %d is used by %s.", b.port, b.owner))
		}
	}
	msg := fmt.Sprintf("Another program uses port %s on this machine, which Fluxer needs, so Fluxer can't start.", ports[0])
	if len(ports) > 1 {
		msg = fmt.Sprintf("Other programs use ports %s on this machine, which Fluxer needs, so Fluxer can't start.", ui.JoinAnd(ports))
	}
	r := fail(msg, "Stop the program that uses them, then run rexctl start.")
	r.Details = details
	return r
}

func (e *Env) stoppedMarker() (string, bool) {
	path, err := e.stack().BootUnit().Path()
	if err != nil {
		return "", false
	}
	data, err := os.ReadFile(path)
	if err != nil {
		return "", false
	}
	s := bufio.NewScanner(bytes.NewReader(data))
	for s.Scan() {
		if v, ok := strings.CutPrefix(strings.TrimSpace(s.Text()), "ConditionPathExists=!"); ok && strings.HasPrefix(v, "/") {
			return v, true
		}
	}
	return "", false
}

func runFluxerStopped(ctx context.Context, e *Env) Result {
	st := e.state()
	if st == nil {
		return skip("")
	}
	marker, ok := e.stoppedMarker()
	if !ok {
		return skip("")
	}
	_, err := os.Stat(marker)
	present := err == nil
	switch {
	case err != nil && !errors.Is(err, fs.ErrNotExist):
		return skip("")
	case present == st.Stopped:
		return pass("Fluxer's service knows whether you stopped Fluxer.")
	case present:
		return fixable(warn("Fluxer won't start with this machine, because a stopped marker is left over from rexctl stop.", "Run rexctl doctor --fix to remove it."))
	}
	return fixable(warn("You stopped Fluxer, but its service would start it again after this machine restarts.", "Run rexctl doctor --fix to keep it stopped, or rexctl start to start it."))
}

func fixFluxerStopped(ctx context.Context, e *Env) error {
	st := e.state()
	marker, ok := e.stoppedMarker()
	if st == nil || !ok {
		return errNoProject
	}
	if st.Stopped {
		return e.run(ctx, host.Step{
			Do:      "Writes " + marker + " so Fluxer stays stopped after this machine restarts.",
			Command: "touch " + marker,
			Undo:    "rexctl start",
			Run: func(context.Context) error {
				return os.WriteFile(marker, nil, 0o644)
			},
		})
	}
	return e.run(ctx, host.Step{
		Do:      "Deletes " + marker + " so Fluxer starts with this machine again.",
		Command: "rm " + marker,
		Undo:    "rexctl stop",
		Run: func(context.Context) error {
			if err := os.Remove(marker); err != nil && !errors.Is(err, fs.ErrNotExist) {
				return err
			}
			return nil
		},
	})
}
