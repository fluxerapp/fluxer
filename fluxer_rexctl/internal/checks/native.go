// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"cmp"
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"os/user"
	"slices"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func init() {
	Register(Check{ID: "service-account", Phases: Installed, Run: nativeOnly(runServiceAccount), Fix: fixServiceAccount})
}

func (e *Env) stack() *native.Stack {
	if e.Stack != nil {
		return e.Stack
	}
	return native.New(e.Instance)
}

func runFluxerService(ctx context.Context, e *Env) Result {
	if e.Instance == nil {
		return skip("")
	}
	st := e.stack().Status(ctx)
	running := st.Unit == "active" || st.Unit == "activating" || st.Unit == "reloading"
	switch {
	case !running && e.state().Stopped:
		return pass("Fluxer is stopped, as you asked. Run rexctl start to start it.")
	case st.Unit == "failed":
		return fixable(fail("Fluxer's service stopped after an error.", "Run rexctl doctor --fix to start it, or rexctl logs to see why."))
	case !running:
		return fixable(fail("Fluxer's service isn't running.", "Run rexctl doctor --fix to start it."))
	case native.CrashLooping(st.Unit, st.Restarts):
		return fail("Fluxer's service keeps stopping.", "Run rexctl logs to see why.")
	case st.PSErr != nil && os.Geteuid() != 0:
		return warn("Fluxer's service is running, and only root can see its parts.", "Run rexctl doctor with sudo.")
	case st.PSErr != nil:
		return warn("Fluxer's service is running, and it didn't say which parts are up.", "Run rexctl doctor again in a minute.")
	}
	if down := failedChildren(st); len(down) > 0 {
		return fixable(fail(Services(down, "keeps stopping.", "keep stopping."), "Run rexctl doctor --fix to restart "+pronoun(down)+", or rexctl logs to see why."))
	}
	if st.Restarts > 0 {
		times := "times"
		if st.Restarts == 1 {
			times = "time"
		}
		return warn(fmt.Sprintf("systemd restarted Fluxer's service %d %s since it last started.", st.Restarts, times), "Run rexctl logs to see why.")
	}
	return pass("Fluxer's service is running.")
}

func pronoun(names []string) string {
	if len(names) == 1 {
		return "it"
	}
	return "them"
}

func failedChildren(st native.Status) []string {
	var out []string
	out = append(out, st.Readiness.Failed...)
	out = append(out, st.Crashing...)
	slices.Sort(out)
	return slices.Compact(slices.DeleteFunc(out, func(s string) bool { return s == native.Supervisor }))
}

func fixFluxerService(ctx context.Context, e *Env) error {
	if e.Instance == nil || e.Start == nil || e.Restart == nil {
		return errNoProject
	}
	st := e.stack().Status(ctx)
	running := st.Unit == "active" || st.Unit == "activating" || st.Unit == "reloading"
	if !running {
		return e.run(ctx, host.Step{Do: "Starts Fluxer and waits until it's ready.", Command: "rexctl start", Run: e.Start})
	}
	down := failedChildren(st)
	if len(down) == 0 {
		return nil
	}
	var parts []string
	if e.Parts != nil {
		parts = e.Parts(down)
	}
	if len(parts) == 0 {
		return e.run(ctx, host.Step{
			Do:      "Restarts Fluxer and waits until it's ready.",
			Command: "rexctl restart",
			Run:     func(ctx context.Context) error { return e.Restart(ctx, nil) },
		})
	}
	return e.run(ctx, host.Step{
		Do:      fmt.Sprintf("Restarts %s and waits until Fluxer is ready.", ui.JoinAnd(parts)),
		Command: "rexctl restart " + strings.Join(parts, " "),
		Run:     func(ctx context.Context) error { return e.Restart(ctx, parts) },
	})
}

func runFluxerBoot(ctx context.Context, e *Env) Result {
	st := e.state()
	if st == nil || st.Stopped || st.BootService == instance.BootServiceOff {
		return skip("")
	}
	if e.stack().Enabled(ctx) {
		return pass("Fluxer starts with this machine.")
	}
	return fixable(warn("Fluxer won't start again after this machine restarts.", "Run rexctl doctor --fix to start it with this machine, or rexctl boot off if you don't want that."))
}

func fixFluxerBoot(ctx context.Context, e *Env) error {
	if e.Instance == nil {
		return errNoProject
	}
	n := e.stack()
	return e.run(ctx, host.Step{
		Do:      "Starts Fluxer's service with this machine.",
		Command: "systemctl enable " + n.Unit(),
		Undo:    "rexctl boot off",
		Run:     n.Enable,
	})
}

func runServiceAccount(ctx context.Context, e *Env) Result {
	if _, err := user.Lookup(native.ServiceUser); err != nil {
		var unknown user.UnknownUserError
		if errors.As(err, &unknown) {
			return fixable(fail("The account "+native.ServiceUser+" that runs Fluxer is missing.", "Run rexctl doctor --fix to create it."))
		}
		return skip("")
	}
	return pass("The account " + native.ServiceUser + " runs Fluxer.")
}

func fixServiceAccount(ctx context.Context, e *Env) error {
	return e.run(ctx, host.Step{
		Do:      "Creates the system account " + native.ServiceUser + " that runs Fluxer.",
		Command: "useradd --system --user-group --no-create-home --shell /usr/sbin/nologin " + native.ServiceUser,
		Run:     native.EnsureUser,
	})
}

func (e *Env) nativeRelease() string {
	if e.Instance == nil {
		return ""
	}
	return cmp.Or(e.stack().Running(), e.state().Release)
}

func dpkgMissing(ctx context.Context, packages []string) ([]string, bool) {
	if _, err := exec.LookPath("dpkg-query"); err != nil {
		return nil, false
	}
	var missing []string
	for _, p := range packages {
		out, err := exec.CommandContext(ctx, "dpkg-query", "-W", "-f=${Status}", p).Output()
		if err != nil || !strings.Contains(string(out), "install ok installed") {
			missing = append(missing, p)
		}
	}
	return missing, true
}

func runNativePackages(ctx context.Context, e *Env) Result {
	version := e.nativeRelease()
	if version == "" || e.Instance != nil && e.Instance.State.IsVoice() {
		return skip("")
	}
	packages, err := e.stack().Packages(version)
	if err != nil {
		return skip("")
	}
	missing, ok := dpkgMissing(ctx, packages)
	switch {
	case !ok:
		return skip("")
	case len(missing) == 1:
		return fixable(fail("The system package "+missing[0]+" that Fluxer needs isn't installed.", "Run rexctl doctor --fix to install it."))
	case len(missing) > 1:
		r := fixable(fail(fmt.Sprintf("%d system packages that Fluxer needs aren't installed.", len(missing)), "Run rexctl doctor --fix to install them."))
		r.Details = missing
		return r
	}
	return pass("The system packages Fluxer needs are installed.")
}

func fixNativePackages(ctx context.Context, e *Env) error {
	version := e.nativeRelease()
	if version == "" {
		return errNoProject
	}
	n := e.stack()
	return e.run(ctx, host.Step{
		Do:      "Installs the system packages Fluxer needs with apt.",
		Command: "apt-get install",
		Run:     func(ctx context.Context) error { return n.EnsurePackages(ctx, version, nil) },
	})
}

type nativeFile struct {
	problem string
	fix     host.Step
}

func (e *Env) nativeFiles() []nativeFile {
	if e.Instance == nil {
		return nil
	}
	var out []nativeFile
	n := e.stack()
	if u, err := user.Lookup(native.ServiceUser); err == nil {
		uid, _ := strconv.Atoi(u.Uid)
		owner := native.ServiceUser + ":" + native.ServiceUser
		if info, err := os.Lstat(n.Root()); err == nil {
			if got, _, ok := host.OwnerOf(info); ok && got != uid {
				root := n.Root()
				out = append(out, nativeFile{
					problem: fmt.Sprintf("%s isn't owned by %s, so Fluxer can't write its data there.", root, native.ServiceUser),
					fix: host.Step{
						Do:      "Gives " + native.ServiceUser + " back " + root + " and everything in it.",
						Command: "chown -R " + owner + " " + root,
						Run: func(ctx context.Context) error {
							return exec.CommandContext(ctx, "chown", "-R", owner, root).Run()
						},
					},
				})
			}
		}
	}
	for _, p := range []string{n.SecretsFile(), n.EnvFile()} {
		info, err := os.Lstat(p)
		if err != nil || info.Mode().Perm()&0o007 == 0 {
			continue
		}
		path := p
		out = append(out, nativeFile{
			problem: path + " holds your instance's secrets, and every account on this machine can read it.",
			fix: host.Step{
				Do:      "Lets only root and " + native.ServiceUser + " read " + path + ".",
				Command: "chmod o-rwx " + path,
				Run:     func(context.Context) error { return os.Chmod(path, info.Mode().Perm()&^0o007) },
			},
		})
	}
	if dir := n.RuntimeDir(); dir != "" {
		if info, err := os.Stat(dir); err == nil && info.Mode().Perm()&0o002 != 0 {
			out = append(out, nativeFile{
				problem: "Every account on this machine can change " + dir + ", where Fluxer keeps its control sockets.",
				fix: host.Step{
					Do:      "Stops other accounts from changing " + dir + ".",
					Command: "chmod o-w " + dir,
					Run:     func(context.Context) error { return os.Chmod(dir, info.Mode().Perm()&^0o002) },
				},
			})
		}
	}
	return out
}

func runNativeFiles(ctx context.Context, e *Env) Result {
	if e.Instance == nil {
		return skip("")
	}
	files := e.nativeFiles()
	switch len(files) {
	case 0:
		return pass("Fluxer's files have the right owners and permissions.")
	case 1:
		return fixable(fail(files[0].problem, "Run rexctl doctor --fix to fix it."))
	}
	r := fixable(fail(fmt.Sprintf("%d of Fluxer's files have the wrong owner or permissions.", len(files)), "Run rexctl doctor --fix to fix them."))
	for _, f := range files {
		r.Details = append(r.Details, f.problem)
	}
	return r
}

func fixNativeFiles(ctx context.Context, e *Env) error {
	var steps []host.Step
	for _, f := range e.nativeFiles() {
		steps = append(steps, f.fix)
	}
	return e.run(ctx, steps...)
}
