// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"
	"os/exec"
	"os/user"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/setup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const nativeInstallFix = "Run rexctl install --runtime docker to use one container, or try again after the next Fluxer release."

var nativeComposeFlags = []string{"install-docker", "setup-podman"}

func (in *installer) nativeFlags(ctx context.Context) error {
	if err := in.app.nativeRoot(""); err != nil {
		return err
	}
	for _, name := range nativeComposeFlags {
		if in.c.Flags().Changed(name) {
			return ui.Usage(fmt.Sprintf("--runtime systemd doesn't take --%s.", name))
		}
	}
	return rt.NativeAlone(ctx)
}

func (in *installer) nativeTarget() error {
	dir := in.dir
	if native.HoldsReleases(dir) && !(in.f.ForMigrate && native.HoldsOnlyDownload(dir, stack.EmbeddedRelease())) {
		return ui.Refused(fmt.Sprintf("%s already holds a Fluxer that runs as %s.", dir, host.StandaloneNativeUnit), "Pass --dir with another folder, or keep using that install.")
	}
	if err := nativeFolder(dir, "--dir"); err != nil {
		return err
	}
	in.dir = instance.RealPath(dir)
	return nil
}

func (in *installer) nativeMachine(ctx context.Context, c *ui.Checklist) {
	in.checkRows(ctx, c, "memory", "disk")
	in.ran["engine"], in.ran["podman-subids"], in.ran["windows-linux-containers"] = true, true, true
	if !nativeOS(in.host) {
		in.failed = ui.Refused("Fluxer runs as a systemd service on Debian 12 or later, or Ubuntu 22.04 or later.", "Run rexctl install --runtime docker, or use one of those releases.")
		c.Check("system", "System", func(context.Context) (ui.State, string, error) {
			return ui.StateFail, "a systemd service needs Debian 12 or later, or Ubuntu 22.04 or later", nil
		})
		return
	}
	c.Check("release", "Release", func(ctx context.Context) (ui.State, string, error) {
		rel, err := nativeRelease(ctx, stack.EmbeddedRelease(), nativeInstallFix)
		if err != nil {
			if in.failed == nil {
				in.failed = err
			}
			return ui.StateFail, ui.Sentence(err.Error()), nil
		}
		in.rel = rel
		return ui.StateOK, "Fluxer " + rel.Version + " has a native build for this machine", nil
	})
	c.Check("apt", "Packages", func(ctx context.Context) (ui.State, string, error) {
		if _, err := exec.LookPath("apt-get"); err != nil {
			if in.failed == nil {
				in.failed = rt.NativeError(native.ErrNoApt, "")
			}
			return ui.StateFail, "this machine has no apt", nil
		}
		if major := native.ExpectedPostgres(ctx); major > 0 {
			in.postgres = major
			return ui.StateOK, fmt.Sprintf("apt installs what Fluxer needs, with PostgreSQL %d", major), nil
		}
		return ui.StateOK, "apt installs what Fluxer needs", nil
	})
	if in.failed != nil {
		return
	}
	in.nativePlan()
}

func (in *installer) nativePlan() {
	switch here := native.PostgresHere(); {
	case here > 0:
		in.change(ui.PlanRun, fmt.Sprintf("Install the system packages Fluxer needs with apt, and use the PostgreSQL %d on this machine", here), nil)
	case in.postgres > 0:
		in.change(ui.PlanRun, fmt.Sprintf("Install the system packages Fluxer needs, like PostgreSQL %d, with apt", in.postgres), nil)
	default:
		in.change(ui.PlanRun, "Install the system packages Fluxer needs, like PostgreSQL, with apt", nil)
	}
	what := "Add the " + native.ServiceUser + " account and " + native.UnitName(in.f.Project)
	if _, err := user.Lookup(native.ServiceUser); err == nil {
		what = "Add " + native.UnitName(in.f.Project)
	}
	in.change(ui.PlanCreate, what, nil)
	in.change(ui.PlanChange, "Stop postgresql-common from making its default cluster", nil)
}

func (in *installer) nativeTasks() []ui.Task {
	last := ui.Task{ID: "start", Title: "Starting Fluxer", Run: in.nativeStart}
	if in.f.NoStart {
		last = ui.Task{ID: "settings", Title: "Writing Fluxer's settings", Run: in.nativePrepare}
	}
	return []ui.Task{
		{ID: "download", Title: "Downloading Fluxer", Run: in.nativeDownload},
		{ID: "packages", Title: "Installing the system packages Fluxer needs", Run: in.nativePackages},
		last,
	}
}

func (in *installer) nativeDownload(ctx context.Context, s *ui.Step) error {
	n := native.New(in.inst)
	if _, err := n.Fetch(ctx, in.rel, s.Progress); err != nil {
		return rt.NativeError(err, "Couldn't download Fluxer.")
	}
	if err := n.Flip(in.rel.Version); err != nil {
		return rt.NativeError(err, "Couldn't unpack Fluxer.")
	}
	if _, err := n.WriteSettings(in.rel.Version); err != nil {
		return rt.NativeError(err, "Couldn't read the settings of Fluxer "+in.rel.Version+".")
	}
	s.Done("Downloaded Fluxer %s.", in.rel.Version)
	return nil
}

func (in *installer) nativePackages(ctx context.Context, s *ui.Step) error {
	stopped := func() error { return ui.Interrupted(in.resumeHint()) }
	if dpkgInterrupted(ctx) {
		s.Title("Finishing package changes an earlier run left halfway")
		if err := repairDpkg(context.WithoutCancel(ctx), s, in.resumeHint()); err != nil {
			return err
		}
		s.Title("Installing the system packages Fluxer needs")
	}
	if ctx.Err() != nil {
		return stopped()
	}
	n := native.New(in.inst)
	if err := n.EnsurePackages(context.WithoutCancel(ctx), in.rel.Version, s); err != nil {
		return rt.NativeError(err, "Couldn't install the system packages Fluxer needs.")
	}
	if ctx.Err() != nil {
		return stopped()
	}
	if err := native.EnsureUser(context.WithoutCancel(ctx)); err != nil {
		return rt.NativeError(err, "Couldn't create the account "+native.ServiceUser+".")
	}
	if ctx.Err() != nil {
		return stopped()
	}
	s.Done("Installed the system packages Fluxer needs.")
	return nil
}

func (in *installer) nativeStart(ctx context.Context, s *ui.Step) error {
	inst := in.inst
	in.app.remember(inst.Dir)
	set, err := deploy.ReleaseSet(inst)
	if err != nil {
		return err
	}
	n := native.New(inst)
	if _, err := rt.ApplyNative(ctx, inst, n, set, s.Warn, native.ApplyChanges); err != nil {
		return err
	}
	if err := rt.NativeWait(ctx, n, s, "Starting"); err != nil {
		return err
	}
	if !in.net.boot {
		if err := n.Disable(ctx); err != nil {
			s.Warn("Fluxer still starts when this machine starts.", "Run rexctl boot off to change that.")
		}
	}
	inst.State.Stopped = false
	inst.State.Installing = false
	inst.State.BootService = bootValue(in.net.boot)
	if err := saveInstance(inst); err != nil {
		return err
	}
	s.Done("Started Fluxer.")
	return nil
}

func (in *installer) nativePrepare(ctx context.Context, s *ui.Step) error {
	set, err := deploy.ReleaseSet(in.inst)
	if err != nil {
		return err
	}
	if err := native.New(in.inst).Prepare(ctx, set); err != nil {
		return rt.NativeError(err, "Couldn't write Fluxer's settings.")
	}
	if err := saveInstance(in.inst); err != nil {
		return err
	}
	s.Done("Wrote Fluxer's settings.")
	return nil
}

func bootValue(on bool) string {
	if on {
		return instance.BootServiceOn
	}
	return instance.BootServiceOff
}

func (in *installer) writeNative(inst *instance.Instance) error {
	st := inst.State
	st.Layout = instance.LayoutFHS
	loopback, err := pickLoopback(inst.Dir)
	if err != nil {
		return ui.Wrap(err, "Couldn't find a free internal address for a second Fluxer on this machine.", "Remove a Fluxer you no longer use, then try again.")
	}
	st.Loopback = loopback
	set := settings.NativeSet(in.rel.Version)
	for k, v := range native.Pinned {
		switch {
		case k == settings.StorageBackend && in.storage != nil:
		case k == settings.LiveKitEnabled && st.Settings[settings.VoiceBundled] == "false":
		default:
			st.Settings[k] = v
		}
	}
	if _, err := secrets.Mint(st.Secrets, false); err != nil {
		return ui.Wrap(err, "Couldn't create your instance's secrets.", thenAgain("", in.resumeHint()))
	}
	if _, err := setup.EnsureToken(st.Secrets); err != nil {
		return ui.Wrap(err, "Couldn't create a setup token.", thenAgain("", in.resumeHint()))
	}
	st.Release = set.Release
	if err := release.WriteRelease(inst, set); err != nil {
		return saveFailed(inst, err, fmt.Sprintf("Couldn't write Fluxer's files to %s.", inst.Dir), in.resumeHint())
	}
	if err := inst.Save(); err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't save your settings in %s.", inst.Dir), thenAgain("Check that you can write to it", in.resumeHint()))
	}
	in.nativeSizing(inst, set)
	return nil
}

var nativeSizingRows = []struct{ label, key string }{
	{"Rust service threads", stack.RustWorkerThreadsVar},
	{"Media proxy threads", "FLUXER_MEDIA_PROXY_TOKIO_WORKER_THREADS"},
	{"API database connections", "FLUXER_API_POSTGRES_MAX_CONNECTIONS"},
	{"Cache database connections", stack.KVPoolVar},
	{"Cache size limit", stack.KVMaxBytesVar},
	{"Postgres connections", "FLUXER_POSTGRES_SERVER_MAX_CONNECTIONS"},
	{"Postgres shared buffers", "FLUXER_POSTGRES_SHARED_BUFFERS"},
	{"Postgres work memory", "FLUXER_POSTGRES_WORK_MEM"},
	{"Search indexing threads", "FLUXER_MEILISEARCH_MAX_INDEXING_THREADS"},
	{"Search indexing memory", "FLUXER_MEILISEARCH_MAX_INDEXING_MEMORY"},
}

func (in *installer) nativeSizing(inst *instance.Instance, set *stack.Set) {
	u := in.app.UI
	if !u.Verbose() {
		return
	}
	env, _, err := native.New(inst).Environment(set)
	if err != nil {
		u.Detail("%v", err)
		return
	}
	for _, r := range nativeSizingRows {
		v := strings.TrimSpace(env[r.key])
		if v == "" {
			continue
		}
		if r.key == stack.KVMaxBytesVar {
			if n, err := strconv.ParseInt(v, 10, 64); err == nil {
				v = ui.Bytes(n)
			}
		}
		u.Detail("%-27s %s", r.label, v)
	}
}

func sizingLine(profile string) string {
	hw := settings.Hardware()
	if profile != "" {
		return fmt.Sprintf("Sized for %s with the %s memory profile", plural(hw.CPUs, "CPU", "CPUs"), profile)
	}
	return fmt.Sprintf("Sized for %s and %s of memory", plural(hw.CPUs, "CPU", "CPUs"), ui.Memory(int64(hw.MemoryBytes)))
}

func repairDpkg(ctx context.Context, s *ui.Step, resume string) error {
	cmd := exec.CommandContext(ctx, "dpkg", "--configure", "-a")
	cmd.Env = append(cmd.Environ(), "DEBIAN_FRONTEND=noninteractive")
	ownProcessGroup(cmd)
	out, err := cmd.CombinedOutput()
	for _, line := range strings.Split(strings.TrimSpace(string(out)), "\n") {
		if line != "" {
			s.Log(line)
		}
	}
	if err != nil {
		return ui.Wrap(err, "Couldn't finish the package changes an earlier run left halfway.", thenAgain("Run sudo dpkg --configure -a", resume))
	}
	return nil
}
