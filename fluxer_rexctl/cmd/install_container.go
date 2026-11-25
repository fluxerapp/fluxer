// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"
	"strconv"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
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

func (in *installer) containerMachine(ctx context.Context, c *ui.Checklist) {
	in.checkRows(ctx, c, "memory", "disk")
	want := engine.Kind(in.f.Engine)
	c.Check("engine", want.Name(), func(ctx context.Context) (ui.State, string, error) {
		return in.findEngine(ctx)
	})
	if in.failed != nil {
		return
	}
	c.Check("release", "Release", func(ctx context.Context) (ui.State, string, error) {
		rel, err := resolveRelease(ctx, stack.EmbeddedRelease())
		if err != nil {
			if in.failed == nil {
				in.failed = err
			}
			return ui.StateFail, ui.Sentence(err.Error()), nil
		}
		in.rel = rel
		return ui.StateOK, "Fluxer " + rel.Version + " runs from " + rt.ImageRef(rel.Version), nil
	})
	if in.engine == nil {
		in.ran["engine"], in.ran["podman-subids"], in.ran["windows-linux-containers"] = true, true, true
		return
	}
	in.checkRows(ctx, c, "engine", "podman-subids", "windows-linux-containers")
	c.Check("container", "Container", func(ctx context.Context) (ui.State, string, error) {
		return in.containerFree(ctx)
	})
	if t := in.podmanSetup(ctx); t != nil {
		in.change(ui.PlanChange, "Make Podman start with this machine and keep health checks out of its log", t)
	}
}

func (in *installer) containerFree(ctx context.Context) (ui.State, string, error) {
	for n := 2; ; n++ {
		probe := &instance.Instance{Dir: in.dir, State: &instance.State{Project: in.f.Project}}
		name, volume := rt.ContainerName(probe), rt.VolumeName(probe)
		_, nameUsed, err := in.engine.Inspect(ctx, name)
		if err != nil {
			return ui.StateFail, "", err
		}
		volumeTaken, err := in.engine.VolumeExists(ctx, volume)
		if err != nil {
			return ui.StateFail, "", err
		}
		switch {
		case !nameUsed && !volumeTaken:
			return ui.StateOK, "Fluxer runs as " + name + " with its data in the volume " + volume, nil
		case !in.c.Flags().Changed("project") && n < 100 && nameTaken(in.f.Project, in.dir, false):
			in.f.Project = defaultProject + strconv.Itoa(n)
		case nameUsed:
			in.failed = ui.Refused(fmt.Sprintf("A container called %s already runs on this machine.", name), "Pass --project with another name, such as "+in.f.Project+"2, to run a second Fluxer next to it.")
			return ui.StateFail, name + " is taken", nil
		default:
			in.failed = ui.Refused(fmt.Sprintf("The volume %s already holds data on this machine, and rexctl won't start a new Fluxer on it.", volume), "Pass --project with another name, or restore that data with rexctl restore once Fluxer runs.")
			return ui.StateFail, volume + " is taken", nil
		}
	}
}

func (in *installer) containerTasks() []ui.Task {
	last := ui.Task{ID: "start", Title: "Starting Fluxer", Run: in.containerStart}
	if in.f.NoStart {
		last = ui.Task{ID: "settings", Title: "Writing Fluxer's settings", Run: in.containerPrepare}
	}
	return []ui.Task{
		{ID: "download", Title: "Downloading Fluxer", Run: in.containerDownload},
		last,
	}
}

func (in *installer) containerRuntime(ctx context.Context) (rt.Runtime, error) {
	return in.app.Runtime(ctx, in.inst)
}

func (in *installer) containerDownload(ctx context.Context, s *ui.Step) error {
	r, err := in.containerRuntime(ctx)
	if err != nil {
		return err
	}
	if err := rt.PullImage(ctx, r, s, in.rel.Version); err != nil {
		return err
	}
	s.Done("Downloaded Fluxer %s.", in.rel.Version)
	return nil
}

func (in *installer) containerStart(ctx context.Context, s *ui.Step) error {
	inst := in.inst
	in.app.remember(inst.Dir)
	r, err := in.containerRuntime(ctx)
	if err != nil {
		return err
	}
	if err := r.Start(ctx, s, rt.StartOptions{}); err != nil {
		return err
	}
	inst.State.Stopped = false
	inst.State.Installing = false
	if err := saveInstance(inst); err != nil {
		return err
	}
	s.Done("Started Fluxer.")
	return nil
}

func (in *installer) containerPrepare(ctx context.Context, s *ui.Step) error {
	r, err := in.containerRuntime(ctx)
	if err != nil {
		return err
	}
	if err := rt.PrepareContainer(ctx, r); err != nil {
		return err
	}
	inst := in.inst
	inst.State.Stopped = true
	if err := saveInstance(inst); err != nil {
		return err
	}
	s.Done("Wrote Fluxer's settings.")
	return nil
}

func (in *installer) writeContainer(inst *instance.Instance) error {
	st := inst.State
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
	st.Engine = instance.EngineRef{Kind: string(in.engine.Kind), Host: in.engine.Host, ID: in.engine.ID}
	st.Release = set.Release
	if err := release.WriteRelease(inst, set); err != nil {
		return saveFailed(inst, err, fmt.Sprintf("Couldn't write Fluxer's files to %s.", inst.Dir), in.resumeHint())
	}
	if err := inst.Save(); err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't save your settings in %s.", inst.Dir), thenAgain("Check that you can write to it", in.resumeHint()))
	}
	return nil
}
