// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"maps"
	"os"
	"slices"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/migrate"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func checkObjectsFS(path string) error {
	if r := checks.ObjectsFS(path); r.Status == checks.Fail {
		return errors.New(r.Message)
	}
	return nil
}

type composeHost struct {
	app     *App
	inst    *instance.Instance
	e       *engine.Engine
	p       *engine.Project
	set     *stack.Set
	objects *engine.Project
	image   string
	pulled  bool
}

func (h *composeHost) Runtime() migrate.Runtime                             { return migrate.Compose }
func (h *composeHost) Dir() string                                          { return h.inst.Dir }
func (h *composeHost) Settings() map[string]string                          { return maps.Clone(h.inst.State.Settings) }
func (h *composeHost) AppProxyURL() string                                  { return composeAppProxyURL }
func (h *composeHost) ObjectsRoot() string                                  { return stack.ObjectsRoot }
func (h *composeHost) FreePorts(context.Context, []int) error               { return nil }
func (h *composeHost) RetireValkey(context.Context, string) (string, error) { return "", nil }
func (h *composeHost) ReviveValkey(context.Context, string) error           { return nil }

func (h *composeHost) Backup(ctx context.Context) (string, error) {
	info, err := backup.Create(ctx, h.inst, h.p, backup.CreateOptions{Reason: "migrate"})
	if err != nil {
		return "", err
	}
	return info.Path, nil
}

func (h *composeHost) present(services []string) []string {
	have := h.p.Services()
	return slices.DeleteFunc(slices.Clone(services), func(s string) bool { return !slices.Contains(have, s) })
}

func (h *composeHost) Stop(ctx context.Context, services []string) error {
	if s := h.present(services); len(s) > 0 {
		return h.p.Stop(ctx, s...)
	}
	return nil
}

func (h *composeHost) Halt(ctx context.Context) error {
	if err := h.e.Named(h.inst.Project(), h.inst.Dir).Stop(ctx); err != nil {
		return err
	}
	h.inst.State.Stopped = true
	return h.inst.Save()
}

func (h *composeHost) Start(ctx context.Context) error { return migrateStart(ctx, h.app, h.inst) }

func (h *composeHost) StartOnly(ctx context.Context, services []string) error {
	return h.p.Up(ctx, engine.UpOptions{Services: h.present(services), NoDeps: true, Wait: true})
}

func (h *composeHost) Remove(ctx context.Context, services []string) error {
	if s := h.present(services); len(s) > 0 {
		return h.p.Remove(ctx, s...)
	}
	return nil
}

func (h *composeHost) Check(ctx context.Context) error {
	return h.p.WaitReady(ctx, engine.DefaultReadyTimeout, nil)
}

func (h *composeHost) Services(ctx context.Context) ([]migrate.Service, error) {
	containers, err := h.p.PS(ctx)
	if err != nil {
		return nil, err
	}
	out := make([]migrate.Service, 0, len(containers))
	for _, c := range containers {
		out = append(out, migrate.Service{Name: c.Service, Running: c.State == "running", Health: c.Health})
	}
	return out, nil
}

func (h *composeHost) Apply(ctx context.Context, changes map[string]string) error {
	st := h.inst.State
	st.Settings = merged(st.Settings, changes)
	r, err := h.inst.Render(h.set, stack.OverlaysFor(st.Settings), st.Images.Refs)
	if err != nil {
		return err
	}
	st.Rendered = r.Hashes
	if err := h.inst.Save(); err != nil {
		return err
	}
	p, err := deploy.Load(ctx, h.inst, h.e)
	if err != nil {
		return err
	}
	h.p, h.objects = p, nil
	return nil
}

func (h *composeHost) Env(changes map[string]string) (map[string]string, error) {
	return deploy.Environment(h.set, h.inst.State, merged(h.inst.State.Settings, changes))
}

func (h *composeHost) CheckTarget(ctx context.Context, changes map[string]string) error {
	p, err := customComposeModel(ctx, h.e, h.inst, merged(h.inst.State.Settings, changes))
	if err != nil {
		return err
	}
	if changes[settings.StorageBackend] == settings.StorageFilesystem && !stack.FilesystemStorage(h.inst.State.Settings) {
		return objectsMountProblem(p.Model)
	}
	return nil
}

func (h *composeHost) dataFolder(what string) string {
	dm, ok := stack.DataMounts[what]
	if !ok || h.p == nil {
		return ""
	}
	return modelBind(h.p.Model, dm.Service, dm.Target)
}

func (h *composeHost) objectsProject(ctx context.Context) (*engine.Project, error) {
	if h.objects != nil {
		return h.objects, nil
	}
	p, err := composeModel(ctx, h.e, h.inst, withOptions(h.inst.State.Settings, migrate.OptStorage))
	if err != nil {
		return nil, err
	}
	h.objects = p
	return p, nil
}

func (h *composeHost) CheckObjects(ctx context.Context) error {
	if err := checkObjectsFS(engine.Stores(h.inst.Dir, h.e, "").Data); err != nil {
		return err
	}
	vols, err := h.e.Volumes(ctx, h.inst.Project())
	if err != nil {
		return err
	}
	if bind := h.dataFolder(stack.SeaweedFSVolume); bind != "" {
		if err := migrate.CheckSpace(bind, engine.Stores(h.inst.Dir, h.e, "").Data); err != nil && !errors.Is(err, os.ErrPermission) {
			return err
		}
		vols = nil
	}
	for _, v := range vols {
		if (v.Labels[composeVolumeLabel] == stack.SeaweedFSVolume || v.Name == h.inst.Project()+"_"+stack.SeaweedFSVolume) && v.Mountpoint != "" {
			if err := migrate.CheckSpace(v.Mountpoint, v.Mountpoint); err != nil && !errors.Is(err, os.ErrPermission) {
				return err
			}
		}
	}
	_, err = h.importerImage(ctx)
	return err
}

func (h *composeHost) importerImage(ctx context.Context) (string, error) {
	if h.image != "" {
		return h.image, nil
	}
	version := stack.EmbeddedRelease()
	ref := rt.ImageRef(version)
	_, had, err := h.e.LocalImage(ctx, ref)
	if err != nil {
		return "", rt.ContainerError(err, "Couldn't check for Fluxer "+version+" on this machine. rexctl didn't change anything.")
	}
	if had {
		err = rt.FetchImage(ctx, h.e, version, nil)
	} else {
		err = h.app.UI.Step(ctx, "Downloading Fluxer "+version+" for the move off SeaweedFS", func(ctx context.Context, s *ui.Step) error {
			if err := rt.FetchImage(ctx, h.e, version, h.app.progress(s, "Downloading")); err != nil {
				return err
			}
			s.Done("Downloaded Fluxer %s.", version)
			return nil
		})
	}
	if err != nil {
		return "", rt.ContainerError(err, "Couldn't download Fluxer "+version+". rexctl didn't change anything.")
	}
	h.image, h.pulled = ref, !had
	return ref, nil
}

func (h *composeHost) dropImporterImage(ctx context.Context) {
	if !h.pulled {
		return
	}
	h.pulled = false
	if err := h.e.RemoveImages(context.WithoutCancel(ctx), []string{h.image}); err != nil {
		h.app.UI.Detail("removing the download of Fluxer %s: %v", stack.EmbeddedRelease(), err)
	}
}

func (h *composeHost) PrepareObjects(ctx context.Context) error {
	p, err := h.objectsProject(ctx)
	if err != nil {
		return err
	}
	svc, ok := p.Model.Services[stack.ObjectsInitService]
	if !ok {
		return ui.Failed(fmt.Sprintf("This Fluxer release has no %s service.", stack.ObjectsInitService), "Run rexctl upgrade, then try again.")
	}
	o := p.HelperFor(stack.ObjectsInitService)
	o.Entrypoint = svc.Entrypoint
	o.User = svc.User
	o.Mounts = append(o.Mounts, engine.Mount{Volume: stack.ObjectsVolume, Target: stack.ObjectsRoot})
	o.Stderr = migrateOutput(h.app)
	return p.Helper(ctx, o)
}

func (h *composeHost) dataVolume(ctx context.Context, what string) (string, error) {
	vols, err := h.e.Volumes(ctx, h.inst.Project())
	if err != nil {
		return "", err
	}
	for _, v := range vols {
		if v.Labels[composeVolumeLabel] == what || v.Name == h.inst.Project()+"_"+what {
			return v.Name, nil
		}
	}
	return "", nil
}

func (h *composeHost) HoldsData(ctx context.Context, what string) (bool, error) {
	if bind := h.dataFolder(what); bind != "" {
		return folderHoldsData(bind), nil
	}
	name, err := h.dataVolume(ctx, what)
	return name != "", err
}

func (h *composeHost) DropData(ctx context.Context, what string) error {
	if h.dataFolder(what) != "" {
		return nil
	}
	name, err := h.dataVolume(ctx, what)
	if err != nil || name == "" {
		return err
	}
	return h.e.RemoveVolume(ctx, name)
}

func (h *composeHost) Importer() migrate.Importer { return composeImporter{h} }

type composeImporter struct{ h *composeHost }

func (c composeImporter) Run(ctx context.Context, j migrate.Job) error {
	imp := migrate.ComposeImporter{Project: c.h.p}
	if j.Objects {
		p := c.h.p
		if !stack.FilesystemStorage(c.h.inst.State.Settings) {
			var err error
			if p, err = c.h.objectsProject(ctx); err != nil {
				return err
			}
		}
		imp.Objects = p
		image, err := c.h.importerImage(ctx)
		if err != nil {
			return err
		}
		imp.Image, imp.Root = image, rt.ImageRoot
	}
	return imp.Run(ctx, j)
}
