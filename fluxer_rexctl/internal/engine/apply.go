// SPDX-License-Identifier: AGPL-3.0-or-later

package engine

import (
	"context"
	"encoding/hex"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"time"

	"github.com/compose-spec/compose-go/v2/types"
	cerrdefs "github.com/containerd/errdefs"
	"github.com/docker/compose/v5/pkg/api"
	"github.com/moby/moby/api/types/container"
	"github.com/moby/moby/api/types/mount"
	"github.com/moby/moby/client"
)

func recreatePolicy(r Recreate) string {
	switch r {
	case RecreateAlways:
		return api.RecreateForce
	case RecreateNever:
		return api.RecreateNever
	}
	return api.RecreateDiverged
}

func (p *Project) Up(ctx context.Context, o UpOptions) error {
	if err := p.needModel("up"); err != nil {
		return err
	}
	if err := p.Engine.refuseTooOld("up"); err != nil {
		return err
	}
	c, err := p.Engine.conn()
	if err != nil {
		return err
	}
	keep := context.WithoutCancel(ctx)
	if err := p.finishStop(keep, c); err != nil {
		return err
	}
	plan, err := p.Plan(keep)
	if err != nil {
		return err
	}
	if err := p.pullMissing(keep, o.OnEvent); err != nil {
		return err
	}
	stop := c.bus.listen(p.Name, o.OnEvent)
	defer stop()
	model := copyModel(p.Model)
	if len(o.Services) == 0 {
		if len(plan.Remove) > 0 {
			if err := p.removeServices(keep, c, plan.Remove); err != nil {
				return err
			}
		}
		if hashless, err := p.hashlessNetworks(keep, plan.Networks); err != nil {
			return err
		} else if len(hashless) > 0 {
			detailf("engine: network %v has no config hash, taking the project down to recreate it", hashless)
			if err := c.svc.Down(keep, p.Name, api.DownOptions{Project: model}); err != nil {
				return p.wrap("up", err)
			}
		}
		for _, key := range plan.LabelOnly {
			n := model.Networks[key]
			n.External = true
			model.Networks[key] = n
		}
	} else {
		var opts []types.DependencyOption
		if o.NoDeps {
			opts = append(opts, types.IgnoreDependencies)
		}
		model, err = model.WithSelectedServices(o.Services, opts...)
		if err != nil {
			return &Error{Op: "up", Class: ClassInvalidConfig, Err: err}
		}
	}
	if o.RemoveOrphans {
		if err := p.removeOwnedOrphans(keep); err != nil {
			return err
		}
	}
	policy := recreatePolicy(o.Recreate)
	create := api.CreateOptions{
		Recreate:             policy,
		RecreateDependencies: policy,
		Inherit:              true,
		QuietPull:            true,
	}
	err = c.svc.Create(keep, model, create)
	if imageGone(err) {
		if err := p.pullMissing(keep, o.OnEvent); err != nil {
			return err
		}
		err = c.svc.Create(keep, model, create)
	}
	if err != nil {
		return p.wrap("up", err)
	}
	return p.wrap("up", c.svc.Start(ctx, p.Name, api.StartOptions{Project: model, Wait: o.Wait, WaitTimeout: o.WaitTimeout}))
}

func (p *Project) hashlessNetworks(ctx context.Context, keys []string) ([]string, error) {
	if len(keys) == 0 {
		return nil, nil
	}
	c, err := p.Engine.api()
	if err != nil {
		return nil, err
	}
	res, err := c.NetworkList(ctx, client.NetworkListOptions{Filters: projectFilter(p.Name)})
	if err != nil {
		return nil, p.wrap("up", err)
	}
	var out []string
	for _, n := range res.Items {
		if slices.Contains(keys, n.Labels[api.NetworkLabel]) && n.Labels[api.ConfigHashLabel] == "" {
			out = append(out, n.Labels[api.NetworkLabel])
		}
	}
	return out, nil
}

func (p *Project) removeServices(ctx context.Context, c *conn, services []string) error {
	err := c.svc.Remove(ctx, p.Name, api.RemoveOptions{Services: services, Stop: true, Force: true, Volumes: true})
	if err != nil && !errors.Is(err, api.ErrNoResources) {
		return p.wrap("remove", err)
	}
	return nil
}

func (p *Project) Create(ctx context.Context) error {
	if err := p.needModel("create"); err != nil {
		return err
	}
	if err := p.Engine.refuseTooOld("create"); err != nil {
		return err
	}
	c, err := p.Engine.conn()
	if err != nil {
		return err
	}
	if err := p.pullMissing(ctx, nil); err != nil {
		return err
	}
	if err := p.removeOwnedOrphans(ctx); err != nil {
		return err
	}
	opts := api.CreateOptions{
		Recreate:             api.RecreateDiverged,
		RecreateDependencies: api.RecreateDiverged,
		Inherit:              true,
		QuietPull:            true,
	}
	err = c.svc.Create(ctx, copyModel(p.Model), opts)
	if imageGone(err) {
		if err := p.pullMissing(ctx, nil); err != nil {
			return err
		}
		err = c.svc.Create(ctx, copyModel(p.Model), opts)
	}
	return p.wrap("create", err)
}

func (p *Project) StartWatched(ctx context.Context, onEvent func(Event), services ...string) error {
	c, err := p.Engine.conn()
	if err != nil {
		return err
	}
	stop := c.bus.listen(p.Name, onEvent)
	defer stop()
	return p.Start(ctx, services...)
}

func (p *Project) Start(ctx context.Context, services ...string) error {
	c, err := p.Engine.conn()
	if err != nil {
		return err
	}
	if err := p.finishStop(ctx, c); err != nil {
		return err
	}
	if p.Model == nil && len(services) > 0 {
		return p.startNamed(ctx, c, services)
	}
	var model *types.Project
	if p.Model != nil {
		model = copyModel(p.Model)
		if len(services) > 0 {
			if model, err = model.WithSelectedServices(services); err != nil {
				return &Error{Op: "start", Class: ClassInvalidConfig, Err: err}
			}
		}
	}
	return p.wrap("start", c.svc.Start(ctx, p.Name, api.StartOptions{Project: model}))
}

func (p *Project) startNamed(ctx context.Context, c *conn, services []string) error {
	containers, err := p.PS(ctx)
	if err != nil {
		return err
	}
	if composeCanStart(containers, services) {
		return p.wrap("start", c.svc.Start(ctx, p.Name, api.StartOptions{AttachTo: services}))
	}
	cli := c.cli.Client()
	for _, ct := range containers {
		if !slices.Contains(services, ct.Service) || ct.State == "running" {
			continue
		}
		if _, err := cli.ContainerStart(ctx, ct.ID, client.ContainerStartOptions{}); err != nil {
			return p.wrap("start", err)
		}
	}
	return nil
}

func composeCanStart(containers []Container, services []string) bool {
	byService := make(map[string]Container, len(containers))
	for _, ct := range containers {
		byService[ct.Service] = ct
	}
	pending := slices.Clone(services)
	seen := map[string]bool{}
	for len(pending) > 0 {
		name := pending[len(pending)-1]
		pending = pending[:len(pending)-1]
		if seen[name] {
			continue
		}
		seen[name] = true
		ct, ok := byService[name]
		if !ok || ct.Labels[api.ConfigHashLabel] == "" {
			return false
		}
		for dep := range strings.SplitSeq(ct.Labels[api.DependenciesLabel], ",") {
			if dep, _, _ = strings.Cut(dep, ":"); dep != "" {
				pending = append(pending, dep)
			}
		}
	}
	return true
}

func (p *Project) present(ctx context.Context, op string, services []string) error {
	containers, err := p.PS(ctx)
	if err != nil {
		return err
	}
	for _, c := range containers {
		if len(services) == 0 || slices.Contains(services, c.Service) {
			return nil
		}
	}
	return &Error{Op: op, Class: ClassOther, Err: errNoContainers}
}

func (p *Project) Stop(ctx context.Context, services ...string) error {
	return p.stop(ctx, nil, services)
}

func (p *Project) StopWithin(ctx context.Context, timeout time.Duration) error {
	return p.stop(ctx, &timeout, nil)
}

func (p *Project) stop(ctx context.Context, timeout *time.Duration, services []string) error {
	c, err := p.Engine.conn()
	if err != nil {
		return err
	}
	if err := p.present(ctx, "stop", services); errors.Is(err, errNoContainers) {
		return nil
	} else if err != nil {
		return err
	}
	if marker := p.stopMarker(); marker != "" && p.writeStopMarker(services) == nil {
		defer os.Remove(marker)
	}
	if err := c.svc.Stop(ctx, p.Name, api.StopOptions{Project: p.modelOrNil(), Services: services, Timeout: timeout}); err != nil {
		return p.wrap("stop", err)
	}
	return p.stopPodmanCompose(ctx, services)
}

const staleStopMarker = 5 * time.Minute

func (p *Project) stopMarker() string {
	if p.Dir == "" {
		return ""
	}
	dir := filepath.Join(p.Dir, ".rexctl")
	if info, err := os.Stat(dir); err != nil || !info.IsDir() {
		return ""
	}
	return filepath.Join(dir, "stopping")
}

func (p *Project) writeStopMarker(services []string) error {
	root, err := os.OpenRoot(p.Dir)
	if err != nil {
		return err
	}
	defer root.Close()
	f, err := root.OpenFile(filepath.Join(".rexctl", "stopping"), os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o600)
	if err != nil {
		return err
	}
	_, err = f.WriteString(strings.Join(services, "\n"))
	if cerr := f.Close(); err == nil {
		err = cerr
	}
	return err
}

func (p *Project) finishStop(ctx context.Context, c *conn) error {
	marker := p.stopMarker()
	if marker == "" {
		return nil
	}
	info, err := os.Stat(marker)
	if err != nil {
		return nil
	}
	if time.Since(info.ModTime()) < staleStopMarker {
		data, err := os.ReadFile(marker)
		if err != nil {
			return nil
		}
		services := strings.Fields(string(data))
		detailf("engine: an earlier stop of %v was interrupted, finishing it", services)
		if err := p.present(ctx, "stop", services); errors.Is(err, errNoContainers) {
			_ = os.Remove(marker)
			return nil
		} else if err != nil {
			return err
		}
		if err := c.svc.Stop(ctx, p.Name, api.StopOptions{Project: p.modelOrNil(), Services: services}); err != nil {
			return p.wrap("stop", err)
		}
	}
	_ = os.Remove(marker)
	return nil
}

func (p *Project) Restart(ctx context.Context, services ...string) error {
	return p.RestartWatched(ctx, nil, services...)
}

func (p *Project) RestartWatched(ctx context.Context, onEvent func(Event), services ...string) error {
	if err := p.Engine.refuseTooOld("restart"); err != nil {
		return err
	}
	c, err := p.Engine.conn()
	if err != nil {
		return err
	}
	if err := p.present(ctx, "restart", services); err != nil {
		return err
	}
	stop := c.bus.listen(p.Name, onEvent)
	defer stop()
	return p.wrap("restart", c.svc.Restart(ctx, p.Name, api.RestartOptions{Project: p.modelOrNil(), Services: services}))
}

func (p *Project) Remove(ctx context.Context, services ...string) error {
	c, err := p.Engine.conn()
	if err != nil {
		return err
	}
	return p.removeServices(ctx, c, services)
}

func (p *Project) Down(ctx context.Context, volumes bool) error {
	_, err := p.down(ctx, volumes, nil)
	return err
}

func (p *Project) DownReport(ctx context.Context, volumes bool) (DownResult, error) {
	return p.down(ctx, volumes, nil)
}

func (p *Project) DownWithin(ctx context.Context, volumes bool, timeout time.Duration) error {
	_, err := p.down(ctx, volumes, &timeout)
	return err
}

func (p *Project) down(ctx context.Context, volumes bool, timeout *time.Duration) (DownResult, error) {
	var res DownResult
	c, err := p.Engine.conn()
	if err != nil {
		return res, err
	}
	a, err := p.Engine.api()
	if err != nil {
		return res, err
	}
	if err := p.removeHelpers(ctx, a); err != nil {
		return res, err
	}
	list, err := a.ContainerList(ctx, client.ContainerListOptions{All: true, Filters: projectFilter(p.Name)})
	if err != nil {
		return res, p.wrap("down", err)
	}
	owned, foreign := p.split(list.Items)
	var anonymous []string
	if volumes {
		anonymous = anonymousVolumes(owned)
	}
	shared := foreignNetworks(foreign)
	if p.Model != nil {
		model := withoutNetworks(withoutServices(copyModel(p.Model), summaryServices(foreign)), shared)
		if err := p.wrap("down", c.svc.Down(ctx, p.Name, api.DownOptions{Project: model, Volumes: volumes, Timeout: timeout})); err != nil {
			return res, err
		}
	}
	if list, err = a.ContainerList(ctx, client.ContainerListOptions{All: true, Filters: projectFilter(p.Name)}); err != nil {
		return res, p.wrap("down", err)
	}
	owned, foreign = p.split(list.Items)
	for _, n := range foreignNetworks(foreign) {
		if !slices.Contains(shared, n) {
			shared = append(shared, n)
		}
	}
	if err := removeContainers(ctx, a, owned, timeout); err != nil {
		return res, p.wrap("down", err)
	}
	res.Foreign = containerNames(foreign)
	res.dirs = workingDirs(foreign)
	if p.Model == nil && volumes {
		named, err := p.prefixedVolumes(ctx, a)
		if err != nil {
			return res, p.wrap("down", err)
		}
		anonymous = append(anonymous, named...)
	}
	if p.Model == nil || volumes {
		return res, p.wrap("down", p.removeLeftovers(ctx, a, anonymous, shared))
	}
	return res, nil
}

func (p *Project) removeOwnedOrphans(ctx context.Context) error {
	a, err := p.Engine.api()
	if err != nil {
		return err
	}
	list, err := a.ContainerList(ctx, client.ContainerListOptions{All: true, Filters: projectFilter(p.Name)})
	if err != nil {
		return p.wrap("up", err)
	}
	owned, _ := p.split(list.Items)
	orphans := slices.DeleteFunc(owned, func(ct container.Summary) bool {
		svc, ok := p.Model.Services[ct.Labels[api.ServiceLabel]]
		_, disabled := p.Model.DisabledServices[ct.Labels[api.ServiceLabel]]
		return ct.Labels[api.OneoffLabel] == "True" || disabled || ok && svc.GetScale() > 0
	})
	return p.wrap("up", removeContainers(ctx, a, orphans, nil))
}

func (p *Project) split(items []container.Summary) (owned, foreign []container.Summary) {
	for _, ct := range items {
		if p.owns(ct.Labels) {
			owned = append(owned, ct)
		} else {
			foreign = append(foreign, ct)
		}
	}
	return owned, foreign
}

func (p *Project) owns(labels map[string]string) bool {
	wd := labels[api.WorkingDirLabel]
	if p.Dir == "" {
		return wd == "" || labels[ManagedByLabel] != "" || insideManaged(wd)
	}
	if wd != "" {
		return within(p.Dir, wd)
	}
	if files := labels[api.ConfigFilesLabel]; files != "" {
		for _, f := range strings.Split(files, ",") {
			if !within(p.Dir, f) {
				return false
			}
		}
		return true
	}
	return labels[ManagedByLabel] != ""
}

func (p *Project) elsewhere(labels map[string]string) bool {
	wd := labels[api.WorkingDirLabel]
	return p.Dir != "" && wd != "" && !within(p.Dir, wd)
}

func within(dir, path string) bool {
	if inside(dir, path) {
		return true
	}
	rd, err1 := filepath.EvalSymlinks(dir)
	rp, err2 := filepath.EvalSymlinks(path)
	return err1 == nil && err2 == nil && inside(rd, rp)
}

func inside(dir, path string) bool {
	rel, err := filepath.Rel(filepath.Clean(dir), filepath.Clean(path))
	return err == nil && !filepath.IsAbs(rel) && rel != ".." && !strings.HasPrefix(rel, ".."+string(filepath.Separator))
}

func insideManaged(dir string) bool {
	for d := filepath.Clean(dir); filepath.IsAbs(d); d = filepath.Dir(d) {
		if _, err := os.Stat(filepath.Join(d, managedStateFile)); err == nil {
			return true
		}
		if d == filepath.Dir(d) {
			return false
		}
	}
	return false
}

func summaryServices(cs []container.Summary) []string {
	var out []string
	for _, ct := range cs {
		if s := ct.Labels[api.ServiceLabel]; s != "" && !slices.Contains(out, s) {
			out = append(out, s)
		}
	}
	return out
}

func withoutServices(model *types.Project, names []string) *types.Project {
	for _, name := range names {
		delete(model.Services, name)
	}
	for key, svc := range model.Services {
		for _, name := range names {
			delete(svc.DependsOn, name)
		}
		model.Services[key] = svc
	}
	return model
}

func foreignNetworks(cs []container.Summary) []string {
	var out []string
	for _, ct := range cs {
		if ct.NetworkSettings == nil {
			continue
		}
		for name := range ct.NetworkSettings.Networks {
			if !slices.Contains(out, name) {
				out = append(out, name)
			}
		}
	}
	return out
}

func withoutNetworks(model *types.Project, names []string) *types.Project {
	for key, n := range model.Networks {
		if slices.Contains(names, n.Name) {
			n.External = true
			model.Networks[key] = n
		}
	}
	return model
}

func (r DownResult) ForeignOutside(dir string) []string {
	return slices.DeleteFunc(slices.Clone(r.Foreign), func(name string) bool {
		wd := r.dirs[name]
		return wd != "" && within(dir, wd)
	})
}

func workingDirs(cs []container.Summary) map[string]string {
	out := map[string]string{}
	for _, ct := range cs {
		name := ct.ID
		if len(ct.Names) > 0 {
			name = strings.TrimPrefix(ct.Names[0], "/")
		}
		out[name] = ct.Labels[api.WorkingDirLabel]
	}
	return out
}

func containerNames(cs []container.Summary) []string {
	var out []string
	for _, ct := range cs {
		name := ct.ID
		if len(ct.Names) > 0 {
			name = strings.TrimPrefix(ct.Names[0], "/")
		}
		out = append(out, name)
	}
	slices.Sort(out)
	return out
}

func removeContainers(ctx context.Context, a client.APIClient, cs []container.Summary, timeout *time.Duration) error {
	var secs *int
	if timeout != nil {
		n := int(timeout.Seconds())
		secs = &n
	}
	for _, ct := range cs {
		if ct.State == container.StateRunning || ct.State == container.StatePaused || ct.State == container.StateRestarting {
			if _, err := a.ContainerStop(ctx, ct.ID, client.ContainerStopOptions{Timeout: secs}); err != nil && !cerrdefs.IsNotFound(err) {
				return err
			}
		}
		if _, err := a.ContainerRemove(ctx, ct.ID, client.ContainerRemoveOptions{Force: true}); err != nil && !cerrdefs.IsNotFound(err) {
			return err
		}
	}
	return nil
}

func (p *Project) prefixedVolumes(ctx context.Context, a client.APIClient) ([]string, error) {
	res, err := a.VolumeList(ctx, client.VolumeListOptions{Filters: projectFilter(p.Name)})
	if err != nil {
		return nil, err
	}
	var out []string
	for _, v := range res.Items {
		if strings.HasPrefix(v.Name, p.Name+"_") {
			out = append(out, v.Name)
		}
	}
	return out, nil
}

func anonymousVolumes(containers []container.Summary) []string {
	var out []string
	for _, c := range containers {
		for _, m := range c.Mounts {
			if m.Type != mount.TypeVolume || len(m.Name) != 64 || slices.Contains(out, m.Name) {
				continue
			}
			if _, err := hex.DecodeString(m.Name); err == nil {
				out = append(out, m.Name)
			}
		}
	}
	slices.Sort(out)
	return out
}

func (p *Project) removeLeftovers(ctx context.Context, a client.APIClient, anonymous, shared []string) error {
	for _, name := range anonymous {
		if _, err := a.VolumeRemove(ctx, name, client.VolumeRemoveOptions{}); err != nil && !cerrdefs.IsNotFound(err) && !cerrdefs.IsConflict(err) {
			return err
		}
	}
	res, err := a.NetworkList(ctx, client.NetworkListOptions{Filters: projectFilter(p.Name)})
	if err != nil {
		return err
	}
	for _, n := range res.Items {
		if slices.Contains(shared, n.Name) {
			continue
		}
		in, err := a.NetworkInspect(ctx, n.ID, client.NetworkInspectOptions{})
		if cerrdefs.IsNotFound(err) {
			continue
		}
		if err != nil {
			return err
		}
		if len(in.Network.Containers) > 0 {
			continue
		}
		if _, err := a.NetworkRemove(ctx, n.ID, client.NetworkRemoveOptions{}); err != nil && !cerrdefs.IsNotFound(err) && !cerrdefs.IsConflict(err) {
			return err
		}
	}
	return nil
}

func (p *Project) DownKeeping(ctx context.Context, keep ...string) error {
	if err := p.Down(ctx, false); err != nil {
		return err
	}
	vols, err := p.Engine.Volumes(ctx, p.Name)
	if err != nil {
		return err
	}
	a, err := p.Engine.api()
	if err != nil {
		return err
	}
	for _, name := range volumesToDrop(p.Model, vols, p.Name, keep) {
		if _, err := a.VolumeRemove(ctx, name, client.VolumeRemoveOptions{}); err != nil && !cerrdefs.IsNotFound(err) {
			return p.wrap("down", err)
		}
	}
	return nil
}

func (e *Engine) RemoveVolume(ctx context.Context, name string) error {
	a, err := e.api()
	if err != nil {
		return err
	}
	if _, err := a.VolumeRemove(ctx, name, client.VolumeRemoveOptions{}); err != nil && !cerrdefs.IsNotFound(err) {
		return &Error{Op: "volume", Class: ClassOf(err), Err: err}
	}
	return nil
}

func volumesToDrop(model *types.Project, vols []Volume, project string, keep []string) []string {
	var kept, out []string
	if model != nil {
		for short, v := range model.Volumes {
			if slices.Contains(keep, short) {
				kept = append(kept, v.Name)
			}
		}
		for short, v := range model.Volumes {
			if !bool(v.External) && v.Name != "" && !slices.Contains(keep, short) && !slices.Contains(kept, v.Name) {
				out = append(out, v.Name)
			}
		}
	}
	for _, v := range vols {
		if v.Project != project || slices.Contains(kept, v.Name) || !strings.HasPrefix(v.Name, project+"_") {
			continue
		}
		short := v.Labels[api.VolumeLabel]
		if short == "" {
			short = strings.TrimPrefix(v.Name, project+"_")
		}
		if !slices.Contains(keep, short) {
			out = append(out, v.Name)
		}
	}
	slices.Sort(out)
	return slices.Compact(out)
}

func (p *Project) modelOrNil() *types.Project {
	if p.Model == nil {
		return nil
	}
	return copyModel(p.Model)
}

func imageGone(err error) bool {
	return err != nil && strings.Contains(err.Error(), "No such image")
}

func (p *Project) wrap(op string, err error) error {
	if err == nil {
		return nil
	}
	var e *Error
	if errors.As(err, &e) {
		return err
	}
	if errors.Is(err, context.Canceled) || errors.Is(err, context.DeadlineExceeded) {
		return err
	}
	return &Error{Op: op, Class: ClassOf(err), Err: err}
}

func (e *Engine) EnsureNetwork(ctx context.Context, name, project, key string, ipv6 bool) error {
	c, err := e.api()
	if err != nil {
		return err
	}
	if _, err := c.NetworkInspect(ctx, name, client.NetworkInspectOptions{}); err == nil {
		return nil
	} else if !cerrdefs.IsNotFound(err) {
		return err
	}
	opts := client.NetworkCreateOptions{
		Driver: "bridge",
		Labels: map[string]string{api.ProjectLabel: project, api.NetworkLabel: key},
	}
	if ipv6 {
		opts.EnableIPv6 = &ipv6
	}
	_, err = c.NetworkCreate(ctx, name, opts)
	if cerrdefs.IsConflict(err) {
		return nil
	}
	return err
}

func (e *Engine) NetworkIPv6(ctx context.Context, name string) (bool, bool, error) {
	c, err := e.api()
	if err != nil {
		return false, false, err
	}
	res, err := c.NetworkInspect(ctx, name, client.NetworkInspectOptions{})
	if cerrdefs.IsNotFound(err) {
		return false, false, nil
	}
	if err != nil {
		return false, false, err
	}
	return true, res.Network.EnableIPv6, nil
}

func (e *Engine) IPv6Voice() bool {
	return e.Kind == Docker && !e.Rootless && AtLeast(e.Version, MinDockerIPv6)
}
