// SPDX-License-Identifier: AGPL-3.0-or-later

package engine

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"time"

	"github.com/compose-spec/compose-go/v2/types"
	"github.com/docker/compose/v5/pkg/api"
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
	policy := recreatePolicy(o.Recreate)
	create := api.CreateOptions{
		Recreate:             policy,
		RecreateDependencies: policy,
		RemoveOrphans:        o.RemoveOrphans,
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
	err := c.svc.Remove(ctx, p.Name, api.RemoveOptions{Services: services, Stop: true, Force: true})
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
	opts := api.CreateOptions{
		Recreate:             api.RecreateDiverged,
		RecreateDependencies: api.RecreateDiverged,
		RemoveOrphans:        true,
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
	return &Error{Op: op, Class: ClassOther, Err: ErrNoContainers}
}

func (p *Project) Stop(ctx context.Context, services ...string) error {
	c, err := p.Engine.conn()
	if err != nil {
		return err
	}
	if err := p.present(ctx, "stop", services); errors.Is(err, ErrNoContainers) {
		return nil
	} else if err != nil {
		return err
	}
	if marker := p.stopMarker(); marker != "" && os.WriteFile(marker, []byte(strings.Join(services, "\n")), 0o600) == nil {
		defer os.Remove(marker)
	}
	if err := c.svc.Stop(ctx, p.Name, api.StopOptions{Project: p.modelOrNil(), Services: services}); err != nil {
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
		if err := p.present(ctx, "stop", services); errors.Is(err, ErrNoContainers) {
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
	c, err := p.Engine.conn()
	if err != nil {
		return err
	}
	a, err := p.Engine.api()
	if err != nil {
		return err
	}
	if err := p.removeHelpers(ctx, a); err != nil {
		return err
	}
	return p.wrap("down", c.svc.Down(ctx, p.Name, api.DownOptions{Project: p.modelOrNil(), RemoveOrphans: true, Volumes: volumes}))
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
