// SPDX-License-Identifier: AGPL-3.0-or-later

package engine

import (
	"context"
	"errors"
	"slices"
	"strconv"
	"strings"
	"time"

	cerrdefs "github.com/containerd/errdefs"
	"github.com/distribution/reference"
	"github.com/docker/compose/v5/pkg/api"
	"github.com/moby/moby/api/types/container"
	"github.com/moby/moby/api/types/volume"
	"github.com/moby/moby/client"
)

var ErrNoContainers = errors.New("engine: the project has no containers")

func projectFilter(project string) client.Filters {
	return make(client.Filters).Add("label", api.ProjectLabel+"="+project)
}

func (p *Project) PS(ctx context.Context) ([]Container, error) {
	c, err := p.Engine.api()
	if err != nil {
		return nil, err
	}
	res, err := c.ContainerList(ctx, client.ContainerListOptions{All: true, Filters: projectFilter(p.Name)})
	if err != nil {
		return nil, &Error{Op: "ps", Class: ClassOf(err), Err: err}
	}
	out := make([]Container, 0, len(res.Items))
	for _, s := range res.Items {
		if s.Labels[api.OneoffLabel] == "True" {
			continue
		}
		ct := Container{
			ID:      s.ID,
			Service: s.Labels[api.ServiceLabel],
			State:   strings.ToLower(string(s.State)),
			Image:   s.Image,
			ImageID: s.ImageID,
			Labels:  s.Labels,
			Created: time.Unix(s.Created, 0),
		}
		if len(s.Names) > 0 {
			ct.Name = strings.TrimPrefix(s.Names[0], "/")
		}
		ct.Number, _ = strconv.Atoi(s.Labels[api.ContainerNumberLabel])
		if s.Health != nil && s.Health.Status != container.NoHealthcheck {
			ct.Health = strings.ToLower(string(s.Health.Status))
		} else {
			ct.Health = healthFromStatus(s.Status)
		}
		podmanHealth := p.Engine.Kind == Podman && ct.State == "running" && ct.Health == ""
		if needsInspect(ct) || podmanHealth {
			in, err := c.ContainerInspect(ctx, s.ID, client.ContainerInspectOptions{})
			if err == nil {
				ct.RestartCount = in.Container.RestartCount
				if st := in.Container.State; st != nil {
					ct.ExitCode = st.ExitCode
					if podmanHealth && st.Health != nil && st.Health.Status != "" && st.Health.Status != container.NoHealthcheck {
						ct.Health = strings.ToLower(string(st.Health.Status))
					}
				}
			} else if !cerrdefs.IsNotFound(err) {
				return nil, &Error{Op: "ps", Class: ClassOf(err), Err: err}
			}
		}
		out = append(out, ct)
	}
	slices.SortFunc(out, func(a, b Container) int {
		if c := strings.Compare(a.Service, b.Service); c != 0 {
			return c
		}
		return a.Number - b.Number
	})
	return out, nil
}

func needsInspect(c Container) bool {
	switch c.State {
	case "running":
		return c.Health != "" && c.Health != "healthy"
	case "created", "paused", "removing":
		return false
	}
	return true
}

func healthFromStatus(status string) string {
	s := strings.ToLower(status)
	switch {
	case strings.Contains(s, "(healthy)"):
		return "healthy"
	case strings.Contains(s, "(unhealthy)"):
		return "unhealthy"
	case strings.Contains(s, "(health: starting)"), strings.Contains(s, "(starting)"):
		return "starting"
	}
	return ""
}

func (e *Engine) Volumes(ctx context.Context, project string) ([]Volume, error) {
	c, err := e.api()
	if err != nil {
		return nil, err
	}
	items, err := projectVolumes(ctx, c, project)
	if err != nil {
		return nil, &Error{Op: "volumes", Class: ClassOf(err), Err: err}
	}
	out := make([]Volume, 0, len(items))
	for _, v := range items {
		labels := v.Labels
		if labels == nil {
			labels = map[string]string{}
		}
		out = append(out, Volume{Name: v.Name, Project: labels[api.ProjectLabel], Labels: labels, Mountpoint: v.Mountpoint, Device: v.Options["device"]})
	}
	slices.SortFunc(out, func(a, b Volume) int { return strings.Compare(a.Name, b.Name) })
	return out, nil
}

func projectVolumes(ctx context.Context, c client.APIClient, project string) ([]volume.Volume, error) {
	if project == "" {
		res, err := c.VolumeList(ctx, client.VolumeListOptions{})
		return res.Items, err
	}
	res, err := c.VolumeList(ctx, client.VolumeListOptions{Filters: projectFilter(project)})
	if err != nil {
		return nil, err
	}
	named, err := c.VolumeList(ctx, client.VolumeListOptions{Filters: make(client.Filters).Add("name", project+"_")})
	if err != nil {
		return nil, err
	}
	items := res.Items
	for _, v := range named.Items {
		if v.Labels[api.ProjectLabel] == "" && strings.HasPrefix(v.Name, project+"_") {
			items = append(items, v)
		}
	}
	return items, nil
}

type PortOwner struct {
	Number  int
	Proto   string
	Project string
}

func (e *Engine) PublishedHostPorts(ctx context.Context) ([]PortOwner, error) {
	c, err := e.api()
	if err != nil {
		return nil, err
	}
	res, err := c.ContainerList(ctx, client.ContainerListOptions{})
	if err != nil {
		return nil, &Error{Op: "ports", Class: ClassOf(err), Err: err}
	}
	seen := map[PortOwner]bool{}
	out := make([]PortOwner, 0, len(res.Items))
	for _, s := range res.Items {
		project := s.Labels[api.ProjectLabel]
		for _, p := range s.Ports {
			if p.PublicPort == 0 {
				continue
			}
			proto := strings.ToLower(p.Type)
			if proto != "tcp" && proto != "udp" {
				continue
			}
			po := PortOwner{Number: int(p.PublicPort), Proto: proto, Project: project}
			if seen[po] {
				continue
			}
			seen[po] = true
			out = append(out, po)
		}
	}
	slices.SortFunc(out, func(a, b PortOwner) int {
		if a.Number != b.Number {
			return a.Number - b.Number
		}
		if c := strings.Compare(a.Proto, b.Proto); c != 0 {
			return c
		}
		return strings.Compare(a.Project, b.Project)
	})
	return out, nil
}

func (e *Engine) Projects(ctx context.Context) ([]ProjectInfo, error) {
	c, err := e.conn()
	if err != nil {
		return nil, err
	}
	stacks, err := c.svc.List(ctx, api.ListOptions{All: true})
	if err != nil {
		return nil, &Error{Op: "projects", Class: ClassOf(err), Err: err}
	}
	out := make([]ProjectInfo, 0, len(stacks))
	for _, s := range stacks {
		out = append(out, ProjectInfo{Name: s.Name, Status: s.Status, ConfigFiles: s.ConfigFiles})
	}
	return out, nil
}

func (e *Engine) LocalImage(ctx context.Context, ref string) (string, bool, error) {
	c, err := e.api()
	if err != nil {
		return "", false, err
	}
	res, err := c.ImageInspect(ctx, ref)
	if err != nil {
		if cerrdefs.IsNotFound(err) {
			return "", false, nil
		}
		return "", false, &Error{Op: "image", Class: ClassOf(err), Err: err}
	}
	return res.ID, true, nil
}

func (e *Engine) ImageSize(ctx context.Context, ref string) (uint64, bool, error) {
	c, err := e.api()
	if err != nil {
		return 0, false, err
	}
	res, err := c.ImageInspect(ctx, ref)
	if err != nil {
		if cerrdefs.IsNotFound(err) {
			return 0, false, nil
		}
		return 0, false, &Error{Op: "image", Class: ClassOf(err), Err: err}
	}
	return uint64(max(res.Size, 0)), true, nil
}

func (e *Engine) RepoDigest(ctx context.Context, image, ref string) (string, error) {
	named, err := reference.ParseNormalizedNamed(ref)
	if err != nil {
		return "", nil
	}
	c, err := e.api()
	if err != nil {
		return "", err
	}
	res, err := c.ImageInspect(ctx, image)
	if err != nil {
		if cerrdefs.IsNotFound(err) {
			return "", nil
		}
		return "", &Error{Op: "image", Class: ClassOf(err), Err: err}
	}
	for _, d := range res.RepoDigests {
		dn, err := reference.ParseNormalizedNamed(d)
		if err != nil || dn.Name() != named.Name() {
			continue
		}
		digested, ok := dn.(reference.Digested)
		if !ok {
			continue
		}
		if tagged, ok := named.(reference.NamedTagged); ok {
			if full, err := reference.WithDigest(tagged, digested.Digest()); err == nil {
				return reference.FamiliarString(full), nil
			}
		}
		return reference.FamiliarString(dn), nil
	}
	return "", nil
}

func (e *Engine) RemoveImages(ctx context.Context, refs []string) error {
	c, err := e.api()
	if err != nil {
		return err
	}
	var errs []error
	var failed []string
	for _, ref := range refs {
		_, err := c.ImageRemove(ctx, ref, client.ImageRemoveOptions{})
		if err == nil || cerrdefs.IsNotFound(err) {
			continue
		}
		if ctx.Err() != nil {
			return ctx.Err()
		}
		errs = append(errs, err)
		failed = append(failed, ref)
	}
	if err := errors.Join(errs...); err != nil {
		return &Error{Op: "remove images", Class: ClassOf(err), Images: failed, Err: err}
	}
	return nil
}
