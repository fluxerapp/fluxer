// SPDX-License-Identifier: AGPL-3.0-or-later

package engine

import (
	"context"
	"fmt"
	"maps"
	"slices"
	"strings"

	"github.com/compose-spec/compose-go/v2/types"
	"github.com/docker/compose/v5/pkg/api"
	"github.com/docker/compose/v5/pkg/compose"
	"github.com/moby/moby/api/types/network"
	"github.com/moby/moby/client"
)

func (p *Project) Plan(ctx context.Context) (*Plan, error) {
	if err := p.needModel("plan"); err != nil {
		return nil, err
	}
	if err := p.Engine.refuseTooOld("plan"); err != nil {
		return nil, err
	}
	containers, err := p.PS(ctx)
	if err != nil {
		return nil, err
	}
	plan := &Plan{Create: []string{}, Recreate: []string{}, Remove: []string{}, Networks: []string{}, LabelOnly: []string{}, Volumes: []string{}}
	byService := map[string][]Container{}
	for _, c := range containers {
		byService[c.Service] = append(byService[c.Service], c)
	}
	images := map[string]string{}
	for _, name := range p.Services() {
		svc := p.Model.Services[name]
		cs := byService[name]
		if len(cs) == 0 {
			plan.Create = append(plan.Create, name)
			continue
		}
		want, err := compose.ServiceHash(svc)
		if err != nil {
			return nil, &Error{Op: "plan", Class: ClassInvalidConfig, Err: err}
		}
		id, ok := images[svc.Image]
		if !ok {
			found := false
			id, found, err = p.Engine.LocalImage(ctx, svc.Image)
			if err != nil {
				return nil, err
			}
			if !found {
				id = ""
			}
			images[svc.Image] = id
		}
		for _, c := range cs {
			if c.Labels[api.ConfigHashLabel] != want || id == "" || c.ImageID != id {
				plan.Recreate = append(plan.Recreate, name)
				break
			}
		}
	}
	for service := range byService {
		if svc, ok := p.Model.Services[service]; !ok || svc.GetScale() == 0 {
			plan.Remove = append(plan.Remove, service)
		}
	}
	slices.Sort(plan.Remove)
	if err := p.planNetworks(ctx, plan); err != nil {
		return nil, err
	}
	if err := p.planVolumes(ctx, plan); err != nil {
		return nil, err
	}
	if len(plan.Volumes) > 0 {
		return plan, &Error{Op: "plan", Class: ClassVolumeChanged, Err: fmt.Errorf("volume %s has another driver or other options than the compose file asks for", strings.Join(plan.Volumes, ", "))}
	}
	return plan, nil
}

func (p *Project) planNetworks(ctx context.Context, plan *Plan) error {
	c, err := p.Engine.api()
	if err != nil {
		return err
	}
	res, err := c.NetworkList(ctx, client.NetworkListOptions{Filters: projectFilter(p.Name)})
	if err != nil {
		return &Error{Op: "plan", Class: ClassOf(err), Err: err}
	}
	for _, key := range slices.Sorted(maps.Keys(p.Model.Networks)) {
		want := p.Model.Networks[key]
		if want.External {
			continue
		}
		var live *network.Summary
		for i, n := range res.Items {
			if n.Labels[api.NetworkLabel] == key || n.Name == want.Name {
				live = &res.Items[i]
				break
			}
		}
		if live == nil {
			continue
		}
		in, err := c.NetworkInspect(ctx, live.ID, client.NetworkInspectOptions{})
		if err != nil {
			return &Error{Op: "plan", Class: ClassOf(err), Err: err}
		}
		if !sameNetwork(want, in.Network.Network) {
			plan.Networks = append(plan.Networks, key)
			continue
		}
		hash, err := compose.NetworkHash(&want)
		if err != nil {
			return &Error{Op: "plan", Class: ClassInvalidConfig, Err: err}
		}
		if live.Labels[api.ConfigHashLabel] != hash {
			plan.LabelOnly = append(plan.LabelOnly, key)
		}
	}
	return nil
}

func sameNetwork(want types.NetworkConfig, live network.Network) bool {
	driver := want.Driver
	if driver == "" {
		driver = "bridge"
	}
	if driver != live.Driver {
		return false
	}
	if (want.EnableIPv6 != nil && *want.EnableIPv6) != live.EnableIPv6 {
		return false
	}
	if want.Internal != live.Internal {
		return false
	}
	var wantSubnets, liveSubnets []string
	for _, pool := range want.Ipam.Config {
		if pool != nil && pool.Subnet != "" {
			wantSubnets = append(wantSubnets, pool.Subnet)
		}
	}
	if len(wantSubnets) == 0 {
		return true
	}
	for _, cfg := range live.IPAM.Config {
		if cfg.Subnet.IsValid() {
			liveSubnets = append(liveSubnets, cfg.Subnet.String())
		}
	}
	slices.Sort(wantSubnets)
	slices.Sort(liveSubnets)
	return slices.Equal(wantSubnets, liveSubnets)
}

func (p *Project) planVolumes(ctx context.Context, plan *Plan) error {
	c, err := p.Engine.api()
	if err != nil {
		return err
	}
	items, err := projectVolumes(ctx, c, p.Name)
	if err != nil {
		return &Error{Op: "plan", Class: ClassOf(err), Err: err}
	}
	for _, key := range slices.Sorted(maps.Keys(p.Model.Volumes)) {
		want := p.Model.Volumes[key]
		if want.External {
			continue
		}
		for _, v := range items {
			if v.Labels[api.VolumeLabel] != key && v.Name != want.Name {
				continue
			}
			driver := want.Driver
			if driver == "" {
				driver = "local"
			}
			if driver != v.Driver || !sameOptions(want.DriverOpts, v.Options) {
				plan.Volumes = append(plan.Volumes, key)
			}
			break
		}
	}
	return nil
}

func sameOptions(want map[string]string, live map[string]string) bool {
	if len(want) == 0 && len(live) == 0 {
		return true
	}
	return maps.Equal(want, live)
}

func (p *Project) DroppedMounts(shipped *Project) []string {
	var out []string
	for _, name := range slices.Sorted(maps.Keys(shipped.Model.Services)) {
		svc, ok := p.Model.Services[name]
		if !ok {
			continue
		}
		for _, v := range shipped.Model.Services[name].Volumes {
			if !slices.ContainsFunc(svc.Volumes, func(m types.ServiceVolumeConfig) bool { return m.Target == v.Target }) {
				out = append(out, v.Target+" on "+name)
			}
		}
	}
	return out
}
