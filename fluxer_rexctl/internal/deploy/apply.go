// SPDX-License-Identifier: AGPL-3.0-or-later

package deploy

import (
	"context"
	"maps"
	"slices"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const edgeService = "edge"

func Apply(ctx context.Context, inst *instance.Instance, e *engine.Engine, o ApplyOptions) (*Change, error) {
	if err := refuseEjected(inst); err != nil {
		return nil, err
	}
	if err := checkDrift(inst, true); err != nil {
		return nil, err
	}
	set, err := ReleaseSet(inst)
	if err != nil {
		return nil, err
	}
	r, err := render(inst, set, inst.State.Settings)
	if err != nil {
		return nil, err
	}
	mounted, err := mountedHashes(inst)
	if err != nil {
		return nil, err
	}
	restartEdge := len(r.RestartNeeded) > 0 || !maps.Equal(mounted, inst.State.Mounted)
	if err := save(inst); err != nil {
		return nil, err
	}
	s, err := specFor(inst, set, inst.State.Settings)
	if err != nil {
		return nil, err
	}
	p, err := e.Load(ctx, s)
	if err != nil {
		return nil, err
	}
	if o.Pull {
		if err := p.Pull(ctx, o.OnEvent); err != nil {
			return nil, err
		}
	}
	plan, err := p.Plan(ctx)
	if err != nil {
		return nil, err
	}
	if err := up(ctx, p, o.Wait, o.OnEvent); err != nil {
		return nil, err
	}
	if err := inst.DropRootCopies(ctx, true); err != nil {
		return nil, err
	}
	change := &Change{Restarted: restarted(plan), Removed: slices.Clone(plan.Remove), Applied: true}
	if restartEdge && slices.Contains(p.Services(), edgeService) && !slices.Contains(change.Restarted, edgeService) {
		if err := p.Restart(context.WithoutCancel(ctx), edgeService); err != nil {
			return change, err
		}
		change.Restarted = append(change.Restarted, edgeService)
	}
	inst.State.Mounted = mounted
	if err := save(inst); err != nil {
		return change, err
	}
	return change, nil
}

func up(ctx context.Context, p *engine.Project, wait bool, onEvent func(engine.Event)) error {
	return p.Up(context.WithoutCancel(ctx), engine.UpOptions{
		Recreate:      engine.RecreateChanged,
		RemoveOrphans: true,
		Wait:          wait,
		WaitTimeout:   engine.DefaultReadyTimeout,
		OnEvent:       onEvent,
	})
}

func restarted(plan *engine.Plan) []string {
	out := append(slices.Clone(plan.Recreate), plan.Create...)
	slices.Sort(out)
	return slices.Compact(out)
}

func running(ctx context.Context, inst *instance.Instance, e *engine.Engine) (bool, error) {
	if inst.State.Stopped {
		return false, nil
	}
	containers, err := e.Named(inst.Project(), inst.Dir).PS(ctx)
	if err != nil {
		return false, err
	}
	for _, c := range containers {
		if c.State == "running" && !stack.OneShot[c.Service] {
			return true, nil
		}
	}
	return false, nil
}
