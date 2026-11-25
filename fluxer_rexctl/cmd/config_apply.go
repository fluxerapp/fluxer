// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

var errKeptSettings = errors.New("cmd: the settings change was declined")

func applyDeployment(ctx context.Context, app *App, inst *instance.Instance, r rt.Runtime, changes map[string]*string, title string) (*deploy.Change, error) {
	if r.Kind() == rt.Compose {
		if _, err := app.guard(ctx, inst, false); err != nil {
			return nil, err
		}
	}
	if inst.State.Pending != nil {
		note := deploy.Interrupted(inst)
		err := app.UI.Step(ctx, "Finishing the last settings change", func(ctx context.Context, s *ui.Step) error {
			if err := rt.ResolvePending(ctx, r, s); err != nil {
				return err
			}
			s.Done("%s", note)
			return nil
		})
		if err != nil {
			return nil, err
		}
	}
	if err := deploy.Changeable(inst); err != nil {
		return nil, err
	}
	if err := edgeGuard(inst, changes); err != nil {
		return nil, err
	}
	p, err := deploy.Prepare(inst, changes)
	if err != nil {
		return nil, err
	}
	if p.Unchanged {
		return &deploy.Change{Keys: p.Keys, Unchanged: true}, nil
	}
	if err := r.Settings().Validate(ctx, p); err != nil {
		return nil, err
	}
	pv, err := r.Settings().Preview(ctx, p)
	if err != nil {
		return nil, err
	}
	dp := p.Preview
	dp.Restarts = pv.Restarts
	ok, err := confirmPreview(app, r.Kind(), running(inst), dp, pv.Notes...)
	if err != nil {
		return nil, err
	}
	if !ok {
		return nil, errKeptSettings
	}
	settlePorts, err := openPorts(ctx, app, inst, changes)
	if err != nil {
		return nil, err
	}
	var change *deploy.Change
	err = app.UI.Step(ctx, title, func(ctx context.Context, s *ui.Step) error {
		var err error
		change, err = r.Settings().Apply(ctx, s, p)
		if err != nil {
			return err
		}
		if !change.Unchanged {
			s.Done("%s", changedLine(change.Keys, changes))
		}
		return nil
	})
	reportVoiceDown(app, change, err)
	settlePorts(change, err)
	if err != nil {
		return nil, err
	}
	afterSettingsApplied(ctx, app, inst, changes, change)
	if !change.Unchanged {
		if err := syncEdgeSite(context.WithoutCancel(ctx), app, inst); err != nil {
			return change, err
		}
	}
	return change, nil
}

func reportVoiceDown(app *App, change *deploy.Change, err error) {
	if change == nil || len(change.VoiceDown) == 0 {
		return
	}
	ids := ui.JoinAnd(change.VoiceDown)
	if change.Applied || err != nil {
		app.UI.Warn(fmt.Sprintf("Voice servers %s didn't start again.", ids), "Run rexctl voice status to see why.")
		return
	}
	app.UI.Info("Stopped voice servers %s. They start again with Fluxer.", ids)
}

func afterSettingsApplied(ctx context.Context, app *App, inst *instance.Instance, changes map[string]*string, change *deploy.Change) {
	if change == nil || change.Unchanged {
		return
	}
	if change.Applied {
		moveVoiceEndpoints(ctx, app, inst)
	}
	if _, ok := changes[settings.VoiceBundled]; ok && change.Applied {
		restoreBundledVoiceServer(ctx, app, inst)
	}
	_, publicIP := changes[settings.VoicePublicIP]
	_, turn := changes[settings.TurnServersVar]
	turn = turn && change.Applied
	if !publicIP && !turn {
		return
	}
	if publicIP {
		moveVoicePublicIP(ctx, app, inst, change.Applied)
	}
	if turn && len(inst.State.Voice) > 0 {
		upgradeLocalVoiceServers(ctx, app, inst, nil)
	}
}

func partsOf(names []string) []string {
	var parts []string
	for _, name := range names {
		part := rt.PartOf(name)
		if part == "all" {
			return []string{"all"}
		}
		if !slices.Contains(parts, part) {
			parts = append(parts, part)
		}
	}
	return parts
}

func everything(parts []string) bool {
	if slices.Contains(parts, "all") {
		return true
	}
	for _, part := range partsOf(stack.Services) {
		if !slices.Contains(parts, part) {
			return false
		}
	}
	return true
}

func restartText(parts []string) string {
	switch {
	case len(parts) == 0:
		return "Right away"
	case everything(parts):
		return "Restarts Fluxer"
	}
	return "Restarts " + ui.JoinAnd(parts)
}

func running(inst *instance.Instance) bool {
	return !inst.State.Stopped && inst.State.Upgrade == nil
}

func changedLine(keys []string, values map[string]*string) string {
	if len(keys) == 1 {
		return deploymentDone(keys[0], values[keys[0]])
	}
	return fmt.Sprintf("Saved %d settings.", len(keys))
}

func downtime(kind rt.Kind, parts []string) string {
	long := everything(parts) || slices.Contains(parts, "database") || len(parts) > 3
	switch {
	case kind == rt.Native && long:
		return "about 10 seconds"
	case kind == rt.Native:
		return "a few seconds"
	case long:
		return "about a minute"
	}
	return "about 20 seconds"
}

func restartLine(kind rt.Kind, parts []string) string {
	if everything(parts) {
		return fmt.Sprintf("Fluxer restarts and is down for %s.", downtime(kind, parts))
	}
	if len(parts) == 1 {
		return fmt.Sprintf("Fluxer's %s restarts and is down for %s.", parts[0], downtime(kind, parts))
	}
	return fmt.Sprintf("Fluxer's %s restart and are down for %s.", ui.JoinAnd(parts), downtime(kind, parts))
}

func confirmPreview(app *App, kind rt.Kind, live bool, p deploy.Preview, extra ...string) (bool, error) {
	var restarts []string
	if live {
		restarts = partsOf(append(slices.Clone(p.Restarts), p.Removes...))
		if len(p.Networks) > 0 {
			restarts = []string{"all"}
		}
	}
	notes := p.Notes()
	for _, n := range extra {
		if !slices.Contains(notes, n) {
			notes = append(notes, n)
		}
	}
	if len(restarts) == 0 && len(notes) == 0 {
		return true, nil
	}
	for _, note := range notes {
		app.UI.Info("%s", note)
	}
	if len(restarts) > 0 {
		app.UI.Info("%s", restartLine(kind, restarts))
		app.UI.Detail("restarts: %s", strings.Join(restarts, ", "))
	}
	return app.UI.Confirm("Go ahead?", true)
}
