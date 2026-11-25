// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"fmt"
	"slices"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

func matchSites(app *App, inst *instance.Instance) error {
	set, err := inst.Set()
	if err != nil {
		return nil
	}
	data, err := stack.ReadFile(set, stack.Caddyfile)
	if err != nil {
		return nil
	}
	switch sites, dropIns := instance.TopLevelImports(data); {
	case sites:
		back, err := inst.RestoreSites(inst.State.MovedSites)
		for _, f := range back {
			app.UI.Info("Moved %s/%s to %s/%s.", instance.CaddyDropInDir, f, instance.CaddySitesDir, f)
		}
		if err != nil || len(inst.State.MovedSites) == 0 {
			return err
		}
		inst.State.MovedSites = nil
		return inst.Save()
	case dropIns:
		moved, err := inst.UnloadSites()
		for _, f := range moved {
			app.UI.Info("Moved %s/%s to %s/%s. The release you went back to loads sites from there.", instance.CaddySitesDir, f, instance.CaddyDropInDir, f)
		}
		if len(moved) > 0 {
			inst.State.MovedSites = slices.Compact(slices.Sorted(slices.Values(append(inst.State.MovedSites, moved...))))
			if serr := inst.Save(); err == nil {
				err = serr
			}
		}
		return err
	}
	if files, err := inst.SiteFiles(); err == nil && len(files) > 0 {
		app.UI.Warn(fmt.Sprintf("The release you went back to doesn't load %s. The sites there stay off until you upgrade.", instance.CaddySitesDir), "")
	}
	return nil
}

func matchRestoredSites(app *App) func(*instance.Instance) {
	return func(inst *instance.Instance) {
		if err := matchSites(app, inst); err != nil {
			app.UI.Warn(fmt.Sprintf("Couldn't move the Caddy sites to where Fluxer %s loads them (%v).", inst.State.Release, err), "Check "+instance.CaddyDropInDir+" and "+instance.CaddySitesDir+" before you reload Caddy.")
		}
	}
}
