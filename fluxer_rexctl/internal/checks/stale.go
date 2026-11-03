// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
)

func init() {
	Register(Check{ID: "stale-containers", Phases: Installed, Run: runStale, Fix: fixStale, SafeFix: true})
}

func (e *Env) staleServices(ctx context.Context) ([]string, error) {
	cs, err := e.containers(ctx)
	if err != nil {
		return nil, err
	}
	ids := map[string]string{}
	seen := map[string]bool{}
	var out []string
	for _, c := range cs {
		if c.Image == "" || c.ImageID == "" || seen[c.Service] {
			continue
		}
		id, ok := ids[c.Image]
		if !ok {
			local, found, err := e.Engine.LocalImage(ctx, c.Image)
			if err != nil || !found {
				local = ""
			}
			id = local
			ids[c.Image] = id
		}
		if id != "" && imageID(id) != imageID(c.ImageID) {
			seen[c.Service] = true
			out = append(out, c.Service)
		}
	}
	return out, nil
}

func imageID(id string) string { return strings.TrimPrefix(strings.TrimSpace(id), "sha256:") }

func runStale(ctx context.Context, e *Env) Result {
	if e.Engine == nil {
		return skip("")
	}
	stale, err := e.staleServices(ctx)
	if err != nil {
		return skip("")
	}
	if len(stale) == 0 {
		return pass("Every Fluxer service runs the version on this machine.")
	}
	r := fixable(warn(Services(stale, "still runs an older version.", "still run an older version."),
		"Run rexctl doctor --fix to restart them on the current version."))
	if len(stale) == 1 {
		r.Fix = "Run rexctl doctor --fix to restart it on the current version."
	}
	r.Details = stale
	return r
}

func fixStale(ctx context.Context, e *Env) error {
	stale, err := e.staleServices(ctx)
	if err != nil || len(stale) == 0 {
		return err
	}
	if e.loadErr != nil {
		return e.loadErr
	}
	return e.Project.Up(ctx, engine.UpOptions{Services: stale, NoDeps: true, Recreate: engine.RecreateAlways})
}
