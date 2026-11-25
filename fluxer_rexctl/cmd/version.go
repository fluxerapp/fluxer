// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"cmp"
	"context"
	"runtime"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type versionJSON struct {
	Version  string  `json:"version"`
	Commit   string  `json:"commit"`
	Go       string  `json:"go"`
	OS       string  `json:"os"`
	Arch     string  `json:"arch"`
	Fluxer   *string `json:"fluxer"`
	Installs string  `json:"installs"`
	Runtime  string  `json:"runtime,omitempty"`

	MinPodman   string `json:"min_podman"`
	MinDocker   string `json:"min_docker"`
	StateSchema int    `json:"state_schema"`
}

func newVersionCmd(app *App) *cobra.Command {
	c := &cobra.Command{
		Use:   "version",
		Short: "Show the rexctl and Fluxer versions",
		Long:  "Show the version of rexctl, the Fluxer release it installs, and the Fluxer release and runtime of the instance in this folder, if there is one.",
		Example: "rexctl version\n" +
			"rexctl version --json",
		Args: noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runVersion(c.Context(), app)
		},
	}
	app.jsonFlag(c)
	return c
}

func runVersion(ctx context.Context, app *App) error {
	v := versionJSON{Version: app.Version, Commit: app.Commit, Go: runtime.Version(), OS: runtime.GOOS, Arch: runtime.GOARCH,
		Installs: stack.EmbeddedRelease(), MinPodman: engine.MinPodman, MinDocker: engine.MinDocker, StateSchema: instance.StateSchema}
	label := ""
	if inst := quietInstance(app); inst != nil && inst.State.Release != "" && !inst.State.IsVoice() {
		if r, err := rt.Open(ctx, inst, app.UI); err == nil {
			release := inst.State.Release
			v.Runtime = string(r.Kind())
			label = engine.Kind(inst.State.Engine.Kind).Name()
			if r.Kind() != rt.Compose {
				label = r.Info(ctx).Label
				if st, err := r.Status(ctx); err == nil {
					release = cmp.Or(st.RunningRelease, release)
				}
			}
			v.Fluxer = &release
		}
	}
	if app.Flags.JSON {
		return app.UI.JSON(v)
	}
	rexctl := v.Version
	if v.Commit != "" && v.Commit != "unknown" {
		rexctl += " (" + shortCommit(v.Commit) + ")"
	}
	rows := []ui.KV{{Key: "rexctl", Value: rexctl}}
	if v.Fluxer != nil {
		rows = append(rows, ui.KV{Key: "Fluxer", Value: *v.Fluxer}, ui.KV{Key: "Runs on", Value: label})
	}
	if v.Installs != "" && (v.Fluxer == nil || *v.Fluxer != v.Installs) {
		rows = append(rows, ui.KV{Key: "Installs", Value: "Fluxer " + v.Installs})
	}
	rows = append(rows, ui.KV{Key: "Built with", Value: v.Go + ", " + v.OS + "/" + v.Arch})
	app.UI.Card(ui.Card{Title: "Versions", Rows: rows})
	return nil
}

func quietInstance(app *App) *instance.Instance {
	l, err := instance.Resolve(app.Flags.Dir)
	if err != nil {
		return nil
	}
	inst, err := instance.Find(l.Dir)
	if err != nil || inst.State == nil {
		return nil
	}
	return inst
}

func shortCommit(c string) string {
	if len(c) > 12 {
		return c[:12]
	}
	return c
}
