// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"encoding/hex"
	"errors"
	"fmt"
	"maps"
	"os"
	"slices"
	"strings"

	"github.com/compose-spec/compose-go/v2/types"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type dataPlace struct {
	short   string
	service string
	target  string
}

var oldDataPlaces = func() []dataPlace {
	out := []dataPlace{{"edge-data", "edge", "/data"}, {"edge-config", "edge", "/config"}}
	for _, short := range slices.Sorted(maps.Keys(stack.DataMounts)) {
		dm := stack.DataMounts[short]
		out = append(out, dataPlace{short, dm.Service, dm.Target})
	}
	return out
}()

func placeAt(service, target string) (dataPlace, bool) {
	for _, d := range oldDataPlaces {
		if d.service == service && d.target == target {
			return d, true
		}
	}
	return dataPlace{}, false
}

type mountSource struct {
	bind   string
	volume string
}

func (m mountSource) String() string {
	switch {
	case m.bind != "":
		return "the folder " + m.bind
	case m.volume != "":
		return "the volume " + m.volume
	}
	return "a new empty volume"
}

func anonymousVolume(name string) bool {
	if len(name) != 64 {
		return false
	}
	_, err := hex.DecodeString(name)
	return err == nil
}

func modelService(m *types.Project, name string) (types.ServiceConfig, bool) {
	if m == nil {
		return types.ServiceConfig{}, false
	}
	if svc, ok := m.Services[name]; ok {
		return svc, true
	}
	svc, ok := m.DisabledServices[name]
	return svc, ok
}

func modelSource(m *types.Project, project string, svc types.ServiceConfig, target string) mountSource {
	for _, v := range svc.Volumes {
		if v.Target != target {
			continue
		}
		switch v.Type {
		case types.VolumeTypeBind:
			if v.Source != "" {
				return mountSource{bind: cleanPath(v.Source)}
			}
		case types.VolumeTypeVolume:
			if v.Source == "" {
				return mountSource{}
			}
			name := project + "_" + v.Source
			if vol, ok := m.Volumes[v.Source]; ok && vol.Name != "" {
				name = vol.Name
			}
			return mountSource{volume: name}
		}
	}
	return mountSource{}
}

func modelBind(m *types.Project, service, target string) string {
	svc, ok := modelService(m, service)
	if !ok {
		return ""
	}
	return modelSource(m, "", svc, target).bind
}

func containerSource(c engine.Container, target string) (mountSource, bool) {
	for _, mt := range c.Mounts {
		if mt.Target != target {
			continue
		}
		switch {
		case mt.Bind != "":
			return mountSource{bind: cleanPath(mt.Bind)}, true
		case anonymousVolume(mt.Volume):
			return mountSource{}, true
		}
		return mountSource{volume: mt.Volume}, true
	}
	return mountSource{}, false
}

func folderHoldsData(dir string) bool {
	entries, err := os.ReadDir(dir)
	if err != nil {
		return !errors.Is(err, os.ErrNotExist)
	}
	return len(entries) > 0
}

type mountDrift struct {
	place dataPlace
	had   mountSource
	will  mountSource
}

func (d mountDrift) sentence() string {
	return fmt.Sprintf("%s ran on %s, and with %s as it is now it would get %s.", d.place.service, d.had, instance.CustomCompose, d.will)
}

func mountDrifts(ctx context.Context, p *engine.Project) ([]mountDrift, error) {
	if p == nil || p.Model == nil {
		return nil, nil
	}
	containers, err := p.PS(ctx)
	if err != nil {
		return nil, err
	}
	var out []mountDrift
	for _, d := range oldDataPlaces {
		svc, ok := p.Model.Services[d.service]
		if !ok {
			continue
		}
		will := modelSource(p.Model, p.Name, svc, d.target)
		for _, c := range containers {
			if c.Service != d.service {
				continue
			}
			if had, ok := containerSource(c, d.target); ok && had != will {
				out = append(out, mountDrift{place: d, had: had, will: will})
			}
			break
		}
	}
	return out, nil
}

func driftLines(drifts []mountDrift) string {
	lines := make([]string, len(drifts))
	for i, d := range drifts {
		lines[i] = d.sentence()
	}
	return strings.Join(lines, " ")
}

func checkMountDrift(ctx context.Context, p *engine.Project) error {
	drifts, err := mountDrifts(ctx, p)
	if err != nil || len(drifts) == 0 {
		return err
	}
	return ui.Refused(driftLines(drifts)+" The backup would read the wrong data.",
		"Put back the lines in "+instance.CustomCompose+" that mount the data the containers ran on, with rexctl config edit --compose, then run "+migrateCommand+" again.")
}

func oldFolders(m *types.Project) []string {
	var out []string
	for _, d := range oldDataPlaces {
		if bind := modelBind(m, d.service, d.target); bind != "" && !slices.Contains(out, bind) {
			out = append(out, bind)
		}
	}
	return out
}

func oldFoldersOf(ctx context.Context, app *App, inst *instance.Instance) []string {
	e, err := app.Engine(ctx, inst)
	if err != nil {
		return nil
	}
	p, err := deploy.Load(ctx, inst, e)
	if err != nil {
		return nil
	}
	return oldFolders(p.Model)
}

var storageUsers = []string{"api", "worker", "media-proxy"}

func objectsMountProblem(m *types.Project) error {
	if m == nil {
		return nil
	}
	var bound, lost []string
	for _, name := range storageUsers {
		svc, ok := m.Services[name]
		if !ok {
			continue
		}
		switch src := modelSource(m, "", svc, stack.ObjectsRoot); {
		case src.bind != "":
			bound = append(bound, name)
		case src.volume == "":
			lost = append(lost, name)
		}
	}
	if len(bound) > 0 {
		return ui.Refused(fmt.Sprintf("%s mounts a folder at %s for %s, and the move off SeaweedFS puts the files in the %s volume instead.", instance.CustomCompose, stack.ObjectsRoot, ui.JoinAnd(bound), stack.ObjectsVolume),
			"Remove that mount with rexctl config edit --compose, then try again.")
	}
	if len(lost) > 0 {
		return ui.Refused(fmt.Sprintf("%s replaces the volumes of %s, so after the move off SeaweedFS Fluxer wouldn't find the uploaded files there.", instance.CustomCompose, ui.JoinAnd(lost)),
			"Remove !override from the volumes of "+ui.JoinAnd(lost)+" with rexctl config edit --compose, so the mounts listed there add to Fluxer's own, then try again.")
	}
	return nil
}
