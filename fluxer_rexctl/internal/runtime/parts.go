// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"fmt"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type partDef struct {
	name     string
	services []string
	optional bool
}

var partDefs = []partDef{
	{"web", []string{"app-proxy", "static-proxy"}, false},
	{"api", []string{"api", "worker"}, false},
	{"gateway", []string{"gateway"}, false},
	{"media", []string{"media-proxy"}, false},
	{"search", []string{"meilisearch"}, false},
	{"database", []string{"postgres"}, false},
	{"cache", []string{"valkey"}, false},
	{"storage", []string{"seaweedfs", "seaweedfs-init", stack.ObjectsInitService}, false},
	{"queue", []string{"nats"}, false},
	{"voice", []string{"livekit"}, true},
	{"edge", []string{"edge"}, false},
	{"admin", []string{"admin"}, false},
	{"push", []string{"push"}, true},
	{"unfurl", []string{"unfurl"}, false},
	{"internal", []string{"snowflakes", "users", "gifs", "messages", stack.RustService}, false},
}

func PartNames() []string {
	out := make([]string, len(partDefs))
	for i, p := range partDefs {
		out[i] = p.name
	}
	return out
}

func PartOf(service string) string {
	for _, p := range partDefs {
		if slices.Contains(p.services, service) {
			return p.name
		}
	}
	return service
}

func ServicesOf(parts []string) ([]string, error) {
	var out []string
	for _, name := range parts {
		name = strings.ToLower(strings.TrimSpace(name))
		i := slices.IndexFunc(partDefs, func(p partDef) bool { return p.name == name })
		if i < 0 && slices.Contains(stack.Services, name) {
			if !slices.Contains(out, name) {
				out = append(out, name)
			}
			continue
		}
		if i < 0 {
			return nil, &ui.ExitError{
				Code: ui.ExitUsage,
				Msg:  fmt.Sprintf("There's no part called %s.", name),
				Fix:  "Pick " + ui.JoinOr(PartNames()) + ".",
			}
		}
		for _, svc := range partDefs[i].services {
			if !slices.Contains(out, svc) {
				out = append(out, svc)
			}
		}
	}
	return out, nil
}

func foldParts(containers []engine.Container) []Part {
	var out []Part
	index := map[string]int{}
	for _, c := range containers {
		name := PartOf(c.Service)
		i, ok := index[name]
		if !ok {
			i = len(out)
			index[name] = i
			out = append(out, Part{Name: name, CPU: -1, MemBytes: -1, Optional: optionalPart(name)})
		}
		p := &out[i]
		p.Members = append(p.Members, c.Service)
		p.Restarts += c.RestartCount
		if c.State == "running" && (p.Since.IsZero() || c.Created.Before(p.Since)) {
			p.Since = c.Created
		}
		p.State = merge(p.State, containerState(c))
		if c.Health != "" && c.Health != "healthy" && p.Health == "" {
			p.Health = c.Service + " is " + c.Health
		}
	}
	order := PartNames()
	slices.SortStableFunc(out, func(a, b Part) int {
		return rank(order, a.Name) - rank(order, b.Name)
	})
	for i := range out {
		if out[i].State == "" {
			out[i].State = PartRunning
		}
	}
	return out
}

func rank(order []string, name string) int {
	if i := slices.Index(order, name); i >= 0 {
		return i
	}
	return len(order)
}

func optionalPart(name string) bool {
	for _, p := range partDefs {
		if p.name == name {
			return p.optional
		}
	}
	return false
}

func containerState(c engine.Container) PartState {
	switch c.State {
	case "running":
		switch c.Health {
		case "starting":
			return PartStarting
		case "unhealthy":
			return PartDegraded
		}
		return PartRunning
	case "created":
		return PartStarting
	case "restarting", "dead":
		return PartFailed
	case "exited":
		if stack.OneShot[c.Service] && c.ExitCode == 0 {
			return ""
		}
		if c.ExitCode != 0 {
			return PartFailed
		}
	}
	return PartStopped
}

func merge(a, b PartState) PartState {
	switch {
	case a == "":
		return b
	case b == "", a == b:
		return a
	case a == PartFailed || b == PartFailed:
		return PartFailed
	case a == PartStopped || b == PartStopped:
		return PartDegraded
	case a == PartDegraded || b == PartDegraded:
		return PartDegraded
	}
	return PartStarting
}
