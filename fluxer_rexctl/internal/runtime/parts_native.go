// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"slices"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type embeddedPart struct {
	name   string
	member string
	child  string
}

func nativeEmbedded(settings map[string]string) []embeddedPart {
	out := []embeddedPart{{name: "edge", member: "edge"}, {name: "queue", member: "nats"}}
	switch {
	case stack.KVPostgres(settings) && stack.ExternalPostgres(settings):
		out = append(out, embeddedPart{name: "cache", member: "postgres"})
	case stack.KVPostgres(settings):
		out = append(out, embeddedPart{name: "cache", member: "postgres", child: "postgres"})
	}
	if stack.FilesystemStorage(settings) {
		out = append(out, embeddedPart{name: "storage", member: "files"})
	}
	return out
}

func withEmbedded(parts []Part, cs []engine.Container, since time.Time, settings map[string]string) []Part {
	if len(parts) == 0 {
		return parts
	}
	for _, ep := range nativeEmbedded(settings) {
		if slices.ContainsFunc(parts, func(p Part) bool { return p.Name == ep.name }) {
			continue
		}
		p := Part{Name: ep.name, Members: []string{ep.member}, CPU: -1, MemBytes: -1, Optional: optionalPart(ep.name), State: PartRunning, Since: since}
		if ep.child != "" {
			p.State, p.Since = PartStopped, time.Time{}
			if i := slices.IndexFunc(cs, func(c engine.Container) bool { return c.Service == ep.child }); i >= 0 {
				p.State = orRunning(containerState(cs[i]))
				if cs[i].State == "running" {
					p.Since = cs[i].Created
				}
			}
		}
		parts = append(parts, p)
	}
	order := PartNames()
	slices.SortStableFunc(parts, func(a, b Part) int {
		return rank(order, a.Name) - rank(order, b.Name)
	})
	return parts
}

func orRunning(s PartState) PartState {
	if s == "" {
		return PartRunning
	}
	return s
}

func (r *nativeRT) children(parts []string) ([]string, error) {
	return childrenOf(r.inst, parts)
}

func childrenOf(inst *instance.Instance, parts []string) ([]string, error) {
	embedded := nativeEmbedded(inst.State.Settings)
	var rest, inside []string
	for _, p := range parts {
		name := strings.ToLower(strings.TrimSpace(p))
		if slices.ContainsFunc(embedded, func(e embeddedPart) bool { return e.name == name && e.child == "" }) {
			inside = append(inside, name)
			continue
		}
		rest = append(rest, p)
	}
	if len(inside) > 0 {
		verb := "runs"
		if len(inside) > 1 {
			verb = "run"
		}
		return nil, unsupported("On this instance, "+ui.JoinAnd(inside)+" "+verb+" inside fluxerd.", "Run rexctl restart to restart all of Fluxer.")
	}
	services, err := ServicesOf(rest)
	if err != nil {
		return nil, err
	}
	found, _ := stack.ModeServices(inst.State.Settings, services)
	if len(found) == 0 {
		return nil, unsupported("This instance runs without "+ui.JoinAnd(rest)+".", "Run rexctl status to see what runs.")
	}
	var children []string
	for _, c := range found {
		if !slices.Contains(children, c) {
			children = append(children, c)
		}
	}
	return children, nil
}
