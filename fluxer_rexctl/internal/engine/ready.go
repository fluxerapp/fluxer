// SPDX-License-Identifier: AGPL-3.0-or-later

package engine

import (
	"context"
	"slices"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const (
	readyInterval  = 5 * time.Second
	crashRestarts  = 5
	freshContainer = 15 * time.Minute
)

func (p *Project) WaitReady(ctx context.Context, timeout time.Duration, onTick func(Readiness), skip ...string) error {
	if timeout <= 0 {
		timeout = DefaultReadyTimeout
	}
	deadline := time.Now().Add(timeout)
	first := map[string]int{}
	ticker := time.NewTicker(readyInterval)
	defer ticker.Stop()
	for {
		containers, err := p.PS(ctx)
		if err != nil {
			return err
		}
		containers = slices.DeleteFunc(containers, func(c Container) bool { return slices.Contains(skip, c.Service) })
		services := p.Services()
		if services == nil {
			services = servicesOf(containers)
		}
		services = slices.DeleteFunc(services, func(s string) bool { return slices.Contains(skip, s) })
		r, fatal := Assess(services, containers)
		if onTick != nil {
			onTick(r)
		}
		if len(r.NotReady) == 0 && len(r.Failed) == 0 {
			return nil
		}
		crashing := crashLooping(r, containers, first)
		if fatal || len(crashing) > 0 || time.Now().After(deadline) {
			return &NotReadyError{Readiness: r, Fatal: fatal, Crashing: crashing}
		}
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-ticker.C:
		}
	}
}

func (p *Project) Crashing(ctx context.Context) error {
	containers, err := p.PS(ctx)
	if err != nil {
		return err
	}
	services := p.Services()
	if services == nil {
		services = servicesOf(containers)
	}
	r, fatal := Assess(services, containers)
	if crashing := crashLooping(r, containers, map[string]int{}); len(crashing) > 0 {
		return &NotReadyError{Readiness: r, Fatal: fatal, Crashing: crashing}
	}
	return nil
}

func servicesOf(containers []Container) []string {
	var out []string
	for _, c := range containers {
		if !slices.Contains(out, c.Service) {
			out = append(out, c.Service)
		}
	}
	return out
}

func Assess(services []string, containers []Container) (Readiness, bool) {
	r := Readiness{Ready: []string{}, NotReady: []string{}, Failed: []string{}}
	fatal := false
	byService := map[string][]Container{}
	for _, c := range containers {
		byService[c.Service] = append(byService[c.Service], c)
	}
	for _, svc := range services {
		cs := byService[svc]
		if len(cs) == 0 {
			r.NotReady = append(r.NotReady, svc)
			continue
		}
		state := "ready"
		for _, c := range cs {
			switch s := containerState(svc, c); {
			case s == "failed":
				state = "failed"
				if stack.OneShot[svc] {
					fatal = true
				}
			case s == "waiting" && state == "ready":
				state = "waiting"
			}
		}
		switch state {
		case "ready":
			r.Ready = append(r.Ready, svc)
		case "failed":
			r.Failed = append(r.Failed, svc)
		default:
			r.NotReady = append(r.NotReady, svc)
		}
	}
	slices.Sort(r.Ready)
	slices.Sort(r.NotReady)
	slices.Sort(r.Failed)
	return r, fatal
}

func crashLooping(r Readiness, containers []Container, first map[string]int) []string {
	var out []string
	for _, c := range containers {
		if stack.OneShot[c.Service] || slices.Contains(r.Ready, c.Service) {
			continue
		}
		base, ok := first[c.ID]
		if !ok {
			base = c.RestartCount
			if time.Since(c.Created) < freshContainer {
				base = 0
			}
			first[c.ID] = base
		}
		if c.RestartCount-base >= crashRestarts && !slices.Contains(out, c.Service) {
			out = append(out, c.Service)
		}
	}
	slices.Sort(out)
	return out
}

func containerState(service string, c Container) string {
	if stack.OneShot[service] {
		switch c.State {
		case "exited":
			if c.ExitCode == 0 {
				return "ready"
			}
			return "failed"
		case "dead":
			return "failed"
		}
		return "waiting"
	}
	switch c.State {
	case "running":
		switch c.Health {
		case "", "healthy":
			return "ready"
		case "unhealthy":
			return "failed"
		}
		return "waiting"
	case "exited", "dead":
		return "failed"
	}
	return "waiting"
}
