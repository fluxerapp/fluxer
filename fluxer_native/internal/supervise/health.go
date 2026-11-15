// SPDX-License-Identifier: AGPL-3.0-or-later

package supervise

import (
	"context"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/logs"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/probe"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

type watched struct {
	name   string
	probes []spec.Probe
	health spec.HealthState
	err    string
}

func groupWatch(probes []spec.Probe) []*watched {
	var out []*watched
	byName := map[string]*watched{}
	for _, pr := range probes {
		w, ok := byName[pr.Name]
		if !ok {
			w = &watched{name: pr.Name}
			byName[pr.Name] = w
			out = append(out, w)
		}
		w.probes = append(w.probes, pr)
	}
	return out
}

type tracker struct {
	seen  bool
	fails int
}

func (t *tracker) next(err error, unhealthyAfter int, graceOver bool) (spec.HealthState, bool) {
	switch {
	case err == nil:
		t.seen, t.fails = true, 0
		return spec.HealthHealthy, true
	case t.seen || graceOver:
		t.fails++
		return spec.HealthUnhealthy, t.fails >= unhealthyAfter
	}
	return "", false
}

func (s *Supervisor) watchHealth(ctx context.Context, p *proc, gen int) {
	if len(p.c.Probes) == 0 && len(p.watch) == 0 {
		return
	}
	started := time.Now()
	var gate tracker
	watch := make([]tracker, len(p.watch))
	for {
		interval := s.t.HealthEvery
		if len(p.c.Probes) > 0 && !gate.seen {
			interval = s.t.ProbeEvery
		}
		for _, w := range watch {
			if !w.seen {
				interval = s.t.ProbeEvery
			}
		}
		timer := time.NewTimer(interval)
		select {
		case <-ctx.Done():
			timer.Stop()
			return
		case <-timer.C:
		}
		if len(p.c.Probes) > 0 {
			err := s.check(ctx, p.c.Probes)
			if ctx.Err() != nil {
				return
			}
			if next, ok := gate.next(err, s.t.UnhealthyAfter, false); ok {
				changed := false
				s.update(func() {
					if p.gen == gen && p.state == spec.StateRunning && p.health != next {
						p.health = next
						changed = true
					}
				})
				s.logHealth(changed, p.c.Name, next, err)
			}
		}
		for i, w := range p.watch {
			err := s.check(ctx, w.probes)
			if ctx.Err() != nil {
				return
			}
			next, ok := watch[i].next(err, s.t.UnhealthyAfter, time.Since(started) >= s.t.WatchGrace)
			if !ok {
				continue
			}
			changed := false
			s.update(func() {
				if p.gen == gen && p.state == spec.StateRunning {
					changed = w.health != next
					w.health = next
					w.err = ""
					if err != nil {
						w.err = err.Error()
					}
				}
			})
			s.logHealth(changed, p.c.Name+"/"+w.name, next, err)
		}
	}
}

func (s *Supervisor) logHealth(changed bool, name string, next spec.HealthState, err error) {
	switch {
	case changed && next == spec.HealthHealthy:
		s.out.Logf(logs.PrioInfo, "%s is healthy", name)
	case changed && next == spec.HealthUnhealthy:
		s.out.Logf(logs.PrioWarning, "%s is unhealthy: %v", name, err)
	}
}

func (s *Supervisor) check(ctx context.Context, probes []spec.Probe) error {
	ctx, cancel := context.WithTimeout(ctx, s.t.ProbeTimeout)
	defer cancel()
	return probe.All(ctx, probes)
}
