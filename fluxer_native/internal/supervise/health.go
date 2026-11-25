// SPDX-License-Identifier: AGPL-3.0-or-later

package supervise

import (
	"cmp"
	"context"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/logs"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/probe"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

type watched struct {
	name   string
	core   bool
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
		w.core = w.core || pr.Core
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

func (t *tracker) exhausted(retries int) bool {
	return retries > 0 && t.fails >= retries
}

func (s *Supervisor) healthCheck(c spec.Child) spec.HealthCheck {
	return spec.HealthCheck{
		Interval:    cmp.Or(c.Health.Interval, s.t.HealthEvery),
		Timeout:     cmp.Or(c.Health.Timeout, s.t.ProbeTimeout),
		StartPeriod: cmp.Or(c.Health.StartPeriod, s.t.StartPeriod),
		Retries:     cmp.Or(c.Health.Retries, s.t.Retries),
	}
}

func (s *Supervisor) watchHealth(ctx context.Context, p *proc, gen int, c spec.Child, ws []*watched) {
	if len(c.Probes) == 0 && len(ws) == 0 {
		return
	}
	hc := s.healthCheck(c)
	unhealthyAfter := min(s.t.UnhealthyAfter, hc.Retries)
	started := time.Now()
	var gate tracker
	watch := make([]tracker, len(ws))
	for {
		graceOver := time.Since(started) >= hc.StartPeriod
		interval := hc.Interval
		pending := len(c.Probes) > 0 && !gate.seen
		for _, w := range watch {
			pending = pending || !w.seen
		}
		if pending && !graceOver {
			interval = min(s.t.ProbeEvery, interval)
		}
		timer := time.NewTimer(interval)
		select {
		case <-ctx.Done():
			timer.Stop()
			return
		case <-timer.C:
		}
		graceOver = time.Since(started) >= hc.StartPeriod
		restart := false
		if len(c.Probes) > 0 {
			err := s.check(ctx, c.Probes, hc.Timeout)
			if ctx.Err() != nil {
				return
			}
			if next, ok := gate.next(err, unhealthyAfter, graceOver); ok {
				changed := false
				s.update(func() {
					if p.gen != gen || !p.alive() {
						return
					}
					if next == spec.HealthHealthy && p.state == spec.StateStarting {
						p.state = spec.StateRunning
					}
					changed = p.health != next
					p.health = next
				})
				s.logHealth(changed, c.Name, next, err)
			}
			restart = gate.exhausted(hc.Retries)
		}
		for i, w := range ws {
			err := s.check(ctx, w.probes, hc.Timeout)
			if ctx.Err() != nil {
				return
			}
			next, ok := watch[i].next(err, unhealthyAfter, graceOver)
			if w.core {
				restart = restart || watch[i].exhausted(hc.Retries)
			}
			if !ok {
				continue
			}
			changed := false
			s.update(func() {
				if p.gen == gen && p.alive() {
					changed = w.health != next
					w.health = next
					w.err = ""
					if err != nil {
						w.err = err.Error()
					}
				}
			})
			s.logHealth(changed, c.Name+"/"+w.name, next, err)
		}
		if restart && c.RestartUnhealthy {
			select {
			case p.kick <- gen:
			default:
			}
			return
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

func (s *Supervisor) check(ctx context.Context, probes []spec.Probe, timeout time.Duration) error {
	ctx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()
	return probe.All(ctx, probes)
}
