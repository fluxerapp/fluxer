// SPDX-License-Identifier: AGPL-3.0-or-later

package updates

import (
	"fmt"
	"time"
)

const (
	FirstDelay  = 10 * time.Minute
	FirstSpread = 30 * time.Minute
	Every       = 24 * time.Hour
	Spread      = time.Hour
	LongFailure = 7 * 24 * time.Hour
	maxBackoff  = 16 * time.Hour
)

type Random func(n time.Duration) time.Duration

func Backoff(failures int) time.Duration {
	if failures < 1 || failures > 5 {
		return Every
	}
	return min(time.Hour<<(failures-1), maxBackoff)
}

func (s State) FirstDue(start time.Time, rnd Random) time.Time {
	if s.NextAt.After(start) {
		return minTime(s.NextAt, start.Add(Every+Spread))
	}
	return start.Add(FirstDelay + rnd(FirstSpread))
}

func (s *State) Succeeded(now time.Time, releases []Release, rnd Random) {
	s.CheckedAt, s.TriedAt = now, now
	s.FailedAt, s.Failures, s.Error = time.Time{}, 0, ""
	s.Releases, s.Outdated = releases, false
	s.NextAt = now.Add(Every - Spread + rnd(2*Spread))
}

type Failure struct {
	First bool
	Week  bool
	Retry time.Duration
}

func (s *State) Failed(now time.Time, err error) Failure {
	prev := s.TriedAt
	if s.Failures == 0 {
		s.FailedAt = now
	}
	s.Failures++
	s.TriedAt = now
	s.Error = err.Error()
	f := Failure{First: s.Failures == 1, Retry: Backoff(s.Failures)}
	f.Week = !f.First && prev.Sub(s.FailedAt) < LongFailure && now.Sub(s.FailedAt) >= LongFailure
	s.NextAt = now.Add(f.Retry)
	return f
}

func (f Failure) RetryText() string {
	if h := int(f.Retry / time.Hour); h > 1 {
		return fmt.Sprintf("%d hours", h)
	}
	return "1 hour"
}

func minTime(a, b time.Time) time.Time {
	if a.Before(b) {
		return a
	}
	return b
}
