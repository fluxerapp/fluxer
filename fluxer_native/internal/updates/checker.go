// SPDX-License-Identifier: AGPL-3.0-or-later

package updates

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"math/rand/v2"
	"net/http"
	"os"
	"sync"
	"time"
)

const wake = time.Hour

type Checker struct {
	Path     string
	Client   *http.Client
	Instance func() (Instance, error)
	Log      func(line string)
	Now      func() time.Time
	Rand     Random
	Sleep    func(ctx context.Context, d time.Duration) bool

	mu sync.Mutex
}

func NewChecker(path string, instance func() (Instance, error), log func(string)) *Checker {
	return &Checker{
		Path:     path,
		Client:   NewClient(),
		Instance: instance,
		Log:      log,
		Now:      time.Now,
		Rand:     func(n time.Duration) time.Duration { return rand.N(n) },
		Sleep:    sleep,
	}
}

func sleep(ctx context.Context, d time.Duration) bool {
	t := time.NewTimer(d)
	defer t.Stop()
	select {
	case <-ctx.Done():
		return false
	case <-t.C:
		return true
	}
}

func (c *Checker) Report() (Report, error) {
	in, err := c.Instance()
	if err != nil {
		return Report{}, err
	}
	return c.report(in)
}

func (c *Checker) report(in Instance) (Report, error) {
	s, err := Load(c.Path)
	if err != nil {
		return Report{}, err
	}
	return s.Report(in), nil
}

func (c *Checker) Run(ctx context.Context) {
	s, _ := Load(c.Path)
	next := s.FirstDue(c.Now(), c.Rand)
	c.publish()
	for {
		if !c.Sleep(ctx, min(next.Sub(c.Now()), wake)) {
			return
		}
		in, err := c.Instance()
		if err != nil || !in.Enabled {
			c.publish()
			if !c.Now().Before(next) {
				next = c.Now().Add(wake)
			}
			continue
		}
		next = minTime(next, c.Now().Add(Every+Spread))
		if c.Now().Before(next) {
			continue
		}
		c.check(ctx, in)
		if s, err := Load(c.Path); err == nil && !s.NextAt.IsZero() {
			next = s.NextAt
		} else {
			next = c.Now().Add(Every)
		}
	}
}

func (c *Checker) Check(ctx context.Context) (Report, error) {
	in, err := c.Instance()
	if err != nil {
		return Report{}, err
	}
	return c.check(ctx, in)
}

func (c *Checker) check(ctx context.Context, in Instance) (Report, error) {
	c.mu.Lock()
	defer c.mu.Unlock()
	s, err := Load(c.Path)
	if err != nil {
		return Report{}, err
	}
	releases, fetchErr := Fetch(ctx, c.Client, in.URL)
	now := c.Now()
	switch {
	case errors.Is(fetchErr, ErrNewerFormat):
		s.Succeeded(now, s.Releases, c.Rand)
		s.Outdated, fetchErr = true, nil
	case fetchErr != nil:
		f := s.Failed(now, fetchErr)
		if f.First {
			c.Log("Couldn't check for new releases at " + in.URL + " (" + fetchErr.Error() + "). Fluxer tries again in " + f.RetryText() + ".")
		}
		if f.Week {
			c.Log("Fluxer hasn't been able to check for new releases for 7 days.")
		}
	default:
		s.Succeeded(now, releases, c.Rand)
	}
	r := s.Report(in)
	for _, line := range r.Announce() {
		c.Log(line)
	}
	r.Announced = r.announce()
	if err := c.save(r); err != nil {
		return r, err
	}
	return r, fetchErr
}

func (c *Checker) publish() {
	in, err := c.Instance()
	if err != nil {
		return
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	if r, err := c.report(in); err == nil {
		c.save(r)
	}
}

func (c *Checker) save(r Report) error {
	data, err := json.MarshalIndent(r, "", "  ")
	if err != nil {
		return err
	}
	if old, err := os.ReadFile(c.Path); err == nil && bytes.Equal(bytes.TrimSpace(old), data) {
		return nil
	}
	return Save(c.Path, r)
}

func (c *Checker) Hook(ctx context.Context, check bool) (Report, error) {
	if check {
		return c.Check(ctx)
	}
	return c.Report()
}
