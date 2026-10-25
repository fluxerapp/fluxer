// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"context"
	"fmt"
	"strings"
	"sync"
	"time"

	"charm.land/lipgloss/v2"
	"github.com/charmbracelet/x/ansi"
	"golang.org/x/term"
)

var spinnerFrames = []string{"⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"}

const spinnerInterval = 80 * time.Millisecond

type Step struct {
	ui        *UI
	mu        sync.Mutex
	title     string
	done      string
	logs      []string
	cur       int64
	total     int64
	rate      int64
	waitFor   string
	since     time.Time
	announce  bool
	announced string
}

func (s *Step) Title(title string) {
	s.mu.Lock()
	s.title = title
	s.waitFor = ""
	s.mu.Unlock()
}

func (s *Step) Waiting(title, waitFor string) {
	s.mu.Lock()
	if s.since.IsZero() {
		s.since = time.Now()
	}
	changed := waitFor != s.announced
	s.title, s.waitFor, s.announced = title, waitFor, waitFor
	line := s.titleLocked()
	s.mu.Unlock()
	if changed && s.announce {
		s.ui.emit(line + "\n")
	}
}

func (s *Step) titleLocked() string {
	if s.waitFor == "" {
		return s.title
	}
	return s.title + ", waiting for " + s.waitFor + " (" + Elapsed(time.Since(s.since)) + ")"
}

func Elapsed(d time.Duration) string {
	d = d.Round(time.Second)
	switch {
	case d >= time.Hour:
		return fmt.Sprintf("%dh %dm", int(d.Hours()), int(d.Minutes())%60)
	case d >= time.Minute:
		return fmt.Sprintf("%dm %ds", int(d.Minutes()), int(d.Seconds())%60)
	}
	return fmt.Sprintf("%ds", int(d.Seconds()))
}

func (s *Step) Done(format string, args ...any) {
	s.mu.Lock()
	s.done = fmt.Sprintf(format, args...)
	s.mu.Unlock()
}

func (s *Step) Info(format string, args ...any) {
	s.ui.Info(format, args...)
}

func (s *Step) Log(line string) {
	s.mu.Lock()
	s.logs = append(s.logs, line)
	s.mu.Unlock()
	s.ui.Detail("%s", line)
}

func (s *Step) Progress(current, total int64) {
	s.mu.Lock()
	s.cur, s.total = current, total
	s.mu.Unlock()
}

func (s *Step) Rate(bytesPerSecond int64) {
	s.mu.Lock()
	s.rate = bytesPerSecond
	s.mu.Unlock()
}

func (s *Step) Logs() []string {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]string(nil), s.logs...)
}

func (s *Step) status() (string, string) {
	s.mu.Lock()
	defer s.mu.Unlock()
	title := s.titleLocked()
	if s.total <= 0 && s.cur <= 0 {
		return title, ""
	}
	progress := Bytes(s.cur)
	if s.total > 0 {
		progress += " of " + Bytes(s.total)
	}
	if s.rate > 0 {
		progress += ", " + Bytes(s.rate) + "/s"
	}
	return title, progress
}

func (s *Step) doneText() string {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.done
}

type spinner struct {
	u      *UI
	step   *Step
	accent lipgloss.Style
	frame  int
	width  int
	drawn  bool
	held   bool
	stop   chan struct{}
	wg     sync.WaitGroup
}

func (sp *spinner) clear() {
	if sp.drawn {
		fmt.Fprint(sp.u.errw, "\r"+ansi.EraseEntireLine)
		sp.drawn = false
	}
}

func (sp *spinner) draw() {
	title, progress := sp.step.status()
	text := title
	if progress != "" {
		text += "  " + progress
	}
	if sp.width > 3 {
		text = ansi.Truncate(text, sp.width-3, "")
	}
	frame := sp.accent.Render(spinnerFrames[sp.frame%len(spinnerFrames)])
	fmt.Fprint(sp.u.errw, "\r"+ansi.EraseEntireLine+frame+" "+text)
	sp.drawn = true
}

func (sp *spinner) run() {
	defer sp.wg.Done()
	t := time.NewTicker(spinnerInterval)
	defer t.Stop()
	for {
		select {
		case <-sp.stop:
			return
		case <-t.C:
			sp.u.mu.Lock()
			sp.frame++
			if !sp.held {
				sp.draw()
			}
			sp.u.mu.Unlock()
		}
	}
}

func (u *UI) spins() bool {
	return u.mode == ModeRich && !u.opts.Quiet && !u.accessible && u.stderrTTY
}

func (u *UI) startSpinner(s *Step) *spinner {
	accent := u.themed().Accent
	u.mu.Lock()
	defer u.mu.Unlock()
	if u.spin != nil {
		return nil
	}
	sp := &spinner{u: u, step: s, accent: accent, stop: make(chan struct{})}
	if f, ok := u.opts.Stderr.(interface{ Fd() uintptr }); ok {
		if w, _, err := term.GetSize(int(f.Fd())); err == nil {
			sp.width = w
		}
	}
	u.spin = sp
	sp.draw()
	sp.wg.Add(1)
	go sp.run()
	return sp
}

func (u *UI) stopSpinner(sp *spinner) {
	if sp == nil {
		return
	}
	close(sp.stop)
	sp.wg.Wait()
	u.mu.Lock()
	sp.clear()
	u.spin = nil
	u.mu.Unlock()
}

func (u *UI) Hold() func() {
	u.mu.Lock()
	defer u.mu.Unlock()
	sp := u.spin
	if sp == nil {
		return func() {}
	}
	sp.clear()
	sp.held = true
	return func() {
		u.mu.Lock()
		sp.held = false
		u.mu.Unlock()
	}
}

func (u *UI) Step(ctx context.Context, title string, fn func(ctx context.Context, s *Step) error) error {
	s := &Step{ui: u, title: title}
	var sp *spinner
	switch {
	case u.spins():
		sp = u.startSpinner(s)
	case u.mode != ModeJSON && !u.opts.Quiet:
		s.announce = true
		if u.accessible || u.opts.Verbose {
			u.emit(title + "\n")
		}
	}
	err := fn(ctx, s)
	u.stopSpinner(sp)
	if err != nil {
		if !u.opts.Verbose {
			if logs := s.Logs(); len(logs) > 0 {
				u.emit(u.theme.Dim.Render(strings.Join(logs, "\n")) + "\n")
			}
		}
		return err
	}
	if done := s.doneText(); done != "" {
		u.OK("%s", done)
	}
	return nil
}

func Bytes(n int64) string {
	const unit = 1000
	if n < unit {
		return fmt.Sprintf("%d B", n)
	}
	units := []string{"KB", "MB", "GB", "TB", "PB"}
	v := float64(n) / unit
	i := 0
	for v >= unit && i < len(units)-1 {
		v /= unit
		i++
	}
	s := fmt.Sprintf("%.1f", v)
	s = strings.TrimSuffix(s, ".0")
	return s + " " + units[i]
}
