// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"sync"
	"time"
)

const (
	progressEvery = 50 * time.Millisecond
	tailLines     = 15
)

type Step struct {
	ui       *UI
	rep      reporter
	mu       sync.Mutex
	id       string
	title    string
	done     string
	logs     []string
	cur      int64
	total    int64
	share    bool
	rate     int64
	waitFor  string
	since    time.Time
	started  time.Time
	logPath  string
	stage    [2]int
	sent     time.Time
	items    map[string][2]int64
	itemSent map[string]time.Time
	state    State
}

func (s *Step) now() time.Time { return s.ui.opts.Now() }

func (s *Step) Title(title string) {
	s.mu.Lock()
	if title != s.title {
		s.cur, s.total, s.rate, s.share = 0, 0, 0, false
	}
	s.title = title
	s.waitFor = ""
	s.since = time.Time{}
	s.mu.Unlock()
	s.rep.Send(stepTitle{id: s.id, title: title})
}

func (s *Step) Waiting(title, waitFor string) {
	s.mu.Lock()
	now := s.now()
	if s.since.IsZero() || waitFor != s.waitFor {
		s.since = now
	}
	s.title, s.waitFor = title, waitFor
	since := s.since
	s.mu.Unlock()
	s.rep.Send(stepWait{id: s.id, title: title, waitFor: waitFor, since: since, at: now})
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

func (s *Step) Warned(format string, args ...any) {
	s.mu.Lock()
	s.done = fmt.Sprintf(format, args...)
	s.state = StateWarn
	s.mu.Unlock()
}

func (s *Step) Info(format string, args ...any) {
	s.ui.Info(format, args...)
}

func (s *Step) Log(line string) {
	line = Inline(line)
	s.mu.Lock()
	s.logs = append(s.logs, line)
	if len(s.logs) > 500 {
		s.logs = append([]string(nil), s.logs[len(s.logs)-200:]...)
	}
	s.mu.Unlock()
	s.rep.Send(stepLog{id: s.id, line: line})
}

func (s *Step) Percent(pct float64) {
	if pct < 0 {
		s.progress(0, 0, true)
		return
	}
	s.progress(int64(min(pct, 100)*10), 1000, true)
}

func (s *Step) Progress(current, total int64) {
	s.progress(current, total, false)
}

func (s *Step) progress(current, total int64, share bool) {
	s.mu.Lock()
	s.cur, s.total, s.share = current, total, share
	now := s.now()
	send := now.Sub(s.sent) >= progressEvery || (total > 0 && current >= total)
	if send {
		s.sent = now
	}
	rate := s.rate
	s.mu.Unlock()
	if send {
		s.rep.Send(stepProgress{id: s.id, cur: current, total: total, rate: rate, share: share, at: now})
	}
}

func (s *Step) Item(id, label string, st State, detail string) {
	s.mu.Lock()
	cur := s.items[id]
	s.mu.Unlock()
	s.rep.Send(stepItem{id: s.id, item: id, label: label, state: st, detail: detail, cur: cur[0], total: cur[1], at: s.now()})
}

func (s *Step) ItemProgress(id string, current, total int64) {
	s.mu.Lock()
	if s.items == nil {
		s.items = map[string][2]int64{}
		s.itemSent = map[string]time.Time{}
	}
	s.items[id] = [2]int64{current, total}
	var cur, all int64
	for _, v := range s.items {
		cur, all = cur+v[0], all+v[1]
	}
	now := s.now()
	send := now.Sub(s.itemSent[id]) >= progressEvery || (total > 0 && current >= total)
	if send {
		s.itemSent[id] = now
	}
	s.mu.Unlock()
	if send {
		s.rep.Send(stepItem{id: s.id, item: id, state: StateActive, cur: current, total: total, at: now})
	}
	s.Progress(cur, all)
}

func (s *Step) Warn(msg, fix string) {
	s.rep.Send(stepWarn{id: s.id, msg: msg, fix: fix})
}

func (s *Step) LogPath(path string) {
	s.mu.Lock()
	s.logPath = path
	s.mu.Unlock()
}

func (s *Step) Stage(n, of int) {
	s.mu.Lock()
	s.stage = [2]int{n, of}
	title := s.title
	s.mu.Unlock()
	s.rep.Send(stageMsg{n: n, of: of, id: s.id, title: title})
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

func (s *Step) tail() []string {
	logs := s.Logs()
	if len(logs) > tailLines {
		logs = logs[len(logs)-tailLines:]
	}
	return logs
}

func (s *Step) snapshot() (title, done, logPath string) {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.title, s.done, s.logPath
}

func (u *UI) Hold() func() {
	u.mu.Lock()
	lr := u.live
	u.mu.Unlock()
	if lr == nil {
		return func() {}
	}
	return lr.release()
}

func (u *UI) nextStepID() string {
	u.mu.Lock()
	defer u.mu.Unlock()
	u.steps++
	return fmt.Sprintf("s%d", u.steps)
}

func (u *UI) Step(ctx context.Context, title string, fn func(ctx context.Context, s *Step) error) error {
	u.mu.Lock()
	u.changed = true
	u.mu.Unlock()
	return u.Check(ctx, title, fn)
}

func (u *UI) Check(ctx context.Context, title string, fn func(ctx context.Context, s *Step) error) error {
	owned := u.beginLive(liveStep, "", nil, nil)
	defer u.endLive(owned)
	return u.runStep(ctx, u.nextStepID(), title, [2]int{}, false, fn)
}

func (u *UI) runStep(ctx context.Context, id, title string, stage [2]int, optional bool, fn func(ctx context.Context, s *Step) error) error {
	rep := u.reporter()
	now := u.opts.Now()
	s := &Step{ui: u, rep: rep, id: id, title: title, started: now, stage: stage}
	u.mu.Lock()
	parent := ""
	if n := len(u.stack); n > 0 {
		parent = u.stack[n-1]
	} else {
		u.failTail, u.failLog, u.failShown = nil, "", nil
	}
	u.stack = append(u.stack, id)
	u.mu.Unlock()
	rep.Send(stepStarted{id: id, parent: parent, title: title, stage: stage, at: now})
	err := func() error {
		defer func() {
			u.mu.Lock()
			u.stack = u.stack[:len(u.stack)-1]
			u.mu.Unlock()
		}()
		return fn(ctx, s)
	}()
	cur, done, logPath := s.snapshot()
	if err != nil {
		msg, fix := failureText(err)
		fix, cause := u.verboseFix(err, fix)
		warn := optional && ctx.Err() == nil
		if ee := (*ExitError)(nil); !warn && u.rich() && errors.As(err, &ee) && causeText(ee) != "" {
			fix = dropVerboseAsk(fix)
		}
		tail := s.tail()
		if !warn {
			u.mu.Lock()
			if u.failTail == nil {
				u.failTail, u.failLog = tail, logPath
			}
			u.failShown = append(u.failShown, msg)
			u.mu.Unlock()
		}
		rep.Send(stepFailed{id: id, msg: msg, fix: fix, cause: cause, logPath: logPath, tail: tail, at: u.opts.Now(), started: now, warn: warn})
		return err
	}
	if done == "" {
		done = cur
	}
	s.mu.Lock()
	state := s.state
	s.mu.Unlock()
	rep.Send(stepDone{id: id, summary: done, state: state, at: u.opts.Now(), started: now})
	return nil
}

func failureText(err error) (string, string) {
	var ee *ExitError
	if errors.As(err, &ee) {
		return exitMessage(ee), ee.Fix
	}
	if errors.Is(err, context.Canceled) {
		return "Stopped before it finished.", ""
	}
	return Sentence(err.Error()), ""
}

func exitMessage(ee *ExitError) string {
	if ee.Msg == "" && ee.Err != nil {
		return Sentence(ee.Err.Error())
	}
	return ee.Msg
}

type Task struct {
	ID       string
	Title    string
	Optional bool
	Skip     func() bool
	Run      func(ctx context.Context, s *Step) error
}

func (u *UI) Tasks(ctx context.Context, title string, tasks ...Task) error {
	return u.tasks(ctx, title, nil, tasks)
}

func (u *UI) tasks(ctx context.Context, title string, cancel func(), tasks []Task) error {
	u.mu.Lock()
	u.changed = true
	u.mu.Unlock()
	ids := make([]string, len(tasks))
	planned := make([]stepPlanned, len(tasks))
	for i, t := range tasks {
		ids[i] = u.nextStepID()
		planned[i] = stepPlanned{id: ids[i], title: t.Title}
	}
	if title != "" && !u.liveCapable() && u.tier != TierJSON && !u.silent() {
		u.Section(title)
	}
	owned := u.beginLive(liveTasks, title, planned, cancel)
	defer u.endLive(owned)
	for i, t := range tasks {
		if t.Skip != nil && t.Skip() {
			u.reporter().Send(stepSkipped{id: ids[i], title: t.Title})
			continue
		}
		stage := [2]int{}
		if len(tasks) > 1 {
			stage = [2]int{i + 1, len(tasks)}
		}
		err := u.runStep(ctx, ids[i], t.Title, stage, t.Optional, t.Run)
		if err == nil {
			continue
		}
		if !t.Optional || ctx.Err() != nil {
			return err
		}
	}
	return nil
}

func Memory(n int64) string {
	const gib = 1 << 30
	v := float64(n) / gib
	if v < 10 {
		return strings.TrimSuffix(fmt.Sprintf("%.1f", v), ".0") + " GB"
	}
	return fmt.Sprintf("%.0f GB", v)
}

func Bytes(n int64) string {
	const unit = 1000
	if n < 0 {
		n = 0
	}
	if n < unit {
		return fmt.Sprintf("%d B", n)
	}
	units := []string{"KB", "MB", "GB", "TB", "PB"}
	v := float64(n) / unit
	i := 0
	for v >= unit-0.05 && i < len(units)-1 {
		v /= unit
		i++
	}
	if v >= 10 {
		return fmt.Sprintf("%.0f %s", v, units[i])
	}
	return fmt.Sprintf("%.1f %s", v, units[i])
}
