// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"context"
	"fmt"
	"io"
	"os"
	"strings"
	"sync"
	"time"

	tea "charm.land/bubbletea/v2"
	"github.com/charmbracelet/colorprofile"
	"github.com/charmbracelet/harmonica"
	"github.com/charmbracelet/x/ansi"
)

const (
	liveFPS       = 30
	spinEvery     = 100 * time.Millisecond
	maxItems      = 6
	maxPending    = 3
	logTailRows   = 6
	stallAfter    = 30 * time.Second
	thinkAfter    = 20 * time.Second
	hopCooldown   = 2 * time.Second
	maxContentCol = 100
	firstFrame    = 60 * time.Millisecond
)

type liveKind int

const (
	liveStep liveKind = iota
	liveTasks
	liveChecks
)

type look struct {
	th     Theme
	g      Glyphs
	still  bool
	rex    bool
	colour bool
	width  int
}

func (u *UI) look() look {
	return look{th: u.themed(), g: u.glyphs, still: u.tier == TierStill, rex: u.ShowsRex(), colour: u.colour, width: u.Width()}
}

type liveRun struct {
	p     *tea.Program
	done  chan struct{}
	mu    sync.Mutex
	final tea.Model
	err   error
	out   *outbox
}

type outbox struct {
	mu     sync.Mutex
	lines  []string
	closed bool
}

type outboxNudge struct{}

func (o *outbox) put(line string) bool {
	o.mu.Lock()
	defer o.mu.Unlock()
	if o.closed {
		return false
	}
	o.lines = append(o.lines, line)
	return true
}

func (o *outbox) take() []string {
	o.mu.Lock()
	defer o.mu.Unlock()
	lines := o.lines
	o.lines = nil
	return lines
}

func (o *outbox) close() []string {
	o.mu.Lock()
	defer o.mu.Unlock()
	o.closed = true
	lines := o.lines
	o.lines = nil
	return lines
}

func (lr *liveRun) print(line string) bool {
	if lr.out == nil || !lr.out.put(line) {
		return false
	}
	lr.p.Send(outboxNudge{})
	return true
}

func (lr *liveRun) Send(msg tea.Msg) { lr.p.Send(msg) }

func (lr *liveRun) release() func() {
	if err := lr.p.ReleaseTerminal(); err != nil {
		return func() {}
	}
	return func() { _ = lr.p.RestoreTerminal() }
}

func (u *UI) reporter() reporter {
	u.mu.Lock()
	lr := u.live
	u.mu.Unlock()
	if lr != nil {
		return lr
	}
	if u.tier == TierJSON {
		return u.jsonRep
	}
	return u.linesRep
}

func (u *UI) liveCapable() bool {
	if u.tier != TierLive && u.tier != TierStill {
		return false
	}
	if !u.stderrTTY || u.opts.Quiet {
		return false
	}
	if _, ok := u.opts.Stderr.(*os.File); !ok {
		return false
	}
	return isTerminal(u.opts.Stdin) || !u.opts.Yes
}

func (u *UI) programOptions(input bool) ([]tea.ProgramOption, io.Closer) {
	var closer io.Closer
	opts := []tea.ProgramOption{
		tea.WithOutput(u.opts.Stderr),
		tea.WithEnvironment(environ(u.opts.Getenv)),
		tea.WithoutSignalHandler(),
		tea.WithoutCatchPanics(),
	}
	if !input {
		opts = append(opts, tea.WithInput(nil))
	} else if !isTerminal(u.opts.Stdin) {
		tty, _, err := tea.OpenTTY()
		if err == nil {
			opts = append(opts, tea.WithInput(tty))
			closer = tty
		}
	} else {
		opts = append(opts, tea.WithInput(u.opts.Stdin))
	}
	if u.colour {
		if p := colorprofile.Detect(u.opts.Stderr, environ(u.opts.Getenv)); p > colorprofile.Ascii {
			opts = append(opts, tea.WithColorProfile(p))
		} else {
			opts = append(opts, tea.WithColorProfile(colorprofile.ANSI256))
		}
	} else {
		opts = append(opts, tea.WithColorProfile(colorprofile.Ascii))
	}
	return opts, closer
}

func (u *UI) startProgram(m tea.Model, input bool, out *outbox) *liveRun {
	lr := &liveRun{done: make(chan struct{}), out: out}
	opts, closer := u.programOptions(input)
	lr.p = tea.NewProgram(m, opts...)
	go func() {
		defer close(lr.done)
		if closer != nil {
			defer closer.Close()
		}
		final, err := lr.p.Run()
		lr.mu.Lock()
		lr.final, lr.err = final, err
		lr.mu.Unlock()
		if out != nil {
			if rest := out.close(); len(rest) > 0 {
				u.mu.Lock()
				_, _ = io.WriteString(u.errw, strings.Join(rest, "\n")+"\n")
				u.mu.Unlock()
			}
		}
	}()
	return lr
}

func (u *UI) runProgram(m tea.Model, input bool) (tea.Model, error) {
	ctx := u.context()
	if ctx.Err() != nil {
		return m, stoppedAtQuestion()
	}
	final, err := u.runProgramCtx(ctx, m, input)
	if err == nil && ctx.Err() != nil {
		return final, stoppedAtQuestion()
	}
	return final, err
}

func (u *UI) runProgramCtx(ctx context.Context, m tea.Model, input bool) (tea.Model, error) {
	release := u.Hold()
	defer release()
	lr := u.startProgram(m, input, nil)
	select {
	case <-lr.done:
	case <-ctx.Done():
		lr.p.Quit()
		<-lr.done
	}
	if lr.final == nil {
		return m, lr.err
	}
	return lr.final, lr.err
}

func (u *UI) beginLive(kind liveKind, title string, planned []stepPlanned, cancel func()) *liveRun {
	u.liveMu.Lock()
	defer u.liveMu.Unlock()
	u.mu.Lock()
	lr := u.live
	u.mu.Unlock()
	if lr != nil {
		for _, p := range planned {
			lr.Send(p)
		}
		return nil
	}
	if !u.liveCapable() {
		return nil
	}
	m := newTaskModel(u.look(), kind, title, planned, cancel != nil, u.opts.Now)
	m.cancel = cancel
	m.readyCh = make(chan struct{})
	m.out = &outbox{}
	if m.headed() && !u.silent() {
		lk := m.lk
		u.emit(u.gap() + lk.th.Accent.Render(lk.g.cell(lk.g.Active)) + "  " + lk.th.Bold.Render(Inline(title)) + "\n")
	}
	lr = u.startProgram(m, cancel != nil, m.out)
	select {
	case <-m.readyCh:
	case <-lr.done:
	case <-time.After(time.Second):
	}
	u.mu.Lock()
	u.live = lr
	u.mu.Unlock()
	return lr
}

func (u *UI) endLive(lr *liveRun) *taskModel {
	if lr == nil {
		return nil
	}
	lr.Send(allDone{})
	<-lr.done
	u.mu.Lock()
	u.live = nil
	u.mu.Unlock()
	if m, ok := lr.final.(*taskModel); ok {
		return m
	}
	return nil
}

type liveRow struct {
	id, parent string
	title      string
	state      State
	planned    bool
	started    time.Time
	waitFor    string
	waitSince  time.Time
	cur, total int64
	share      bool
	rate       int64
	meter      rateMeter
	moved      time.Time
	advanced   time.Time
	items      []*liveItem
	logs       []string
	barPos     float64
	barVel     float64
	depth      int
}

type liveItem struct {
	id, label, detail string
	state             State
	cur, total        int64
	changed           time.Time
	barPos, barVel    float64
}

type rateMeter struct {
	lastAt  time.Time
	lastCur int64
	rate    float64
}

func (r *rateMeter) add(cur int64, at time.Time) {
	if r.lastAt.IsZero() || cur < r.lastCur {
		r.lastAt, r.lastCur = at, cur
		return
	}
	dt := at.Sub(r.lastAt).Seconds()
	if dt < 0.25 {
		return
	}
	inst := float64(cur-r.lastCur) / dt
	if r.rate == 0 {
		r.rate = inst
	} else {
		r.rate = 0.7*r.rate + 0.3*inst
	}
	r.lastAt, r.lastCur = at, cur
}

type taskModel struct {
	lk        look
	kind      liveKind
	title     string
	flow      bool
	now       func() time.Time
	started   time.Time
	rows      []*liveRow
	byID      map[string]*liveRow
	finished  int
	showLogs  bool
	frame     int
	gen       int
	ticking   bool
	springing bool
	spring    harmonica.Spring
	rex       *rexMotion
	lastHop   time.Time
	stopping  int
	cancel    func()
	failed    bool
	done      bool
	width     int
	queue     []string
	kept      []string
	shownHead bool
	flushing  bool
	ready     bool
	readyCh   chan struct{}
	out       *outbox
}

func newTaskModel(lk look, kind liveKind, title string, planned []stepPlanned, flow bool, now func() time.Time) *taskModel {
	m := &taskModel{
		lk: lk, kind: kind, title: title, flow: flow, now: now, started: now(),
		byID: map[string]*liveRow{}, width: lk.width,
		spring: harmonica.NewSpring(harmonica.FPS(liveFPS), 7.0, 1.0),
	}
	if kind == liveTasks && title != "" && lk.rex {
		m.rex = newRexMotion(lk, MoodIdle)
	}
	for _, p := range planned {
		m.plan(p)
	}
	return m
}

func (m *taskModel) plan(p stepPlanned) {
	if _, ok := m.byID[p.id]; ok {
		return
	}
	r := &liveRow{id: p.id, title: p.title, planned: true}
	m.rows = append(m.rows, r)
	m.byID[p.id] = r
}

func (m *taskModel) Init() tea.Cmd {
	return tea.Batch(tea.Tick(firstFrame, func(time.Time) tea.Msg { return readyMsg{} }), m.rexInit())
}

type readyMsg struct{}

func (m *taskModel) rexInit() tea.Cmd {
	if m.rex == nil {
		return nil
	}
	return m.rex.init()
}

func (m *taskModel) active() bool {
	for _, r := range m.rows {
		if r.state == StateActive {
			return true
		}
	}
	return false
}

func (m *taskModel) tick() tea.Cmd {
	if m.ticking || !m.active() {
		return nil
	}
	m.ticking = true
	gen := m.gen
	every := spinEvery
	if m.lk.still {
		every = time.Second
	}
	return tea.Tick(every, func(time.Time) tea.Msg { return tickMsg{gen: gen} })
}

func (m *taskModel) springTick() tea.Cmd {
	if m.lk.still || m.springing {
		return nil
	}
	m.springing = true
	return tea.Tick(time.Second/liveFPS, func(time.Time) tea.Msg { return springMsg{} })
}

func (m *taskModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmds []tea.Cmd
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.lk.width = msg.Width
	case tea.KeyPressMsg:
		cmds = append(cmds, m.key(msg))
	case stepPlanned:
		m.plan(msg)
	case stepStarted:
		r := m.byID[msg.id]
		if r == nil {
			r = &liveRow{id: msg.id}
			m.rows = append(m.rows, r)
			m.byID[msg.id] = r
		}
		r.parent, r.title, r.state, r.started, r.moved = msg.parent, msg.title, StateActive, msg.at, msg.at
		if p := m.byID[msg.parent]; p != nil {
			r.depth = p.depth + 1
		}
		if m.rex != nil {
			m.rex.setMood(MoodIdle)
		}
		cmds = append(cmds, m.tick())
	case stepTitle:
		if r := m.byID[msg.id]; r != nil {
			if r.title != msg.title {
				r.cur, r.total, r.rate, r.share, r.meter, r.barPos, r.barVel = 0, 0, 0, false, rateMeter{}, 0, 0
			}
			r.title, r.waitFor = msg.title, ""
		}
	case stepWait:
		if r := m.byID[msg.id]; r != nil {
			r.title, r.waitFor, r.waitSince = msg.title, msg.waitFor, msg.since
		}
	case stepProgress:
		if r := m.byID[msg.id]; r != nil {
			if msg.cur != r.cur {
				r.moved, r.advanced = msg.at, msg.at
			}
			r.cur, r.total, r.rate, r.share = msg.cur, msg.total, msg.rate, msg.share
			r.meter.add(msg.cur, msg.at)
			if m.lk.still {
				r.barPos = r.fraction()
			}
			cmds = append(cmds, m.springTick())
		}
	case stepItem:
		if r := m.byID[msg.id]; r != nil {
			r.moved = msg.at
			r.item(msg, m.lk.still)
			cmds = append(cmds, m.springTick())
		}
	case stepLog:
		if r := m.byID[msg.id]; r != nil {
			r.logs = append(r.logs, msg.line)
			if len(r.logs) > 64 {
				r.logs = r.logs[len(r.logs)-logTailRows:]
			}
		}
	case stepWarn:
		cmds = append(cmds, m.print(m.warnLine(msg.msg, msg.fix)))
	case stepSkipped:
		if r := m.byID[msg.id]; r != nil {
			r.state = StateSkipped
			cmds = append(cmds, m.flush(r, time.Time{}))
		}
	case stepDone:
		if r := m.byID[msg.id]; r != nil {
			r.state = msg.state
			if r.state == StatePending || r.state == StateActive {
				r.state = StateOK
			}
			if msg.summary != "" {
				r.title = msg.summary
			}
			m.finished++
			cmds = append(cmds, m.flush(r, msg.at), m.hop(msg.at))
		}
	case stepFailed:
		if r := m.byID[msg.id]; r != nil {
			if msg.warn {
				r.state = StateWarn
				cmds = append(cmds, m.flush(r, msg.at), m.print(m.warnLine(msg.msg, msg.fix)))
				break
			}
			r.state = StateFail
			m.failed = true
			if m.rex != nil {
				m.rex.setMood(MoodSad)
			}
			cmds = append(cmds, m.flush(r, msg.at))
		}
	case tickMsg:
		if msg.gen != m.gen {
			break
		}
		m.ticking = false
		m.frame++
		if m.rex != nil {
			m.rex.think(m.waiting())
		}
		cmds = append(cmds, m.tick())
	case springMsg:
		m.springing = false
		if m.settle() {
			cmds = append(cmds, m.springTick())
		}
	case readyMsg:
		if !m.ready {
			m.ready = true
			if m.readyCh != nil {
				close(m.readyCh)
			}
		}
		cmds = append(cmds, m.drain())
	case outboxNudge:
		if m.out != nil {
			m.queue = append(m.queue, m.out.take()...)
		}
		return m, m.drain()
	case printedMsg:
		m.flushing = false
		if len(m.queue) > 0 {
			return m, m.drain()
		}
		if m.done {
			return m, tea.Quit
		}
		return m, nil
	case allDone:
		m.done = true
		if m.flushing || len(m.queue) > 0 {
			return m, nil
		}
		return m, tea.Quit
	}
	if m.rex != nil {
		cmds = append(cmds, m.rex.update(msg))
	}
	return m, tea.Batch(cmds...)
}

func (m *taskModel) key(msg tea.KeyPressMsg) tea.Cmd {
	switch msg.String() {
	case "l":
		m.showLogs = !m.showLogs
	case "ctrl+c":
		m.stopping++
		if m.cancel != nil {
			m.cancel()
		}
		if m.stopping > 1 {
			interruptSelf()
			return tea.Quit
		}
	}
	return nil
}

func (m *taskModel) waiting() bool {
	now := m.now()
	for _, r := range m.rows {
		if r.state == StateActive && !r.moved.IsZero() && now.Sub(r.moved) > thinkAfter {
			return true
		}
	}
	return false
}

func (m *taskModel) hop(at time.Time) tea.Cmd {
	if m.rex == nil || at.Sub(m.lastHop) < hopCooldown {
		return nil
	}
	m.lastHop = at
	return m.rex.hop()
}

func (r *liveRow) fraction() float64 {
	if r.total <= 0 {
		return 0
	}
	return min(1, float64(r.cur)/float64(r.total))
}

func (r *liveRow) item(msg stepItem, still bool) {
	var it *liveItem
	for _, x := range r.items {
		if x.id == msg.item {
			it = x
			break
		}
	}
	if it == nil {
		it = &liveItem{id: msg.item, label: msg.item}
		r.items = append(r.items, it)
	}
	if msg.label != "" {
		it.label = msg.label
	}
	if msg.label != "" || msg.detail != "" || msg.state != StateActive || it.state == StatePending {
		if msg.detail != it.detail || msg.state != it.state {
			it.changed = msg.at
		}
		it.detail, it.state = msg.detail, msg.state
	}
	if msg.total > 0 || msg.cur > 0 {
		if msg.cur != it.cur {
			it.changed = msg.at
		}
		it.cur, it.total = msg.cur, msg.total
		if still {
			it.barPos = it.fraction()
		}
	}
	if it.changed.IsZero() {
		it.changed = msg.at
	}
}

func (it *liveItem) fraction() float64 {
	if it.total <= 0 {
		return 0
	}
	return min(1, float64(it.cur)/float64(it.total))
}

func (m *taskModel) settle() bool {
	moving := false
	step := func(pos, vel *float64, target float64) {
		*pos, *vel = m.spring.Update(*pos, *vel, target)
		if abs(*pos-target) < 0.001 && abs(*vel) < 0.001 {
			*pos, *vel = target, 0
			return
		}
		moving = true
	}
	for _, r := range m.rows {
		if r.state != StateActive {
			continue
		}
		step(&r.barPos, &r.barVel, r.fraction())
		for _, it := range r.items {
			step(&it.barPos, &it.barVel, it.fraction())
		}
	}
	return moving
}

func abs(f float64) float64 {
	if f < 0 {
		return -f
	}
	return f
}

func (m *taskModel) flush(r *liveRow, at time.Time) tea.Cmd {
	line := m.finishedLine(r, at)
	m.remove(r)
	if !m.active() {
		m.gen++
		m.ticking = false
	}
	if m.kind == liveChecks && (r.state == StateOK || r.state == StateSkipped) {
		m.kept = append(m.kept, line)
		return nil
	}
	return m.print(line)
}

func (m *taskModel) checksHead() string {
	lk := m.lk
	return lk.th.state(StateActive).Render(lk.g.cell(lk.g.state(StateActive))) + "  " + lk.th.Bold.Render(Inline(m.title))
}

func (m *taskModel) unkeep() []string {
	if m.kind != liveChecks {
		return nil
	}
	var out []string
	if m.title != "" && !m.shownHead {
		out = append(out, m.checksHead())
		m.shownHead = true
	}
	out = append(out, m.kept...)
	m.kept = nil
	return out
}

type printedMsg struct{}

func (m *taskModel) print(line string) tea.Cmd {
	m.queue = append(append(m.queue, m.unkeep()...), line)
	return m.drain()
}

func (m *taskModel) drain() tea.Cmd {
	if !m.ready || m.flushing || len(m.queue) == 0 {
		return nil
	}
	m.flushing = true
	text := strings.Join(m.queue, "\n")
	m.queue = nil
	return tea.Sequence(tea.Println(text), func() tea.Msg { return printedMsg{} })
}

func (m *taskModel) remove(r *liveRow) {
	for i, x := range m.rows {
		if x == r {
			m.rows = append(m.rows[:i], m.rows[i+1:]...)
			break
		}
	}
	delete(m.byID, r.id)
}

func (m *taskModel) railed() bool {
	return m.kind == liveChecks || m.kind == liveTasks && m.title != ""
}

func (m *taskModel) headed() bool { return m.kind == liveTasks && m.title != "" }

func (m *taskModel) indent(r *liveRow) string {
	pre := ""
	if m.railed() {
		pre = m.lk.th.Border.Render(m.lk.g.Rail) + "  "
	}
	return pre + strings.Repeat("  ", r.depth)
}

func (m *taskModel) finishedLine(r *liveRow, at time.Time) string {
	lk := m.lk
	glyph := lk.th.state(r.state).Render(lk.g.cell(lk.g.state(r.state)))
	prefix := m.indent(r) + glyph + " "
	text := Inline(r.title)
	if r.state == StateSkipped {
		text += lk.th.Muted.Render(", skipped")
	}
	right := ""
	if !at.IsZero() && !r.started.IsZero() {
		right = lk.th.Muted.Render(Elapsed(at.Sub(r.started)))
	}
	room := m.contentWidth() - ansi.StringWidth(prefix)
	if right != "" {
		room -= ansi.StringWidth(right) + 2
	}
	if room < 20 || ansi.StringWidth(text) <= room {
		return m.fit(prefix+text, right)
	}
	hang := hangOf(ansi.Strip(text))
	if hang > room/2 {
		hang = 0
	}
	lines := strings.Split(wordwrapKeepHyphens(text, room), "\n")
	out := []string{m.fit(prefix+lines[0], right)}
	pad := m.indent(r) + strings.Repeat(" ", ansi.StringWidth(glyph)+1+hang)
	rest := strings.TrimLeft(strings.Join(lines[1:], " "), " ")
	for _, l := range strings.Split(wordwrapKeepHyphens(rest, m.contentWidth()-ansi.StringWidth(pad)), "\n") {
		out = append(out, m.fit(pad+l, ""))
	}
	return strings.Join(out, "\n")
}

func hangOf(text string) int {
	i := strings.Index(text, "  ")
	if i <= 0 {
		return 0
	}
	return i + len(text[i:]) - len(strings.TrimLeft(text[i:], " "))
}

func (m *taskModel) warnLine(msg, fix string) string {
	lk := m.lk
	pre := ""
	if m.railed() {
		pre = lk.th.Border.Render(lk.g.Rail) + "  "
	}
	pad := pre + "  "
	room := max(20, m.contentWidth()-ansi.StringWidth(pad))
	var out []string
	for i, l := range strings.Split(wordwrapKeepHyphens(Inline(msg), room), "\n") {
		if i == 0 {
			out = append(out, pre+lk.th.Warn.Render(lk.g.cell(lk.g.Warn))+" "+l)
			continue
		}
		out = append(out, pad+l)
	}
	if fix != "" {
		for _, l := range strings.Split(wordwrapKeepHyphens(Inline(fix), room), "\n") {
			out = append(out, pad+l)
		}
	}
	return strings.Join(out, "\n")
}

func (m *taskModel) contentWidth() int {
	w := m.width
	if w <= 0 {
		w = 80
	}
	return min(w, maxContentCol)
}

func (m *taskModel) fit(left, right string) string {
	w := m.contentWidth()
	if right == "" {
		return ansi.Truncate(left, w, m.lk.g.Ellipsis)
	}
	rw := ansi.StringWidth(right)
	room := w - rw - 2
	if room < 10 {
		return ansi.Truncate(left, w, m.lk.g.Ellipsis)
	}
	left = ansi.Truncate(left, room, m.lk.g.Ellipsis)
	return left + strings.Repeat(" ", w-ansi.StringWidth(left)-rw) + right
}

func (m *taskModel) spinner() string {
	if m.lk.still {
		return m.lk.g.Active
	}
	frames := m.lk.g.Spinner
	return frames[m.frame%len(frames)]
}

func (m *taskModel) View() tea.View {
	if m.done {
		v := tea.NewView("")
		v.ProgressBar = tea.NewProgressBar(tea.ProgressBarNone, 0)
		v.ProgramStatus = &tea.ProgramStatus{State: tea.ProgramStateClear, App: "rexctl"}
		return v
	}
	var lines []string
	if m.kind == liveChecks && m.title != "" && !m.shownHead {
		lines = append(lines, m.checksHead())
	}
	lines = append(lines, m.kept...)
	pending := 0
	for _, r := range m.rows {
		if r.state != StateActive {
			pending++
			if pending > maxPending {
				continue
			}
		}
		lines = append(lines, m.rowLines(r)...)
	}
	if more := pending - maxPending; more > 0 {
		lines = append(lines, m.fit(m.indent(&liveRow{})+m.lk.th.Muted.Render(m.lk.g.cell(m.lk.g.Pending)+" "+fmt.Sprintf("and %d more", more)), ""))
	}
	if m.headed() {
		lines = append(lines, m.footer()...)
	}
	if hint := m.hints(); hint != "" {
		lines = append(lines, hint)
	}
	v := tea.NewView(strings.Join(lines, "\n"))
	v.ProgressBar = m.progressBar()
	v.ProgramStatus = m.programStatus(v.ProgressBar)
	return v
}

func (m *taskModel) programStatus(pb *tea.ProgressBar) *tea.ProgramStatus {
	st := &tea.ProgramStatus{State: tea.ProgramStateWorking, App: "rexctl", Title: Inline(m.title)}
	if m.failed {
		st.State = tea.ProgramStateError
		return st
	}
	if pb != nil && pb.State == tea.ProgressBarDefault {
		st.Progress, st.HasProgress = pb.Value, true
	}
	return st
}

func (m *taskModel) footer() []string {
	lk := m.lk
	total := m.finished + len(m.rows)
	rail := lk.th.Border.Render(lk.g.Rail)
	count := lk.th.Muted.Render(fmt.Sprintf("%d of %d done, %s", m.finished, total, Elapsed(m.now().Sub(m.started))))
	bar := renderBar(lk, float64(m.percent(total))/100, max(8, min(28, m.contentWidth()-40)))
	if m.rex == nil {
		return []string{rail, m.fit(rail+"  "+bar+"  "+count, "")}
	}
	side := []string{lk.th.Bold.Render(Inline(m.title)), bar + "  " + count}
	if m.rex.mood == MoodThink {
		side = append(side, lk.th.Muted.Render(rexThinkLine))
	}
	var out []string
	for _, l := range m.rex.view(m.contentWidth()-2, side) {
		out = append(out, strings.TrimRight(m.fit(rail+" "+l, ""), " "))
	}
	return out
}

func (m *taskModel) rowLines(r *liveRow) []string {
	lk := m.lk
	ind := m.indent(r)
	if r.state != StateActive {
		return []string{m.fit(ind+lk.th.Muted.Render(lk.g.cell(lk.g.Pending)+" "+Inline(r.title)), "")}
	}
	title := Inline(r.title)
	now := m.now()
	if r.waitFor != "" {
		title += lk.th.Muted.Render(", waiting for " + Inline(r.waitFor) + " (" + Elapsed(now.Sub(r.waitSince)) + ")")
	}
	if p := m.progressText(r); p != "" {
		title += "  " + lk.th.Muted.Render(p)
	}
	elapsed := lk.th.Muted.Render(Elapsed(now.Sub(r.started)))
	out := []string{m.fit(ind+lk.th.Accent.Render(lk.g.cell(m.spinner()))+" "+title, elapsed)}
	if r.total > 0 {
		out = append(out, m.barLine(ind+"  ", r.barPos, r.fraction()))
	}
	out = append(out, m.itemLines(ind+"  ", r)...)
	if m.showLogs {
		for _, l := range tail(r.logs, logTailRows) {
			out = append(out, m.fit(ind+"  "+lk.th.Muted.Render(lk.g.Rail+" "+l), ""))
		}
	}
	return out
}

func tail[T any](s []T, n int) []T {
	if len(s) <= n {
		return s
	}
	return s[len(s)-n:]
}

func (m *taskModel) progressText(r *liveRow) string {
	if r.share {
		return ""
	}
	rate := r.effectiveRate()
	if !r.advanced.IsZero() && m.now().Sub(r.advanced) > staleRate {
		rate = 0
	}
	return progressSummary(r.cur, r.total, rate)
}

const staleRate = 3 * time.Second

func (r *liveRow) effectiveRate() int64 {
	if r.rate > 0 {
		return r.rate
	}
	return int64(r.meter.rate)
}

func progressSummary(cur, total, rate int64) string {
	if cur <= 0 && total <= 0 || total > 0 && cur >= total {
		return ""
	}
	parts := []string{Bytes(cur)}
	if total > 0 {
		parts[0] += " of " + Bytes(total)
	}
	if rate > 0 {
		parts = append(parts, Bytes(rate)+"/s")
		if total > cur {
			left := time.Duration(float64(total-cur)/float64(rate)) * time.Second
			parts = append(parts, Elapsed(left)+" left")
		}
	}
	return strings.Join(parts, ", ")
}

func (m *taskModel) barLine(ind string, pos, target float64) string {
	w := m.contentWidth() - ansi.StringWidth(ind) - 6
	w = max(8, min(w, 60))
	bar := renderBar(m.lk, pos, w)
	return ind + bar + " " + m.lk.th.Muted.Render(fmt.Sprintf("%3d%%", int(target*100+0.5)))
}

func renderBar(lk look, pos float64, w int) string {
	pos = max(0, min(1, pos))
	filled := int(pos * float64(w))
	if filled >= w {
		return lk.th.AccentStrong.Render(strings.Repeat(lk.g.BarFull, w))
	}
	head := ""
	if filled > 0 || pos > 0 {
		head = lk.g.BarHead
	}
	empty := w - filled - ansi.StringWidth(head)
	return lk.th.AccentStrong.Render(strings.Repeat(lk.g.BarFull, filled)+head) + lk.th.Border.Render(strings.Repeat(lk.g.BarEmpty, max(0, empty)))
}

func (m *taskModel) itemLines(ind string, r *liveRow) []string {
	if len(r.items) == 0 {
		return nil
	}
	lk := m.lk
	label := 0
	for _, it := range r.items {
		label = max(label, ansi.StringWidth(Inline(it.label)))
	}
	label = min(label, 24)
	shown := r.items
	more, done := 0, 0
	if len(shown) > maxItems {
		var keep []*liveItem
		for _, it := range r.items {
			if it.state == StateActive || it.state == StateFail || it.state == StateWarn {
				keep = append(keep, it)
			}
		}
		for _, it := range r.items {
			if len(keep) >= maxItems {
				break
			}
			if it.state != StateActive && it.state != StateFail && it.state != StateWarn {
				keep = append(keep, it)
			}
		}
		if len(keep) > maxItems {
			keep = keep[:maxItems]
		}
		more = len(r.items) - len(keep)
		shown = keep
		for _, it := range r.items {
			if it.state == StateOK {
				done++
			}
		}
		for _, it := range keep {
			if it.state == StateOK {
				done--
			}
		}
	}
	now := m.now()
	var out []string
	for _, it := range shown {
		name := ansi.Truncate(Inline(it.label), label, lk.g.Ellipsis)
		name += strings.Repeat(" ", label-ansi.StringWidth(name))
		pre := ind + lk.th.Border.Render(lk.g.Rail) + " "
		var rest string
		switch {
		case it.total > 0 && it.state == StateActive:
			w := max(6, min(20, m.contentWidth()-ansi.StringWidth(ind)-label-24))
			rest = renderBar(lk, it.barPos, w) + "  " + lk.th.Muted.Render(Bytes(it.cur)+" of "+Bytes(it.total))
		case it.state == StateActive:
			rest = lk.th.Muted.Render(Inline(cmpOr(it.detail, "working")))
		default:
			word := stateWord(it.state)
			if it.total > 0 && it.state == StateOK {
				word = Bytes(it.total) + "  " + word
			}
			rest = lk.th.state(it.state).Render(word)
			if it.detail != "" {
				rest += lk.th.Muted.Render(gap + Inline(it.detail))
			}
		}
		if it.state == StateActive && it.total <= 0 && !it.changed.IsZero() && now.Sub(it.changed) >= stallAfter {
			rest = lk.th.Muted.Render("still " + Inline(cmpOr(it.detail, "working")) + ", " + Elapsed(now.Sub(it.changed)))
		}
		out = append(out, m.fit(pre+name+"  "+rest, ""))
	}
	if more > 0 {
		out = append(out, m.fit(ind+lk.th.Border.Render(lk.g.Rail)+" "+lk.th.Muted.Render(moreItems(more, done)), ""))
	}
	return out
}

func moreItems(more, done int) string {
	switch done {
	case 0:
		return fmt.Sprintf("and %d more", more)
	case more:
		return fmt.Sprintf("and %d more, all done", more)
	}
	return fmt.Sprintf("and %d more, %d of them done", more, done)
}

func cmpOr(a, b string) string {
	if a != "" {
		return a
	}
	return b
}

func stateWord(st State) string {
	switch st {
	case StateOK:
		return "done"
	case StateWarn:
		return "warning"
	case StateFail:
		return "failed"
	case StateSkipped:
		return "skipped"
	case StateActive:
		return "working"
	}
	return "waiting"
}

func (m *taskModel) hints() string {
	lk := m.lk
	if m.stopping > 0 {
		return m.fit(m.footPrefix()+lk.th.Warn.Render(stoppingLine), "")
	}
	var keys [][2]string
	if m.flow {
		keys = append(keys, [2]string{"l", "logs"}, [2]string{"ctrl+c", "stop"})
	}
	if len(keys) == 0 {
		if m.railed() {
			return m.lk.th.Border.Render(lk.g.RailBottom)
		}
		return ""
	}
	return m.fit(m.footPrefix()+keyHints(lk, keys), "")
}

func (m *taskModel) footPrefix() string {
	if m.railed() {
		return m.lk.th.Border.Render(m.lk.g.RailBottom) + "  "
	}
	return ""
}

func keyHints(lk look, keys [][2]string) string {
	parts := make([]string, len(keys))
	for i, k := range keys {
		parts[i] = lk.th.Accent.Bold(true).Render(k[0]) + " " + lk.th.Muted.Render(k[1])
	}
	return strings.Join(parts, gap)
}

func (m *taskModel) progressBar() *tea.ProgressBar {
	total := m.finished + len(m.rows)
	if m.failed {
		return tea.NewProgressBar(tea.ProgressBarError, m.percent(total))
	}
	if m.kind == liveTasks && total > 0 {
		return tea.NewProgressBar(tea.ProgressBarDefault, m.percent(total))
	}
	for _, r := range m.rows {
		if r.state == StateActive && r.total > 0 {
			return tea.NewProgressBar(tea.ProgressBarDefault, int(r.fraction()*100))
		}
	}
	return tea.NewProgressBar(tea.ProgressBarIndeterminate, 0)
}

func (m *taskModel) percent(total int) int {
	if total == 0 {
		return 0
	}
	part := 0.0
	for _, r := range m.rows {
		if r.state == StateActive && r.depth == 0 {
			part = r.fraction()
			break
		}
	}
	return int((float64(m.finished) + part) * 100 / float64(total))
}
