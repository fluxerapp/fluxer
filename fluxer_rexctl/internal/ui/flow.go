// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"context"
	"fmt"
	"slices"
	"strings"
	"unicode"

	tea "charm.land/bubbletea/v2"
	"charm.land/huh/v2"
	"github.com/charmbracelet/x/ansi"
	"github.com/spf13/cobra"
)

type Stage struct {
	ID      string
	Title   string
	Key     rune
	Fields  []Field
	Hide    func() bool
	Checks  func(ctx context.Context, c *Checklist) error
	After   func(ctx context.Context, c *Checklist) error
	Summary func() string
}

type Flow struct {
	Title    string
	Subtitle string
	Greeting bool
	Stages   []*Stage
	Review   func() Card
	Tasks    func() []Task
	Done     func() Success
}

type Checklist struct {
	u     *UI
	ctx   context.Context
	err   error
	noted bool
}

func (c *Checklist) Check(id, label string, fn func(ctx context.Context) (State, string, error)) {
	var st State
	var detail string
	var ferr error
	_ = c.u.runStep(c.ctx, "check."+id, label, [2]int{}, false, func(ctx context.Context, s *Step) error {
		st, detail, ferr = fn(ctx)
		if ferr != nil {
			if st == StatePending || st == StateActive || st == StateOK {
				st = StateFail
			}
			if detail == "" {
				detail = Sentence(ferr.Error())
			}
		}
		s.mu.Lock()
		s.done = checkSummary(label, detail)
		s.state = st
		s.mu.Unlock()
		return nil
	})
	if ferr != nil && c.err == nil {
		c.err = ferr
	}
	if st != StateOK && st != StateSkipped {
		c.noted = true
	}
}

func checkSummary(label, detail string) string {
	if detail == "" {
		return label
	}
	return padRight(label, 14) + "  " + detail
}

func (c *Checklist) Recover(question string, options []Option) (string, error) {
	if len(options) == 0 || !c.u.interactive {
		return "", needsAnswer(question, "")
	}
	v := options[0].Value
	if err := c.u.Ask(nil, Field{Title: question, Value: &v, Options: options, Kind: KindSelect}); err != nil {
		return "", err
	}
	c.err = nil
	return v, nil
}

type flowRun struct {
	u     *UI
	cmd   *cobra.Command
	f     *Flow
	ctx   context.Context
	live  bool
	verb  string
	asked map[*bool]bool
}

func (u *UI) RunFlow(ctx context.Context, cmd *cobra.Command, f *Flow) error {
	r := &flowRun{u: u, cmd: cmd, f: f, ctx: ctx, verb: flowVerb(f.Title), asked: map[*bool]bool{}}
	r.live = u.interactive && u.rich() && u.liveCapable()
	if f.Greeting {
		u.Welcome()
	}
	r.header()
	i := 0
	for {
		stages := r.visible()
		if i >= len(stages) {
			break
		}
		st := stages[i]
		back, err := r.stage(st, i+1, r.total(stages))
		if err != nil {
			return err
		}
		if back {
			if i == 0 {
				if err := r.askStop(); err != nil {
					return err
				}
				continue
			}
			i--
			continue
		}
		i++
	}
	if f.Review != nil {
		stages := r.visible()
		u.announceStage(len(stages)+1, r.total(stages), "review", "Review")
		if err := r.reviewLoop(); err != nil {
			return err
		}
	}
	if f.Tasks != nil {
		stages := r.visible()
		n := r.total(stages)
		u.announceStage(n, n, "tasks", r.tasksTitle())
		tctx, cancel := context.WithCancel(ctx)
		defer cancel()
		var stop func()
		if r.live {
			stop = cancel
		}
		err := u.tasks(tctx, r.tasksTitle(), stop, f.Tasks())
		if tctx.Err() != nil && ctx.Err() == nil {
			return Interrupted(commandPath(cmd))
		}
		if err != nil {
			return err
		}
	}
	if f.Done != nil {
		u.Success(f.Done())
	}
	return nil
}

func commandPath(cmd *cobra.Command) string {
	if cmd == nil {
		return ""
	}
	return cmd.CommandPath()
}

func flowVerb(title string) string {
	f := strings.Fields(title)
	if len(f) == 0 {
		return "go"
	}
	return strings.ToLower(f[len(f)-1])
}

func (r *flowRun) tasksTitle() string {
	v := []rune(r.verb)
	if len(v) == 0 {
		return "Working"
	}
	v[0] = unicode.ToUpper(v[0])
	return string(v)
}

func (r *flowRun) visible() []*Stage {
	var out []*Stage
	for _, st := range r.f.Stages {
		if st != nil && (st.Hide == nil || !st.Hide()) {
			out = append(out, st)
		}
	}
	return out
}

func (r *flowRun) total(stages []*Stage) int {
	n := len(stages)
	if r.f.Review != nil {
		n++
	}
	if r.f.Tasks != nil {
		n++
	}
	return n
}

func (r *flowRun) header() {
	u := r.u
	if r.f.Title == "" || u.silent() {
		return
	}
	if !u.rich() {
		head := r.f.Title
		if r.f.Subtitle != "" {
			head += ", " + r.f.Subtitle
		}
		u.emit(Inline(head) + "\n")
		return
	}
	lk := u.look()
	w := min(u.Width(), maxContentCol)
	left := lk.th.Border.Render(lk.g.RailTop) + "  " + lk.th.Bold.Render(Inline(r.f.Title))
	right := lk.th.Muted.Render(Inline(r.f.Subtitle))
	line := left
	if pad := w - ansi.StringWidth(left) - ansi.StringWidth(right); r.f.Subtitle != "" && pad >= 2 {
		line += strings.Repeat(" ", pad) + right
	}
	u.emit(line + "\n" + lk.th.Border.Render(lk.g.Rail) + "\n")
}

func (u *UI) announceStage(n, of int, id, title string) {
	if u.tier == TierJSON {
		u.jsonRep.event("stage", "n", n, "of", of, "id", id, "title", printable(title))
		return
	}
	if u.silent() || title == "" || u.rich() {
		return
	}
	if u.accessible {
		u.emit(fmt.Sprintf("Stage %d of %d, %s\n", n, of, Inline(title)))
		return
	}
	u.emit(fmt.Sprintf("[%d/%d] %s\n", n, of, Inline(title)))
}

func (r *flowRun) stage(st *Stage, n, of int) (bool, error) {
	u := r.u
	u.announceStage(n, of, st.ID, st.Title)
	headed := false
	if st.Checks != nil {
		noted, err := r.checks(st, st.Checks, false)
		if err != nil {
			return false, err
		}
		headed = noted && r.live
	}
	ask, unset, err := r.collect(st)
	if err != nil {
		return false, err
	}
	if len(ask) > 0 {
		back, err := r.askStage(st, n, of, ask, headed)
		unset()
		if err != nil || back {
			return back, err
		}
		ask = slices.DeleteFunc(ask, func(f Field) bool { return f.Hide != nil && f.Hide() })
		for _, f := range ask {
			r.markAsked(f)
		}
	}
	if st.After != nil {
		if _, err := r.checks(st, st.After, headed); err != nil {
			return false, err
		}
	}
	summary := ""
	if st.Summary != nil {
		summary = st.Summary()
	} else if len(ask) > 0 {
		var parts []string
		for _, f := range ask {
			parts = append(parts, f.answerText(u.glyphs.Bullet))
		}
		summary = strings.Join(parts, ", ")
	}
	r.stageDone(st.Title, summary)
	return false, nil
}

func (r *flowRun) collect(st *Stage) ([]Field, func(), error) {
	u := r.u
	var ask []Field
	var restores []func()
	unset := func() {
		for _, restore := range restores {
			restore()
		}
	}
	shown := false
	for _, f := range st.Fields {
		if !f.hasValue() {
			return nil, unset, errNoValue
		}
		hidden := f.Hide != nil && f.Hide()
		if hidden && !r.live {
			continue
		}
		if f.kind() == KindConfirm && r.asked[f.Bool] {
			f.Default = ""
		}
		if r.live {
			if hidden {
				restores = append(restores, f.keepWhileHidden())
			}
			yes, err := u.resolve(r.cmd, &f)
			if err != nil {
				return nil, unset, err
			}
			if yes {
				ask = append(ask, f)
				shown = shown || !hidden
			}
			continue
		}
		if err := u.Ask(r.cmd, f); err != nil {
			return nil, unset, err
		}
		r.markAsked(f)
	}
	if !shown {
		unset()
		return nil, func() {}, nil
	}
	return ask, unset, nil
}

func (r *flowRun) checks(st *Stage, run func(ctx context.Context, c *Checklist) error, headed bool) (bool, error) {
	c := &Checklist{u: r.u, ctx: r.ctx}
	title := ""
	if r.live && !headed {
		title = st.Title
	}
	owned := r.u.beginLive(liveChecks, title, nil, nil)
	defer r.u.endLive(owned)
	if err := run(r.ctx, c); err != nil {
		return c.noted, err
	}
	return c.noted, c.err
}

func (r *flowRun) markAsked(f Field) {
	if f.kind() == KindConfirm {
		r.asked[f.Bool] = true
	}
}

func (r *flowRun) stageLine(state State, title, summary string) {
	u := r.u
	if u.silent() {
		return
	}
	lk := u.look()
	w := min(u.Width(), maxContentCol)
	if w < 40 {
		return
	}
	glyph := lk.th.state(state).Render(lk.g.cell(lk.g.state(state)))
	line := glyph + "  " + lk.th.Bold.Render(padRight(Inline(title), 10))
	if summary == "" {
		u.emit(ansi.Truncate(line, w, lk.g.Ellipsis) + "\n")
		return
	}
	line += "  "
	hang := ansi.StringWidth(line)
	parts := strings.Split(wordwrapKeepHyphens(Inline(summary), w-hang), "\n")
	for i, p := range parts {
		if i > 0 {
			line = lk.th.Border.Render(lk.g.Rail) + strings.Repeat(" ", hang-ansi.StringWidth(lk.g.Rail))
		}
		u.emit(ansi.Truncate(line+p, w, lk.g.Ellipsis) + "\n")
	}
}

func (r *flowRun) stageDone(title, summary string) {
	u := r.u
	if u.tier == TierJSON || u.silent() {
		return
	}
	if !u.rich() {
		if summary != "" {
			u.OK("%s", summary)
		}
		return
	}
	r.stageLine(StateOK, title, summary)
}

func (r *flowRun) askStop() error {
	ok, err := r.u.ConfirmWith(Question{Text: r.stopLine(), Default: false})
	if err != nil {
		return err
	}
	if ok {
		return r.stopped(true)
	}
	return nil
}

func (r *flowRun) stopLine() string {
	if r.verb == "go" {
		return "Stop now?"
	}
	return "Stop the " + r.verb + "?"
}

func (r *flowRun) stopped(onRail bool) error {
	r.nothingChanged(onRail)
	return errDeclined
}

func (r *flowRun) interrupted(onRail bool) error {
	r.nothingChanged(onRail)
	return errStopped
}

func (r *flowRun) nothingChanged(onRail bool) {
	u := r.u
	if !onRail || r.f.Title == "" || u.silent() || !u.rich() {
		u.Info(nothingChanged)
		return
	}
	lk := u.look()
	u.emit(lk.th.Border.Render(lk.g.RailBottom) + "  " + nothingChanged + "\n")
}

func (r *flowRun) askStage(st *Stage, n, of int, fields []Field, headed bool) (bool, error) {
	sm := r.stageModel(st, n, of, fields)
	sm.headed = headed
	final, err := r.u.runProgram(sm, true)
	if err != nil {
		return false, err
	}
	switch final.(*stageModel).result {
	case stageBack:
		return true, nil
	case stageAbort:
		return false, r.interrupted(true)
	case stageRunning:
		return false, stoppedAtQuestion()
	}
	for _, f := range fields {
		if f.Hide == nil || !f.Hide() {
			f.settle()
		}
	}
	return false, nil
}

func (r *flowRun) stageModel(st *Stage, n, of int, fields []Field) *stageModel {
	u := r.u
	var binds []any
	for _, f := range fields {
		binds = append(binds, f.binding())
	}
	var groups []*huh.Group
	var first huh.Field
	for _, f := range fields {
		hf := u.fieldHuh(f, binds)
		if first == nil && (f.Hide == nil || !f.Hide()) {
			first = hf
		}
		g := huh.NewGroup(hf)
		if f.Hide != nil {
			g = g.WithHideFunc(f.Hide)
		}
		groups = append(groups, g)
	}
	form := u.form(groups...).WithShowHelp(false).WithWidth(min(u.Width(), maxContentCol) - 4)
	var pending []string
	seen := false
	for _, s := range r.visible() {
		if s == st {
			seen = true
			continue
		}
		if seen {
			pending = append(pending, s.Title)
		}
	}
	if r.f.Review != nil {
		pending = append(pending, "Review")
	}
	if r.f.Tasks != nil {
		pending = append(pending, r.tasksTitle())
	}
	return newStageModel(u.look(), st.Title, n, of, form, first, pending)
}

func (r *flowRun) reviewLoop() error {
	u := r.u
	for {
		card := r.f.Review()
		if card.Title == "" {
			card.Title = RexPlanTitle
		}
		if !u.interactive {
			u.Card(card)
			return nil
		}
		if !r.live {
			u.Card(card)
			key, err := r.reviewChoice(card)
			if err != nil {
				return err
			}
			if key == 0 {
				return nil
			}
			if key < 0 {
				return r.stopped(false)
			}
			if err := r.edit(key); err != nil {
				return err
			}
			continue
		}
		m := newReviewModel(u, card, r.verb)
		final, err := u.runProgram(m, true)
		if err != nil {
			return err
		}
		rm := final.(*reviewModel)
		switch {
		case !rm.done:
			return stoppedAtQuestion()
		case rm.interrupt:
			return r.interrupted(false)
		case rm.abort:
			return r.stopped(false)
		case rm.edit != 0:
			if err := r.edit(rm.edit); err != nil {
				return err
			}
			continue
		}
		u.Card(card)
		return nil
	}
}

func (r *flowRun) reviewChoice(card Card) (rune, error) {
	opts := []Option{{Label: "Go ahead and " + r.verb, Value: "go"}}
	for _, row := range card.Rows {
		if row.Edit != 0 {
			opts = append(opts, Option{Label: "Change " + strings.ToLower(Inline(row.Key)), Value: string(row.Edit)})
		}
	}
	opts = append(opts, Option{Label: "Stop without changing anything", Value: "stop"})
	if len(opts) == 2 {
		ok, err := r.u.ConfirmWith(Question{Text: goAheadLine, Default: true})
		if err != nil {
			return 0, err
		}
		if !ok {
			return -1, nil
		}
		return 0, nil
	}
	v := "go"
	if err := r.u.Ask(nil, Field{Title: "What next?", Value: &v, Options: opts, Kind: KindSelect}); err != nil {
		return 0, err
	}
	switch v {
	case "go":
		return 0, nil
	case "stop":
		return -1, nil
	}
	return []rune(v)[0], nil
}

func (r *flowRun) edit(key rune) error {
	stages := r.visible()
	for i, st := range stages {
		if st.Key != key {
			continue
		}
		for {
			back, err := r.stage(st, i+1, r.total(stages))
			if err != nil {
				return err
			}
			if !back {
				return nil
			}
		}
	}
	return nil
}

const (
	stageRunning = iota
	stageDoneResult
	stageBack
	stageAbort
)

type stageSubmitted struct{}

type stageModel struct {
	lk      look
	title   string
	n, of   int
	form    *huh.Form
	first   huh.Field
	pending []string
	headed  bool
	result  int
	width   int
	height  int
}

func newStageModel(lk look, title string, n, of int, form *huh.Form, first huh.Field, pending []string) *stageModel {
	form.SubmitCmd = func() tea.Msg { return stageSubmitted{} }
	form.CancelCmd = func() tea.Msg { return stageSubmitted{} }
	return &stageModel{lk: lk, title: title, n: n, of: of, form: form, first: first, pending: pending, width: lk.width}
}

func (m *stageModel) Init() tea.Cmd { return m.form.Init() }

func (m *stageModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width, m.height = msg.Width, msg.Height
		m.form = m.form.WithWidth(min(max(msg.Width, 20), maxContentCol) - 4)
	case tea.KeyPressMsg:
		switch msg.String() {
		case "ctrl+c":
			m.result = stageAbort
			return m, tea.Quit
		case "esc":
			m.result = stageBack
			return m, tea.Quit
		case "shift+tab":
			if m.form.GetFocusedField() == m.first {
				m.result = stageBack
				return m, tea.Quit
			}
		}
	case stageSubmitted:
		if m.form.State == huh.StateAborted {
			m.result = stageAbort
		} else {
			m.result = stageDoneResult
		}
		return m, tea.Quit
	}
	fm, cmd := m.form.Update(msg)
	if f, ok := fm.(*huh.Form); ok {
		m.form = f
	}
	if _, size := msg.(tea.WindowSizeMsg); !size && m.height > 0 {
		var sized tea.Cmd
		fm, sized = m.form.Update(tea.WindowSizeMsg{Width: m.width, Height: m.height})
		if f, ok := fm.(*huh.Form); ok {
			m.form = f
		}
		cmd = tea.Batch(cmd, sized)
	}
	return m, cmd
}

func (m *stageModel) View() tea.View {
	if m.result != stageRunning {
		return tea.NewView("")
	}
	lk := m.lk
	w := min(max(m.width, 20), maxContentCol)
	var out []string
	rail := lk.th.Border.Render(lk.g.Rail) + "  "
	if w < 40 {
		out = append(out, lk.th.Accent.Render(fmt.Sprintf("Stage %d of %d, %s", m.n, m.of, Inline(m.title))))
		rail = ""
	} else if !m.headed {
		out = append(out, lk.th.Accent.Render(lk.g.cell(lk.g.Active))+"  "+lk.th.Bold.Render(Inline(m.title)))
	}
	for _, l := range strings.Split(strings.TrimRight(releaseUnits(m.form.View()), "\n"), "\n") {
		out = append(out, ansi.Truncate(rail+l, w, lk.g.Ellipsis))
	}
	if w >= 40 {
		for _, p := range m.pending {
			out = append(out, lk.th.Muted.Render(lk.g.cell(lk.g.Pending)+"  "+Inline(p)))
		}
		out = append(out, lk.th.Border.Render(lk.g.Rail))
	}
	keys := [][2]string{{"enter", "next"}, {"shift+tab", "back"}, {"ctrl+c", "quit"}}
	foot := keyHints(lk, keys)
	if w >= 40 {
		foot = lk.th.Border.Render(lk.g.RailBottom) + "  " + foot
	}
	out = append(out, ansi.Truncate(foot, w, lk.g.Ellipsis))
	v := tea.NewView(strings.Join(out, "\n"))
	v.ProgramStatus = &tea.ProgramStatus{State: tea.ProgramStateBlocked, App: "rexctl", Kind: tea.ProgramStatusKindQuestion}
	return v
}

type reviewModel struct {
	lk        look
	lines     []string
	keys      map[rune]bool
	verb      string
	rex       *rexMotion
	edit      rune
	abort     bool
	interrupt bool
	done      bool
	width     int
	u         *UI
	card      Card
}

func newReviewModel(u *UI, card Card, verb string) *reviewModel {
	lk := u.look()
	m := &reviewModel{lk: lk, keys: map[rune]bool{}, verb: verb, width: lk.width, u: u, card: card}
	for _, r := range card.Rows {
		if r.Edit != 0 {
			m.keys[r.Edit] = true
		}
	}
	if lk.rex {
		m.rex = newRexMotion(lk, MoodIdle)
	}
	m.lines = u.cardLines(card)
	return m
}

func (m *reviewModel) Init() tea.Cmd {
	if m.rex != nil {
		return m.rex.init()
	}
	return nil
}

func (m *reviewModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.lk.width = msg.Width
		m.lines = m.u.cardLines(m.card)
	case tea.KeyPressMsg:
		switch k := msg.String(); k {
		case "ctrl+c":
			m.interrupt, m.done = true, true
			return m, tea.Quit
		case "esc", "q":
			m.abort, m.done = true, true
			return m, tea.Quit
		case "enter", "y":
			m.done = true
			return m, tea.Quit
		default:
			if r := []rune(k); len(r) == 1 && m.keys[r[0]] {
				m.edit, m.done = r[0], true
				return m, tea.Quit
			}
		}
	}
	if m.rex != nil {
		return m, m.rex.update(msg)
	}
	return m, nil
}

func (m *reviewModel) View() tea.View {
	if m.done {
		return tea.NewView("")
	}
	var out []string
	if m.rex != nil {
		out = append(out, sayView(m.rex, m.lk, planLine, -1, ToneNeutral)...)
	}
	out = append(out, m.lines...)
	keys := [][2]string{{"enter", m.verb}}
	if len(m.keys) > 0 {
		var ks []string
		for _, r := range m.card.Rows {
			if r.Edit != 0 {
				ks = append(ks, string(r.Edit))
			}
		}
		keys = append(keys, [2]string{strings.Join(ks, "/"), "edit"})
	}
	keys = append(keys, [2]string{"ctrl+c", "quit"})
	out = append(out, "  "+keyHints(m.lk, keys))
	w := min(max(m.width, 20), maxContentCol)
	for i, l := range out {
		out[i] = ansi.Truncate(l, w, m.lk.g.Ellipsis)
	}
	v := tea.NewView(strings.Join(out, "\n"))
	v.ProgramStatus = &tea.ProgramStatus{State: tea.ProgramStateBlocked, App: "rexctl", Kind: tea.ProgramStatusKindQuestion}
	return v
}
