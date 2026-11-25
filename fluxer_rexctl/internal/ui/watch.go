// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"cmp"
	"context"
	"fmt"
	"strings"
	"time"

	tea "charm.land/bubbletea/v2"
	"charm.land/lipgloss/v2"
	"github.com/charmbracelet/x/ansi"
)

type PartRow struct {
	Name     string
	Members  string
	State    State
	Uptime   string
	Restarts string
	CPU      string
	Memory   string
	Spark    []float64
}

type View struct {
	Title    string
	Headline string
	Mood     Mood
	Tone     Tone
	Meta     []KV
	Parts    []PartRow
	Health   []KV
	Data     []KV
	Updated  time.Time
}

type Action struct {
	Key     string
	Help    string
	Confirm string
	Run     func(ctx context.Context, selectedPart string) error
}

const (
	sparkFrom = 70
	twoColumn = 80
)

func partStateWord(st State) string {
	switch st {
	case StateOK:
		return "running"
	case StateActive:
		return "starting"
	case StateWarn:
		return "degraded"
	case StateFail:
		return "failed"
	case StateSkipped:
		return "absent"
	}
	return "stopped"
}

func partTone(lk look, st State) lipgloss.Style {
	switch st {
	case StateOK:
		return lk.th.OK
	case StateActive, StateWarn:
		return lk.th.Warn
	case StateFail:
		return lk.th.Fail
	}
	return lk.th.Muted
}

type dashOpts struct {
	width    int
	selected int
	filter   string
	rex      *rexMotion
	sparks   bool
}

func spark(lk look, samples []float64) string {
	if len(samples) == 0 {
		return ""
	}
	g := lk.g.Spark
	var b strings.Builder
	for _, s := range tail(samples, 20) {
		i := int(max(0, min(1, s)) * float64(len(g)-1))
		b.WriteString(g[i])
	}
	return lk.th.Accent.Render(b.String())
}

func dashHeader(lk look, v View, o dashOpts) []string {
	left := []string{lk.th.Bold.Render(Inline(v.Title))}
	if v.Headline != "" {
		hs := lk.th.tone(v.Tone)
		if v.Tone == ToneNeutral {
			hs = lipgloss.NewStyle()
		}
		left = append(left, hs.Render(Inline(v.Headline)))
	}
	var meta []string
	for _, m := range v.Meta {
		val := Inline(m.Value)
		if m.State != StatePending {
			val = partTone(lk, m.State).Render(lk.g.part(m.State)) + " " + val
		}
		meta = append(meta, val)
	}
	half := (len(meta) + 1) / 2
	right := []string{}
	if len(meta) > 0 {
		right = append(right, strings.Join(meta[:half], gap))
	}
	if half < len(meta) {
		right = append(right, lk.th.Muted.Render(strings.Join(meta[half:], gap)))
	}
	w := o.width
	artW := 0
	var art []string
	if o.rex != nil && (w < 50 || lk.g.ascii) {
		left[0] = rexArt(lk, rexGlyph, 0, o.rex.mood)[0] + " " + left[0]
	}
	if o.rex != nil && w >= 50 && !lk.g.ascii {
		art = rexArt(lk, rexCompact, o.rex.frame(), o.rex.mood)
		artW = rexCompact.width() + 3
	}
	room := w - artW - 1
	lines := max(len(left), len(right))
	var text []string
	if room < 50 {
		text = append(append(text, left...), packMeta(lk, meta, half, room)...)
	} else {
		for i := range lines {
			l, r := "", ""
			if i < len(left) {
				l = left[i]
			}
			if i < len(right) {
				r = right[i]
			}
			l = ansi.Truncate(l, room-ansi.StringWidth(r)-2, lk.g.Ellipsis)
			gap := room - ansi.StringWidth(l) - ansi.StringWidth(r)
			text = append(text, l+strings.Repeat(" ", max(2, gap))+r)
		}
	}
	if len(art) == 0 {
		out := make([]string, len(text))
		for i, t := range text {
			out[i] = " " + ansi.Truncate(t, w-1, lk.g.Ellipsis)
		}
		return out
	}
	h := max(len(art), len(text))
	out := make([]string, h)
	for i := range h {
		a, t := strings.Repeat(" ", rexCompact.width()), ""
		if i < len(art) {
			a = art[i]
		}
		if i < len(text) {
			t = text[i]
		}
		out[i] = strings.TrimRight(" "+a+"   "+ansi.Truncate(t, room, lk.g.Ellipsis), " ")
	}
	return out
}

func packMeta(lk look, meta []string, half, room int) []string {
	var out []string
	line := ""
	for i, m := range meta {
		if i >= half {
			m = lk.th.Muted.Render(m)
		}
		if line != "" && ansi.StringWidth(line)+ansi.StringWidth(gap)+ansi.StringWidth(m) > room {
			out = append(out, line)
			line = ""
		}
		if line != "" {
			line += gap
		}
		line += m
	}
	if line != "" {
		out = append(out, line)
	}
	return out
}

type column struct {
	head  string
	right bool
	cell  func(p PartRow) string
	plain func(p PartRow) string
}

func dashParts(lk look, v View, o dashOpts) []string {
	if len(v.Parts) == 0 {
		return nil
	}
	cols := []column{
		{head: "Part", cell: func(p PartRow) string { return partTone(lk, p.State).Render(lk.g.part(p.State)) + " " + Inline(p.Name) }, plain: func(p PartRow) string { return "  " + Inline(p.Name) }},
		{head: "Uptime", cell: func(p PartRow) string { return partUptime(lk, p) }, plain: func(p PartRow) string { return ansi.Strip(partUptime(lk, p)) }},
		{head: "Memory", right: true, cell: func(p PartRow) string { return Inline(p.Memory) }, plain: func(p PartRow) string { return Inline(p.Memory) }},
		{head: "CPU", right: true, cell: func(p PartRow) string { return Inline(p.CPU) }, plain: func(p PartRow) string { return Inline(p.CPU) }},
		{head: "Restarts", right: true, cell: func(p PartRow) string { return Inline(p.Restarts) }, plain: func(p PartRow) string { return Inline(p.Restarts) }},
		{head: "Runs", cell: func(p PartRow) string { return lk.th.Muted.Render(Inline(p.Members)) }, plain: func(p PartRow) string { return Inline(p.Members) }},
	}
	order := []int{0, 5, 1, 4, 3, 2}
	widths := make([]int, len(cols))
	for i, c := range cols {
		widths[i] = ansi.StringWidth(c.head)
		for _, p := range v.Parts {
			widths[i] = max(widths[i], ansi.StringWidth(c.plain(p)))
		}
	}
	sparks := o.sparks && o.width >= sparkFrom && !lk.g.ascii
	if sparks {
		any := false
		for _, p := range v.Parts {
			any = any || len(p.Spark) > 0
		}
		sparks = any
	}
	budget := o.width - 3
	if sparks {
		budget -= 22
	}
	keep := map[int]bool{0: true}
	used := widths[0]
	for _, i := range []int{1, 2, 3, 4, 5} {
		if widths[i] == ansi.StringWidth(cols[i].head) && emptyColumn(v.Parts, cols[i]) {
			continue
		}
		if used+3+widths[i] <= budget {
			keep[i] = true
			used += 3 + widths[i]
		}
	}
	var shown []int
	for _, i := range order {
		if keep[i] {
			shown = append(shown, i)
		}
	}
	render := func(cells []string, plainW func(int) int) string {
		var b strings.Builder
		for k, i := range shown {
			c := cells[k]
			pad := strings.Repeat(" ", max(0, widths[i]-plainW(k)))
			if cols[i].right {
				b.WriteString(pad + c)
			} else {
				b.WriteString(c + pad)
			}
			if k < len(shown)-1 {
				b.WriteString("   ")
			}
		}
		return b.String()
	}
	heads := make([]string, len(shown))
	for k, i := range shown {
		heads[k] = cols[i].head
	}
	head := lk.th.Muted.Render(render(heads, func(k int) int { return ansi.StringWidth(heads[k]) }))
	out := []string{" " + head}
	for idx, p := range v.Parts {
		if o.filter != "" && !strings.Contains(strings.ToLower(p.Name+" "+p.Members), strings.ToLower(o.filter)) {
			continue
		}
		cells := make([]string, len(shown))
		plain := make([]int, len(shown))
		for k, i := range shown {
			cells[k] = cols[i].cell(p)
			plain[k] = ansi.StringWidth(cols[i].plain(p))
		}
		line := render(cells, func(k int) int { return plain[k] })
		if sparks {
			line += "  " + spark(lk, p.Spark)
		}
		line = strings.TrimRight(line, " ")
		if idx == o.selected && o.selected >= 0 {
			line = lk.th.AccentStrong.Render(selectMark(lk)) + ansi.Truncate(line, o.width-2, lk.g.Ellipsis)
		} else {
			line = " " + ansi.Truncate(line, o.width-2, lk.g.Ellipsis)
		}
		out = append(out, line)
	}
	return out
}

func selectMark(lk look) string {
	if lk.g.ascii {
		return ">"
	}
	return "›"
}

func emptyColumn(parts []PartRow, c column) bool {
	for _, p := range parts {
		if strings.TrimSpace(c.plain(p)) != "" {
			return false
		}
	}
	return true
}

func partUptime(lk look, p PartRow) string {
	if p.State == StateOK || p.Uptime != "" && p.State != StatePending && p.State != StateSkipped {
		if p.State == StateActive || p.State == StateWarn || p.State == StateFail {
			return partTone(lk, p.State).Render(partStateWord(p.State))
		}
		return Inline(p.Uptime)
	}
	return partTone(lk, p.State).Render(partStateWord(p.State))
}

func kvLine(lk look, r KV, keyW int, gutter bool) string {
	line := ""
	switch {
	case r.State != StatePending:
		line += lk.th.state(r.State).Render(lk.g.cell(lk.g.state(r.State))) + " "
	case gutter:
		line += strings.Repeat(" ", ansi.StringWidth(lk.g.cell(lk.g.state(StateOK)))+1)
	}
	if r.Key != "" {
		line += lk.th.Muted.Render(padRight(Inline(r.Key), keyW)) + "  "
	}
	line += Inline(r.Value)
	if r.Hint != "" {
		line += "  " + lk.th.Muted.Render(Inline(r.Hint))
	}
	return line
}

func kvColumn(lk look, title string, rows []KV) []string {
	if len(rows) == 0 {
		return nil
	}
	keyW := 0
	gutter := false
	for _, r := range rows {
		keyW = max(keyW, ansi.StringWidth(Inline(r.Key)))
		gutter = gutter || r.State != StatePending
	}
	out := []string{lk.th.Bold.Render(title)}
	for _, r := range rows {
		out = append(out, kvLine(lk, r, keyW, gutter))
	}
	return out
}

func dashBottom(lk look, v View, o dashOpts) []string {
	health := kvColumn(lk, "Health", v.Health)
	data := kvColumn(lk, "Data", v.Data)
	if len(health) == 0 && len(data) == 0 {
		return nil
	}
	w := o.width - 2
	if o.width < twoColumn || len(health) == 0 || len(data) == 0 {
		var out []string
		for _, l := range health {
			out = append(out, " "+ansi.Truncate(l, w, lk.g.Ellipsis))
		}
		if len(health) > 0 && len(data) > 0 {
			out = append(out, "")
		}
		for _, l := range data {
			out = append(out, " "+ansi.Truncate(l, w, lk.g.Ellipsis))
		}
		return out
	}
	half := w / 2
	h := max(len(health), len(data))
	out := make([]string, h)
	for i := range h {
		l, r := "", ""
		if i < len(health) {
			l = ansi.Truncate(health[i], half-2, lk.g.Ellipsis)
		}
		if i < len(data) {
			r = ansi.Truncate(data[i], w-half, lk.g.Ellipsis)
		}
		out[i] = strings.TrimRight(" "+padRight(l, half)+r, " ")
	}
	return out
}

func dashboard(lk look, v View, o dashOpts) []string {
	out := dashHeader(lk, v, o)
	if parts := dashParts(lk, v, o); len(parts) > 0 {
		out = append(out, "")
		out = append(out, parts...)
	}
	if bottom := dashBottom(lk, v, o); len(bottom) > 0 {
		out = append(out, "")
		out = append(out, bottom...)
	}
	return out
}

func (u *UI) stdoutLook() look {
	lk := u.look()
	lk.width = u.Width()
	if w := u.columns(); w > 0 {
		lk.width = w
	}
	if u.outw == u.opts.Stdout {
		lk.th = plainTheme()
		lk.colour = false
	}
	return lk
}

func (u *UI) StatusCard(v View) {
	if u.tier == TierJSON || u.opts.JSON {
		return
	}
	out := u.outw
	if !u.stdoutTTY || !u.rich() || u.accessible {
		u.statusPlain(v)
		return
	}
	lk := u.stdoutLook()
	o := dashOpts{width: min(lk.width, 120), selected: -1, sparks: !u.accessible}
	if u.ShowsRex() {
		o.rex = newRexMotion(lk, v.Mood)
	}
	fmt.Fprintln(out, strings.Join(dashboard(lk, v, o), "\n"))
}

func (u *UI) statusPlain(v View) {
	out := u.opts.Stdout
	if v.Title != "" {
		fmt.Fprintln(out, Inline(v.Title))
	}
	if v.Headline != "" {
		fmt.Fprintln(out, Inline(v.Headline))
	}
	for _, l := range kvPlain(v.Meta) {
		fmt.Fprintln(out, l)
	}
	if len(v.Parts) > 0 {
		fmt.Fprintln(out)
		measured := false
		for _, p := range v.Parts {
			measured = measured || p.CPU != "" || p.Memory != ""
		}
		head := []string{"Part", "State", "Runs", "Uptime", "Restarts"}
		if measured {
			head = append(head, "CPU", "Memory")
		}
		rows := make([][]string, len(v.Parts))
		for i, p := range v.Parts {
			rows[i] = []string{p.Name, partStateWord(p.State), p.Members, p.Uptime, p.Restarts}
			if measured {
				rows[i] = append(rows[i], cmp.Or(p.CPU, "-"), cmp.Or(p.Memory, "-"))
			}
		}
		u.Table(head, rows)
	}
	for _, group := range [][]KV{v.Health, v.Data} {
		if len(group) == 0 {
			continue
		}
		fmt.Fprintln(out)
		for _, l := range kvPlain(group) {
			fmt.Fprintln(out, l)
		}
	}
}

func (u *UI) Watch(ctx context.Context, fetch func(context.Context) (View, error), every time.Duration, actions ...Action) error {
	if every <= 0 {
		every = 2 * time.Second
	}
	if !u.interactive || !u.rich() || !u.liveCapable() {
		v, err := fetch(ctx)
		if err != nil {
			if ctx.Err() != nil {
				return nil
			}
			return err
		}
		u.StatusCard(v)
		return nil
	}
	var last *View
	selected := 0
	for {
		m := newWatchModel(u.look(), ctx, fetch, every, actions, u.ShowsRex())
		m.selected = selected
		if last != nil {
			m.view, m.loaded = *last, true
		}
		final, err := u.runProgramCtx(ctx, m, true)
		if err != nil {
			return err
		}
		wm := final.(*watchModel)
		if wm.err != nil && !wm.loaded {
			return wm.err
		}
		if wm.run == nil {
			return nil
		}
		v := wm.view
		last, selected = &v, wm.selected
		part := wm.part()
		if err := wm.run.Run(ctx, part); err != nil {
			u.ReportError(err)
		}
		if ctx.Err() != nil {
			return nil
		}
		u.emit(u.gap() + keyHints(u.look(), [][2]string{{"enter", "back to the dashboard"}, {"q", "quit"}}) + "\n")
		if u.waitKey("q") {
			return nil
		}
	}
}

type watchFetched struct {
	view View
	err  error
	gen  int
}

type watchTick struct{ gen int }

type watchModel struct {
	lk        look
	ctx       context.Context
	fetch     func(context.Context) (View, error)
	every     time.Duration
	actions   []Action
	view      View
	loaded    bool
	err       error
	gen       int
	selected  int
	filtering bool
	filter    []rune
	help      bool
	confirm   *Action
	run       *Action
	rex       *rexMotion
	width     int
	height    int
	quit      bool
}

func newWatchModel(lk look, ctx context.Context, fetch func(context.Context) (View, error), every time.Duration, actions []Action, rex bool) *watchModel {
	m := &watchModel{lk: lk, ctx: ctx, fetch: fetch, every: every, actions: actions, width: lk.width, height: 24}
	if rex {
		m.rex = newRexMotion(lk, MoodIdle)
	}
	return m
}

func (m *watchModel) load() tea.Cmd {
	gen := m.gen
	ctx, fetch := m.ctx, m.fetch
	return func() tea.Msg {
		v, err := fetch(ctx)
		return watchFetched{view: v, err: err, gen: gen}
	}
}

func (m *watchModel) Init() tea.Cmd {
	cmds := []tea.Cmd{m.load()}
	if m.rex != nil {
		cmds = append(cmds, m.rex.init())
	}
	return tea.Batch(cmds...)
}

func (m *watchModel) part() string {
	if m.selected >= 0 && m.selected < len(m.view.Parts) {
		return m.view.Parts[m.selected].Name
	}
	return ""
}

func (m *watchModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width, m.height = msg.Width, msg.Height
		m.lk.width = msg.Width
	case watchFetched:
		if msg.gen != m.gen {
			return m, nil
		}
		if m.ctx.Err() != nil {
			m.quit = true
			return m, tea.Quit
		}
		m.err = msg.err
		if msg.err == nil {
			m.view, m.loaded = msg.view, true
			if m.rex != nil {
				return m, tea.Batch(m.rex.setMood(msg.view.Mood), m.schedule())
			}
		}
		return m, m.schedule()
	case watchTick:
		if msg.gen != m.gen {
			return m, nil
		}
		return m, m.load()
	case tea.KeyPressMsg:
		return m, m.key(msg)
	}
	if m.rex != nil {
		return m, m.rex.update(msg)
	}
	return m, nil
}

func (m *watchModel) schedule() tea.Cmd {
	gen := m.gen
	return tea.Tick(m.every, func(time.Time) tea.Msg { return watchTick{gen: gen} })
}

func (m *watchModel) key(msg tea.KeyPressMsg) tea.Cmd {
	k := msg.String()
	if m.confirm != nil {
		switch k {
		case "y", "enter":
			m.run, m.quit = m.confirm, true
			return tea.Quit
		case "n", "esc", "q":
			m.confirm = nil
		case "ctrl+c":
			m.quit = true
			return tea.Quit
		}
		return nil
	}
	if m.filtering {
		switch k {
		case "enter":
			m.filtering = false
		case "esc":
			m.filtering, m.filter = false, nil
		case "backspace":
			if len(m.filter) > 0 {
				m.filter = m.filter[:len(m.filter)-1]
			}
		case "ctrl+c":
			m.quit = true
			return tea.Quit
		default:
			if msg.Text != "" {
				m.filter = append(m.filter, []rune(msg.Text)...)
			}
		}
		return nil
	}
	switch k {
	case "q", "ctrl+c", "esc":
		m.quit = true
		return tea.Quit
	case "up", "k":
		if m.selected > 0 {
			m.selected--
		}
	case "down", "j":
		if m.selected < len(m.view.Parts)-1 {
			m.selected++
		}
	case "/":
		m.filtering = true
	case "?":
		m.help = !m.help
	default:
		for i := range m.actions {
			a := &m.actions[i]
			if a.Key != k || a.Run == nil {
				continue
			}
			if a.Confirm != "" {
				m.confirm = a
				return nil
			}
			m.run, m.quit = a, true
			return tea.Quit
		}
	}
	return nil
}

func (m *watchModel) View() tea.View {
	if m.quit {
		v := tea.NewView("")
		return v
	}
	lk := m.lk
	w := max(20, m.width)
	o := dashOpts{width: min(w, 120), selected: m.selected, filter: string(m.filter), rex: m.rex, sparks: true}
	var out []string
	switch {
	case !m.loaded && m.err == nil:
		out = append(out, "", " "+lk.th.Muted.Render("Asking Fluxer how it's doing"+lk.g.Ellipsis))
	case !m.loaded:
		out = append(out, "", " "+lk.th.Fail.Render(lk.g.Fail)+" "+Inline(Sentence(m.err.Error())))
	default:
		out = append(out, "")
		out = append(out, dashboard(lk, m.view, o)...)
		if m.err != nil {
			out = append(out, "", " "+lk.th.Warn.Render(lk.g.Warn)+" "+lk.th.Muted.Render("Couldn't refresh. "+Inline(Sentence(m.err.Error()))))
		}
	}
	if m.help {
		out = append(out, "")
		for _, a := range m.actions {
			out = append(out, "  "+lk.th.Accent.Bold(true).Render(padRight(a.Key, 6))+lk.th.Muted.Render(a.Help))
		}
		out = append(out, "  "+lk.th.Accent.Bold(true).Render(padRight("/", 6))+lk.th.Muted.Render("filter parts"))
		out = append(out, "  "+lk.th.Accent.Bold(true).Render(padRight("q", 6))+lk.th.Muted.Render("quit"))
	}
	for len(out) < m.height-1 {
		out = append(out, "")
	}
	if len(out) > m.height-1 && m.height > 2 {
		out = out[:m.height-1]
	}
	out = append(out, " "+m.footer())
	content := strings.Join(out, "\n")
	if m.confirm != nil {
		content = overlay(lk, content, m.confirm.Confirm, w, m.height)
	}
	v := tea.NewView(content)
	v.AltScreen = true
	v.WindowTitle = "rexctl status"
	return v
}

func (m *watchModel) footer() string {
	lk := m.lk
	if m.filtering {
		return lk.th.Accent.Render("/") + string(m.filter) + lk.th.Muted.Render("  enter apply"+gap+"esc clear")
	}
	var keys [][2]string
	for _, a := range m.actions {
		keys = append(keys, [2]string{a.Key, a.Help})
	}
	keys = append(keys, [2]string{"/", "filter"}, [2]string{"?", "help"}, [2]string{"q", "quit"})
	line := keyHints(lk, keys)
	if !m.view.Updated.IsZero() {
		line += lk.th.Muted.Render(gap + "updated " + m.view.Updated.Format("15:04:05"))
	}
	return ansi.Truncate(line, max(10, m.width-2), lk.g.Ellipsis)
}

func overlay(lk look, bg, question string, width, height int) string {
	body := []string{Inline(question), "", keyHints(lk, [][2]string{{"y", "yes"}, {"n", "no"}})}
	w := 0
	for _, l := range body {
		w = max(w, ansi.StringWidth(l))
	}
	box := strings.Join(boxed(lk, body, w, ToneWarn), "\n")
	x := max(0, (width-w-6)/2)
	y := max(0, (height-len(body)-2)/2)
	return lipgloss.NewCompositor(
		lipgloss.NewLayer(bg),
		lipgloss.NewLayer(box).X(x).Y(y).Z(1),
	).Render()
}

func boxed(lk look, body []string, w int, tone Tone) []string {
	b := lk.g.Border
	st := lk.th.tone(tone)
	out := []string{st.Render(b.TopLeft + strings.Repeat(b.Top, w+4) + b.TopRight)}
	for _, l := range body {
		out = append(out, st.Render(b.Left)+"  "+padRight(l, w)+"  "+st.Render(b.Right))
	}
	return append(out, st.Render(b.BottomLeft+strings.Repeat(b.Bottom, w+4)+b.BottomRight))
}
