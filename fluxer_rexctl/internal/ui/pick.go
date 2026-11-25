// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"fmt"
	"slices"
	"strings"
	"unicode"

	tea "charm.land/bubbletea/v2"
	"github.com/charmbracelet/x/ansi"
)

type PickItem struct {
	Value  string
	Label  string
	Note   string
	Hint   string
	Group  string
	Detail func(width int) string
}

var ErrBack = &ExitError{Code: ExitOK}

const (
	pickRows   = 10
	detailFrom = 100
)

func (it PickItem) label() string {
	if it.Label != "" {
		return it.Label
	}
	return it.Value
}

func pickOptions(items []PickItem) []Option {
	opts := make([]Option, len(items))
	for i, it := range items {
		opts[i] = Option{Label: it.label(), Value: it.Value, Hint: it.Hint}
	}
	return opts
}

func (u *UI) Pick(title string, items []PickItem, def string) (string, error) {
	if len(items) == 0 {
		return "", Refused(fmt.Sprintf("There's nothing to pick for %q.", title), "")
	}
	if !u.interactive {
		if def != "" {
			return def, nil
		}
		return "", needsAnswer(title, "")
	}
	if u.rich() && u.liveCapable() {
		m := newPickModel(u.look(), title, items, false, []string{def})
		u.emit(u.gap())
		final, err := u.runProgram(m, true)
		if err != nil {
			return "", err
		}
		pm := final.(*pickModel)
		if pm.aborted || !pm.done {
			return "", stoppedAtQuestion()
		}
		if pm.back {
			return "", ErrBack
		}
		v := pm.chosen()[0]
		u.echo(title, pm.labelOf(v))
		return v, nil
	}
	v := def
	if err := u.Ask(nil, Field{Title: title, Value: &v, Options: pickOptions(items), Kind: KindSelect}); err != nil {
		return "", err
	}
	return v, nil
}

func (u *UI) PickMany(title string, items []PickItem, def []string) ([]string, error) {
	if !u.interactive {
		return slices.Clone(def), nil
	}
	if u.rich() && u.liveCapable() && len(items) > 0 {
		m := newPickModel(u.look(), title, items, true, def)
		u.emit(u.gap())
		final, err := u.runProgram(m, true)
		if err != nil {
			return nil, err
		}
		pm := final.(*pickModel)
		if pm.aborted || !pm.done {
			return nil, stoppedAtQuestion()
		}
		if pm.back {
			return nil, ErrBack
		}
		v := pm.chosen()
		labels := make([]string, len(v))
		for i, x := range v {
			labels[i] = pm.labelOf(x)
		}
		answer := JoinAnd(labels)
		if answer == "" {
			answer = "none"
		}
		u.echo(title, answer)
		return v, nil
	}
	v := slices.Clone(def)
	if err := u.Ask(nil, Field{Title: title, Values: &v, Options: pickOptions(items), Kind: KindMultiSelect}); err != nil {
		return nil, err
	}
	return v, nil
}

type pickModel struct {
	lk       look
	title    string
	items    []PickItem
	multi    bool
	picked   map[string]bool
	filter   []rune
	visible  []int
	cursor   int
	offset   int
	done     bool
	aborted  bool
	back     bool
	width    int
	selected string
}

func newPickModel(lk look, title string, items []PickItem, multi bool, def []string) *pickModel {
	m := &pickModel{lk: lk, title: title, items: items, multi: multi, picked: map[string]bool{}, width: lk.width}
	for _, d := range def {
		if d != "" {
			m.picked[d] = true
		}
	}
	m.refilter()
	for i, idx := range m.visible {
		if len(def) > 0 && items[idx].Value == def[0] {
			m.cursor = i
		}
	}
	m.scroll()
	return m
}

func (m *pickModel) Init() tea.Cmd { return nil }

func (m *pickModel) refilter() {
	m.visible = m.visible[:0]
	q := strings.ToLower(string(m.filter))
	for i, it := range m.items {
		hay := strings.ToLower(it.label() + " " + it.Hint + " " + it.Group + " " + it.Value)
		if q == "" || strings.Contains(hay, q) {
			m.visible = append(m.visible, i)
		}
	}
	m.cursor = max(0, min(m.cursor, len(m.visible)-1))
	m.scroll()
}

func (m *pickModel) scroll() {
	if m.cursor < m.offset {
		m.offset = m.cursor
	}
	if m.cursor >= m.offset+pickRows {
		m.offset = m.cursor - pickRows + 1
	}
}

func (m *pickModel) current() (PickItem, bool) {
	if len(m.visible) == 0 {
		return PickItem{}, false
	}
	return m.items[m.visible[m.cursor]], true
}

func (m *pickModel) chosen() []string {
	if !m.multi {
		return []string{m.selected}
	}
	var out []string
	for _, it := range m.items {
		if m.picked[it.Value] {
			out = append(out, it.Value)
		}
	}
	return out
}

func (m *pickModel) labelOf(v string) string {
	for _, it := range m.items {
		if it.Value == v {
			return it.label()
		}
	}
	return v
}

func (m *pickModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
	case tea.KeyPressMsg:
		switch msg.String() {
		case "ctrl+c":
			m.aborted, m.done = true, true
			return m, tea.Quit
		case "esc":
			if len(m.filter) > 0 {
				m.filter = nil
				m.refilter()
				return m, nil
			}
			m.back, m.done = true, true
			return m, tea.Quit
		case "up", "ctrl+p", "shift+tab":
			if m.cursor > 0 {
				m.cursor--
			}
		case "down", "ctrl+n", "tab":
			if m.cursor < len(m.visible)-1 {
				m.cursor++
			}
		case "pgup":
			m.cursor = max(0, m.cursor-pickRows)
		case "pgdown":
			m.cursor = max(0, min(len(m.visible)-1, m.cursor+pickRows))
		case "home":
			m.cursor = 0
		case "end":
			m.cursor = max(0, len(m.visible)-1)
		case "space":
			if m.multi {
				if it, ok := m.current(); ok {
					m.picked[it.Value] = !m.picked[it.Value]
				}
				return m, nil
			}
			m.filter = append(m.filter, ' ')
			m.refilter()
		case "enter":
			it, ok := m.current()
			if !ok {
				return m, nil
			}
			m.selected = it.Value
			m.done = true
			return m, tea.Quit
		case "backspace":
			if len(m.filter) > 0 {
				m.filter = m.filter[:len(m.filter)-1]
				m.refilter()
			}
		default:
			if t := msg.Text; t != "" && !(m.multi && t == " ") {
				for _, r := range t {
					if unicode.IsPrint(r) {
						m.filter = append(m.filter, r)
					}
				}
				m.refilter()
			}
		}
		m.scroll()
	}
	return m, nil
}

func (m *pickModel) View() tea.View {
	if m.done {
		return tea.NewView("")
	}
	return tea.NewView(strings.Join(m.lines(), "\n"))
}

func (m *pickModel) lines() []string {
	lk := m.lk
	w := min(max(m.width, 20), maxContentCol)
	listW := w
	var pane []string
	if m.width >= detailFrom && slices.ContainsFunc(m.items, func(it PickItem) bool { return it.Detail != nil }) {
		listW = w / 2
		if it, ok := m.current(); ok && it.Detail != nil {
			pane = strings.Split(it.Detail(w-listW-3), "\n")
		}
	}
	out := []string{lk.th.Accent.Render(lk.g.cell(lk.g.Active)) + " " + lk.th.Bold.Render(Inline(m.title))}
	filter := lk.th.Muted.Render("type to filter")
	if len(m.filter) > 0 {
		filter = lk.th.Accent.Render("/") + string(m.filter)
	}
	out = append(out, "  "+filter)
	var list []string
	group := ""
	end := min(len(m.visible), m.offset+pickRows)
	labelW := 0
	for i := m.offset; i < end; i++ {
		if it := m.items[m.visible[i]]; it.Note != "" {
			labelW = max(labelW, ansi.StringWidth(Inline(it.label())))
		}
	}
	for i := m.offset; i < end; i++ {
		it := m.items[m.visible[i]]
		if it.Group != "" && it.Group != group && len(m.filter) == 0 {
			group = it.Group
			list = append(list, "  "+lk.th.Muted.Render(Inline(group)))
		}
		mark := "  "
		if i == m.cursor {
			mark = lk.th.AccentStrong.Render(lk.g.Arrow) + " "
			if lk.g.ascii {
				mark = lk.th.AccentStrong.Render(">") + " "
			}
		}
		box := ""
		if m.multi {
			box = "[ ] "
			if m.picked[it.Value] {
				tick := "x"
				if !lk.g.ascii {
					tick = lk.g.OK
				}
				box = lk.th.OK.Render("["+tick+"]") + " "
			}
		}
		label := Inline(it.label())
		if it.Note != "" {
			label = padRight(label, labelW)
		}
		if i == m.cursor {
			label = lk.th.Bold.Render(label)
		}
		if it.Note != "" {
			label += "   " + lk.th.Accent.Render(Inline(it.Note))
		}
		list = append(list, ansi.Truncate("  "+mark+box+label, listW, lk.g.Ellipsis))
	}
	if len(m.visible) == 0 {
		list = append(list, "    "+lk.th.Muted.Render("Nothing matches."))
	}
	if more := len(m.visible) - end; more > 0 {
		list = append(list, "    "+lk.th.Muted.Render(fmt.Sprintf("and %d more", more)))
	}
	if len(pane) > 0 {
		h := max(len(list), len(pane))
		merged := make([]string, h)
		for i := range h {
			l, p := "", ""
			if i < len(list) {
				l = list[i]
			}
			if i < len(pane) {
				p = ansi.Truncate(pane[i], w-listW-3, lk.g.Ellipsis)
			}
			merged[i] = strings.TrimRight(padRight(l, listW)+" "+lk.th.Border.Render(lk.g.Rail)+" "+p, " ")
		}
		list = merged
	}
	out = append(out, list...)
	if it, ok := m.current(); ok && it.Hint != "" && len(pane) == 0 {
		out = append(out, "  "+lk.th.Muted.Render(ansi.Truncate(Inline(it.Hint), w-2, lk.g.Ellipsis)))
	}
	keys := [][2]string{{"↑/↓", "move"}, {"enter", "choose"}, {"esc", "back"}}
	if m.multi {
		keys = [][2]string{{"↑/↓", "move"}, {"space", "toggle"}, {"enter", "done"}, {"esc", "back"}}
	}
	if lk.g.ascii {
		keys[0][0] = "up/down"
	}
	out = append(out, "  "+keyHints(lk, keys))
	return out
}
