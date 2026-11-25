// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"io"
	"regexp"
	"slices"
	"strings"

	tea "charm.land/bubbletea/v2"
	"charm.land/lipgloss/v2"
	"github.com/charmbracelet/x/ansi"
)

type LogOptions struct {
	Title    string
	Parts    []string
	Follow   bool
	MaxLines int
}

const logCap = 10000

type logLine struct {
	part, text string
}

type logLinesMsg struct{ lines []logLine }

type logEOFMsg struct{ err error }

func splitLogLine(s string) logLine {
	s = strings.TrimRight(s, "\r")
	if i := strings.Index(s, " | "); i > 0 && i <= 40 {
		return logLine{part: strings.TrimSpace(s[:i]), text: s[i+3:]}
	}
	if i := strings.Index(s, "|"); i > 0 && i <= 40 && !strings.ContainsAny(s[:i], " \t") {
		return logLine{part: strings.TrimSpace(s[:i]), text: strings.TrimPrefix(s[i+1:], " ")}
	}
	return logLine{text: s}
}

func (u *UI) LogView(ctx context.Context, r io.Reader, o LogOptions) error {
	if !u.stdoutTTY || !u.interactive || !u.rich() || !u.liveCapable() {
		_, err := io.Copy(u.opts.Stdout, r)
		if ctx.Err() != nil || errors.Is(err, io.EOF) {
			return nil
		}
		return err
	}
	if o.MaxLines <= 0 {
		o.MaxLines = logCap
	}
	m := newLogModel(u.look(), o)
	lr := u.startProgram(m, true, nil)
	go feedLogs(r, lr.p)
	select {
	case <-lr.done:
	case <-ctx.Done():
		lr.p.Quit()
		<-lr.done
	}
	if ctx.Err() != nil {
		return nil
	}
	return lr.err
}

func feedLogs(r io.Reader, p *tea.Program) {
	sc := bufio.NewScanner(r)
	sc.Buffer(make([]byte, 64*1024), 1024*1024)
	batch := make([]logLine, 0, 64)
	for sc.Scan() {
		batch = append(batch, splitLogLine(printable(sc.Text())))
		if len(batch) == cap(batch) {
			p.Send(logLinesMsg{lines: batch})
			batch = make([]logLine, 0, 64)
		}
	}
	if len(batch) > 0 {
		p.Send(logLinesMsg{lines: batch})
	}
	p.Send(logEOFMsg{err: sc.Err()})
}

type logModel struct {
	lk        look
	o         LogOptions
	lines     []logLine
	shown     []int
	offset    int
	follow    bool
	width     int
	height    int
	re        *regexp.Regexp
	reErr     string
	filtering bool
	input     []rune
	match     int
	matches   []int
	hidden    map[string]bool
	picking   bool
	pickAt    int
	parts     []string
	ended     bool
	partW     int
	colours   map[string]lipgloss.Style
	quit      bool
}

func newLogModel(lk look, o LogOptions) *logModel {
	m := &logModel{lk: lk, o: o, follow: true, width: lk.width, height: 24, hidden: map[string]bool{}, colours: map[string]lipgloss.Style{}}
	for _, p := range o.Parts {
		m.addPart(p)
	}
	return m
}

func (m *logModel) Init() tea.Cmd { return nil }

func (m *logModel) palette() []lipgloss.Style {
	th := m.lk.th
	if th.plain {
		return []lipgloss.Style{lipgloss.NewStyle()}
	}
	mix := func(a, b lipgloss.Style) lipgloss.Style {
		return lipgloss.NewStyle().Foreground(lipgloss.Blend1D(3, a.GetForeground(), b.GetForeground())[1])
	}
	return []lipgloss.Style{th.Accent, th.OK, th.Warn, mix(th.Accent, th.OK), mix(th.OK, th.Warn)}
}

func (m *logModel) addPart(p string) {
	if p == "" || slices.Contains(m.parts, p) {
		return
	}
	pal := m.palette()
	m.colours[p] = pal[len(m.parts)%len(pal)]
	m.parts = append(m.parts, p)
	m.partW = min(16, max(m.partW, ansi.StringWidth(p)))
}

func (m *logModel) visibleLine(l logLine) bool {
	if m.hidden[l.part] {
		return false
	}
	return m.re == nil || m.re.MatchString(l.text) || m.re.MatchString(l.part)
}

func (m *logModel) rebuild() {
	m.shown = m.shown[:0]
	m.matches = m.matches[:0]
	for i, l := range m.lines {
		if !m.visibleLine(l) {
			continue
		}
		m.shown = append(m.shown, i)
		if m.re != nil {
			m.matches = append(m.matches, len(m.shown)-1)
		}
	}
	m.clamp()
}

func (m *logModel) rows() int { return max(1, m.height-2) }

func (m *logModel) clamp() {
	maxOff := max(0, len(m.shown)-m.rows())
	if m.follow {
		m.offset = maxOff
	}
	m.offset = max(0, min(m.offset, maxOff))
}

func (m *logModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width, m.height = msg.Width, msg.Height
		m.clamp()
	case logLinesMsg:
		for _, l := range msg.lines {
			m.addPart(l.part)
		}
		m.lines = append(m.lines, msg.lines...)
		if over := len(m.lines) - m.o.MaxLines; over > 0 {
			m.lines = append([]logLine(nil), m.lines[over:]...)
			if !m.follow {
				m.offset = max(0, m.offset-over)
			}
		}
		m.rebuild()
	case logEOFMsg:
		m.ended = true
	case tea.KeyPressMsg:
		return m, m.key(msg)
	case tea.MouseWheelMsg:
		if msg.Button == tea.MouseWheelUp {
			m.scroll(-3)
		} else if msg.Button == tea.MouseWheelDown {
			m.scroll(3)
		}
	}
	return m, nil
}

func (m *logModel) scroll(n int) {
	m.offset += n
	maxOff := max(0, len(m.shown)-m.rows())
	m.offset = max(0, min(m.offset, maxOff))
	m.follow = m.offset >= maxOff
}

func (m *logModel) key(msg tea.KeyPressMsg) tea.Cmd {
	k := msg.String()
	if k == "ctrl+c" {
		m.quit = true
		return tea.Quit
	}
	if m.filtering {
		switch k {
		case "enter":
			m.filtering = false
			m.applyFilter(string(m.input))
		case "esc":
			m.filtering = false
		case "backspace":
			if len(m.input) > 0 {
				m.input = m.input[:len(m.input)-1]
			}
		default:
			if msg.Text != "" {
				m.input = append(m.input, []rune(msg.Text)...)
			}
		}
		return nil
	}
	if m.picking {
		switch k {
		case "up", "k":
			m.pickAt = max(0, m.pickAt-1)
		case "down", "j":
			m.pickAt = min(len(m.parts)-1, m.pickAt+1)
		case "space", "x":
			if m.pickAt < len(m.parts) {
				p := m.parts[m.pickAt]
				m.hidden[p] = !m.hidden[p]
			}
		case "enter", "esc", "p", "q":
			m.picking = false
			m.rebuild()
		}
		return nil
	}
	switch k {
	case "q", "esc":
		m.quit = true
		return tea.Quit
	case "up", "k":
		m.scroll(-1)
	case "down", "j":
		m.scroll(1)
	case "pgup", "b":
		m.scroll(-m.rows())
	case "pgdown", "space":
		m.scroll(m.rows())
	case "home", "g":
		m.follow = false
		m.offset = 0
	case "end", "G", "f":
		m.follow = true
		m.clamp()
	case "/":
		m.filtering = true
		m.input = nil
	case "n":
		m.jump(1)
	case "N":
		m.jump(-1)
	case "p":
		if len(m.parts) > 0 {
			m.picking = true
		}
	}
	return nil
}

func (m *logModel) applyFilter(q string) {
	m.reErr = ""
	if q == "" {
		m.re = nil
		m.rebuild()
		return
	}
	re, err := regexp.Compile("(?i)" + q)
	if err != nil {
		m.reErr = "That isn't a pattern rexctl can search for."
		return
	}
	m.re = re
	m.rebuild()
	m.match = -1
	m.jump(1)
}

func (m *logModel) jump(dir int) {
	if len(m.matches) == 0 {
		return
	}
	m.follow = false
	pos := m.offset
	next := -1
	if dir > 0 {
		for _, i := range m.matches {
			if i > pos {
				next = i
				break
			}
		}
		if next < 0 {
			next = m.matches[0]
		}
	} else {
		for k := len(m.matches) - 1; k >= 0; k-- {
			if m.matches[k] < pos {
				next = m.matches[k]
				break
			}
		}
		if next < 0 {
			next = m.matches[len(m.matches)-1]
		}
	}
	m.offset = next
	m.clamp()
}

func (m *logModel) render(l logLine) string {
	lk := m.lk
	line := ""
	if m.partW > 0 {
		st, ok := m.colours[l.part]
		if !ok {
			st = lk.th.Muted
		}
		line = st.Render(padRight(ansi.Truncate(l.part, m.partW, ""), m.partW)) + " " + lk.th.Border.Render(lk.g.Rail) + " "
	}
	text := l.text
	if m.re != nil && !lk.th.plain {
		text = m.re.ReplaceAllStringFunc(text, func(s string) string { return lk.th.Selected.Render(s) })
	}
	return ansi.Truncate(line+text, max(10, m.width), lk.g.Ellipsis)
}

func (m *logModel) View() tea.View {
	if m.quit {
		return tea.NewView("")
	}
	lk := m.lk
	var out []string
	title := cmpOr(m.o.Title, "Logs")
	out = append(out, lk.th.Bold.Render(Inline(title))+lk.th.Muted.Render(fmt.Sprintf("  %d lines", len(m.shown))))
	end := min(len(m.shown), m.offset+m.rows())
	for i := m.offset; i < end; i++ {
		out = append(out, m.render(m.lines[m.shown[i]]))
	}
	for len(out) < m.rows()+1 {
		out = append(out, "")
	}
	if m.picking {
		out = m.pickOverlay(out)
	}
	out = append(out, m.footer())
	v := tea.NewView(strings.Join(out, "\n"))
	v.AltScreen = true
	v.MouseMode = tea.MouseModeCellMotion
	v.WindowTitle = "rexctl logs"
	return v
}

func (m *logModel) pickOverlay(out []string) []string {
	lk := m.lk
	var rows []string
	for i, p := range m.parts {
		mark := "[x]"
		if m.hidden[p] {
			mark = "[ ]"
		}
		cursor := "  "
		if i == m.pickAt {
			cursor = lk.th.AccentStrong.Render(selectMark(lk)) + " "
		}
		rows = append(rows, cursor+mark+" "+m.colours[p].Render(p))
	}
	start := max(1, len(out)-len(rows)-1)
	for i, r := range rows {
		if start+i < len(out) {
			out[start+i] = r
		}
	}
	return out
}

func (m *logModel) footer() string {
	lk := m.lk
	if m.filtering {
		return lk.th.Accent.Render("/") + string(m.input) + lk.th.Muted.Render("  enter search"+gap+"esc cancel")
	}
	var state string
	switch {
	case m.reErr != "":
		state = lk.th.Warn.Render(m.reErr)
	case m.follow && !m.ended:
		state = lk.th.OK.Render("following")
	case m.follow:
		state = lk.th.Muted.Render("end of log")
	default:
		state = lk.th.Warn.Render("paused")
	}
	if m.re != nil {
		state += lk.th.Muted.Render(fmt.Sprintf(", %d matches", len(m.matches)))
	}
	keys := [][2]string{{"f", "follow"}, {"/", "search"}, {"n/N", "next match"}, {"p", "parts"}, {"q", "quit"}}
	if m.picking {
		keys = [][2]string{{"space", "show or hide"}, {"enter", "done"}}
	}
	return ansi.Truncate(state+gap+keyHints(lk, keys), max(10, m.width), lk.g.Ellipsis)
}
