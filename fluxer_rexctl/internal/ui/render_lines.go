// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"fmt"
	"strings"
	"sync"
	"time"

	tea "charm.land/bubbletea/v2"
)

const (
	linesEvery   = 15 * time.Second
	linesPercent = 10
)

type linesReporter struct {
	u     *UI
	mu    sync.Mutex
	steps map[string]*lineStep
}

type lineStep struct {
	title    string
	started  time.Time
	pct      int
	printed  time.Time
	waitFor  string
	waitAt   time.Time
	grouped  bool
	depth    int
	itemDone map[string]State
}

func newLinesReporter(u *UI) *linesReporter {
	return &linesReporter{u: u, steps: map[string]*lineStep{}}
}

func (r *linesReporter) quiet() bool {
	return r.u.opts.Quiet || r.u.opts.JSON
}

func (r *linesReporter) actions() bool {
	return r.u.opts.Getenv("GITHUB_ACTIONS") == "true"
}

func (r *linesReporter) write(s string) {
	r.u.emit(s)
}

func (r *linesReporter) indent(st *lineStep) string {
	return strings.Repeat("  ", st.depth)
}

func (r *linesReporter) Send(msg tea.Msg) {
	r.mu.Lock()
	defer r.mu.Unlock()
	th := r.u.theme
	switch m := msg.(type) {
	case stepStarted:
		st := &lineStep{title: m.title, started: m.at, pct: -1, printed: m.at, itemDone: map[string]State{}}
		if p := r.steps[m.parent]; p != nil {
			st.depth = p.depth + 1
		}
		r.steps[m.id] = st
		if r.quiet() {
			return
		}
		r.write(r.indent(st) + th.Muted.Render(linePrefix("-")) + printable(m.title) + "...\n")
	case stepTitle:
		if st := r.steps[m.id]; st != nil {
			if st.title != m.title {
				st.pct = -1
			}
			st.title = m.title
		}
	case stepWait:
		st := r.steps[m.id]
		if st == nil || r.quiet() {
			return
		}
		if m.waitFor == st.waitFor || !st.waitAt.IsZero() && m.at.Sub(st.waitAt) < linesEvery {
			return
		}
		st.waitFor, st.waitAt = m.waitFor, m.at
		r.write(r.indent(st) + linePrefix("") + "waiting for " + printable(m.waitFor) + " (" + Elapsed(m.at.Sub(m.since)) + ")\n")
	case stepProgress:
		st := r.steps[m.id]
		if st == nil || r.quiet() {
			return
		}
		r.progress(st, m)
	case stepItem:
		st := r.steps[m.id]
		if st == nil || r.quiet() {
			return
		}
		if m.state == StateActive || m.state == StatePending {
			return
		}
		if was, ok := st.itemDone[m.item]; ok && (was != StateFail || m.state == StateFail) {
			return
		}
		st.itemDone[m.item] = m.state
		label := cmpOr(m.label, m.item)
		line := r.indent(st) + linePrefix("") + th.state(m.state).Render(linePrefix(lineWord(m.state))) + printable(label)
		if m.detail != "" {
			line += ", " + printable(m.detail)
		}
		r.write(line + "\n")
	case stepLog:
		st := r.steps[m.id]
		if st == nil || r.quiet() || !r.u.opts.Verbose && !r.actions() {
			return
		}
		if r.actions() && !st.grouped {
			st.grouped = true
			r.write("::group::" + printable(st.title) + "\n")
		}
		r.write(r.indent(st) + linePrefix("") + th.Muted.Render(printable(m.line)) + "\n")
	case stepWarn:
		r.mu.Unlock()
		r.u.Warn(m.msg, m.fix)
		r.mu.Lock()
	case stepSkipped:
		if r.quiet() {
			return
		}
		r.write(th.Muted.Render(linePrefix("skip")) + printable(m.title) + "\n")
	case stepDone:
		st := r.steps[m.id]
		delete(r.steps, m.id)
		if st == nil {
			return
		}
		r.endGroup(st)
		if r.quiet() {
			return
		}
		text := printable(cmpOr(m.summary, st.title))
		if d := m.at.Sub(st.started); d >= 2*time.Second {
			text += th.Muted.Render(" (" + Elapsed(d) + ")")
		}
		state := m.state
		if state == StatePending || state == StateActive {
			state = StateOK
		}
		r.write(r.indent(st) + th.state(state).Render(linePrefix(lineWord(state))) + text + "\n")
	case stepFailed:
		st := r.steps[m.id]
		delete(r.steps, m.id)
		indent := ""
		if st != nil {
			r.endGroup(st)
			indent = r.indent(st)
		}
		word, style := "fail", th.Fail
		if m.warn {
			word, style = "warn", th.Warn
		}
		head := indent + style.Render(linePrefix(word)) + printable(m.msg) + "\n"
		if m.fix != "" {
			head += indent + linePrefix("") + printable(m.fix) + "\n"
		}
		r.write(head)
		var b strings.Builder
		if m.cause != "" {
			b.WriteString(linePrefix("") + printable(m.cause) + "\n")
		}
		for _, l := range m.tail {
			b.WriteString("    | " + printable(l) + "\n")
		}
		if m.logPath != "" {
			b.WriteString(linePrefix("") + "Full log " + m.logPath + "\n")
		}
		if b.Len() > 0 {
			r.write(th.Muted.Render(strings.TrimSuffix(b.String(), "\n")) + "\n")
		}
	}
}

func (r *linesReporter) endGroup(st *lineStep) {
	if st.grouped {
		st.grouped = false
		r.write("::endgroup::\n")
	}
}

func (r *linesReporter) progress(st *lineStep, m stepProgress) {
	if m.at.Sub(st.printed) < linesEvery || (m.share && m.total <= 0) {
		return
	}
	line := r.indent(st) + linePrefix("")
	rate := ""
	if m.rate > 0 {
		rate = ", " + Bytes(m.rate) + "/s"
	}
	if m.total > 0 {
		pct := int(m.cur * 100 / m.total)
		if st.pct >= 0 && pct/linesPercent <= st.pct/linesPercent {
			return
		}
		st.pct = pct
		line += fmt.Sprintf("%d%%", pct)
		if !m.share {
			line += fmt.Sprintf("  %s of %s%s", Bytes(m.cur), Bytes(m.total), rate)
		}
	} else if !m.share {
		line += Bytes(m.cur) + strings.TrimPrefix(rate, ",")
	}
	st.printed = m.at
	r.write(line + "\n")
}

func lineWord(st State) string {
	switch st {
	case StateOK:
		return "ok"
	case StateWarn:
		return "warn"
	case StateFail:
		return "fail"
	case StateSkipped:
		return "skip"
	}
	return "-"
}
