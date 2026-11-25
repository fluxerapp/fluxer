// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"encoding/json"
	"fmt"
	"sync"
	"time"

	tea "charm.land/bubbletea/v2"
)

type jsonReporter struct {
	u    *UI
	mu   sync.Mutex
	sent map[string]time.Time
}

func (r *jsonReporter) event(name string, kv ...any) {
	ev := map[string]any{"v": 1, "t": r.u.opts.Now().UTC().Format("2006-01-02T15:04:05.000Z07:00"), "event": name}
	for i := 0; i+1 < len(kv); i += 2 {
		ev[fmt.Sprint(kv[i])] = kv[i+1]
	}
	data, err := json.Marshal(ev)
	if err != nil {
		return
	}
	r.u.mu.Lock()
	defer r.u.mu.Unlock()
	_, _ = r.u.opts.Stderr.Write(append(data, '\n'))
}

func (r *jsonReporter) Send(msg tea.Msg) {
	switch m := msg.(type) {
	case stepStarted:
		r.event("step.start", "id", m.id, "title", printable(m.title))
	case stepWait:
		r.event("step.wait", "id", m.id, "for", printable(m.waitFor), "elapsed_ms", m.at.Sub(m.since).Milliseconds())
	case stepProgress:
		r.mu.Lock()
		last := r.sent[m.id]
		due := m.at.Sub(last) >= time.Second || (m.total > 0 && m.cur >= m.total)
		if due {
			r.sent[m.id] = m.at
		}
		r.mu.Unlock()
		if due {
			r.event("step.progress", "id", m.id, "current", m.cur, "total", m.total, "rate", m.rate)
		}
	case stepItem:
		if m.label == "" && m.detail == "" && m.state == StateActive {
			return
		}
		r.event("step.item", "id", m.id, "item", m.item, "label", printable(cmpOr(m.label, m.item)), "state", m.state.String(), "detail", printable(m.detail))
	case stepLog:
		r.event("step.log", "id", m.id, "line", printable(m.line))
	case stepWarn:
		r.event("warn", "message", printable(m.msg), "fix", printable(m.fix))
	case stepSkipped:
		r.event("step.item", "id", m.id, "item", m.id, "label", printable(m.title), "state", StateSkipped.String(), "detail", "")
	case stageMsg:
		r.event("stage", "n", m.n, "of", m.of, "id", m.id, "title", printable(m.title))
	case stepDone:
		r.mu.Lock()
		delete(r.sent, m.id)
		r.mu.Unlock()
		state := m.state
		if state == StatePending || state == StateActive {
			state = StateOK
		}
		r.event("step.done", "id", m.id, "summary", printable(m.summary), "state", state.String(), "elapsed_ms", m.at.Sub(m.started).Milliseconds())
	case stepFailed:
		r.mu.Lock()
		delete(r.sent, m.id)
		r.mu.Unlock()
		if m.warn {
			r.event("step.done", "id", m.id, "summary", printable(m.msg), "state", StateWarn.String(), "elapsed_ms", m.at.Sub(m.started).Milliseconds())
			r.event("warn", "message", printable(m.msg), "fix", printable(m.fix))
			return
		}
		r.event("step.fail", "id", m.id, "message", printable(m.msg), "fix", printable(m.fix), "cause", printable(m.cause), "log_path", m.logPath)
	}
}
