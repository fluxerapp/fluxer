// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"strings"

	"github.com/charmbracelet/x/ansi"
)

type PlanVerb string

const (
	PlanCreate PlanVerb = "create"
	PlanChange PlanVerb = "change"
	PlanRemove PlanVerb = "remove"
	PlanRun    PlanVerb = "run"
	PlanOpen   PlanVerb = "open"
	PlanKeep   PlanVerb = "keep"
)

type PlanItem struct {
	Verb   PlanVerb
	Text   string
	Detail string
}

func (v PlanVerb) glyph() string {
	switch v {
	case PlanCreate:
		return "+"
	case PlanChange, PlanOpen:
		return "~"
	case PlanRemove:
		return "-"
	case PlanRun:
		return ">"
	}
	return "="
}

func (v PlanVerb) tone() Tone {
	switch v {
	case PlanCreate:
		return ToneOK
	case PlanChange, PlanOpen:
		return ToneWarn
	case PlanRemove:
		return ToneFail
	case PlanRun:
		return ToneAccent
	}
	return ToneNeutral
}

func planBlock(lk look, items []PlanItem, width int) []string {
	var out []string
	for _, it := range items {
		st := lk.th.tone(it.Verb.tone())
		if it.Verb.tone() == ToneNeutral {
			st = lk.th.Muted
		}
		text := Inline(it.Text)
		parts := strings.Split(wordwrap(text, max(10, width-2)), "\n")
		out = append(out, st.Render(it.Verb.glyph())+" "+parts[0])
		for _, p := range parts[1:] {
			out = append(out, "  "+p)
		}
		if it.Detail != "" {
			out = append(out, "  "+lk.th.Muted.Render(ansi.Truncate(Inline(it.Detail), max(10, width-2), lk.g.Ellipsis)))
		}
	}
	return out
}

func planPlain(items []PlanItem) []string {
	var out []string
	for _, it := range items {
		out = append(out, "  "+it.Verb.glyph()+" "+Inline(it.Text))
		if it.Detail != "" {
			out = append(out, "    "+Inline(it.Detail))
		}
	}
	return out
}

func (u *UI) Plan(title string, items ...PlanItem) {
	if u.silent() {
		return
	}
	if !u.rich() {
		lines := []string{"== " + Inline(title)}
		u.emit(strings.Join(append(lines, planPlain(items)...), "\n") + "\n")
		return
	}
	lk := u.look()
	lines := []string{lk.th.Bold.Render(Inline(title))}
	u.block(append(lines, indentAll(planBlock(lk, items, min(u.Width(), maxContentCol)-4), "  ")...))
}

func indentAll(lines []string, pre string) []string {
	out := make([]string, len(lines))
	for i, l := range lines {
		out[i] = pre + l
	}
	return out
}
