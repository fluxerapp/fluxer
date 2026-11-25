// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"strings"
	"time"

	tea "charm.land/bubbletea/v2"
)

type Mood int

const (
	MoodIdle Mood = iota
	MoodTalk
	MoodThink
	MoodWorried
	MoodSad
	MoodParty
	MoodSleep
)

const typeEvery = 33 * time.Millisecond

func (u *UI) Rex(lines ...string) {
	u.Say(MoodIdle, lines...)
}

func (u *UI) Say(m Mood, lines ...string) {
	text := Inline(strings.Join(lines, " "))
	if text == "" {
		return
	}
	if !u.ShowsRex() {
		if u.tier == TierLines && !u.silent() && u.rexSpeaksInLines() {
			u.emit("Rex: " + text + "\n")
		}
		return
	}
	u.mu.Lock()
	busy := u.live != nil
	u.mu.Unlock()
	if u.tier == TierLive && !busy && u.liveCapable() {
		u.emit(u.gap())
		final, err := u.runProgram(newSayModel(u.look(), m, text), true)
		if err == nil {
			if sm, ok := final.(*sayModel); ok && sm.done {
				u.viewLeft()
				return
			}
		}
	}
	u.block(sayRows(u.look(), m, text))
}

func (u *UI) rexSpeaksInLines() bool {
	switch u.commandName() {
	case "install", "setup", "upgrade":
		return true
	}
	return false
}

func (u *UI) commandName() string {
	f := strings.Fields(u.command)
	if len(f) < 2 {
		return ""
	}
	return f[1]
}

func moodTone(m Mood) Tone {
	switch m {
	case MoodWorried:
		return ToneWarn
	case MoodSad:
		return ToneFail
	}
	return ToneNeutral
}

func bubbleWidth(width int, size rexSize) int {
	return min(width-size.width()-6, 60)
}

func sayRows(lk look, m Mood, text string) []string {
	r := newRexMotion(lk, m)
	if m == MoodParty {
		r.party = false
	}
	return sayView(r, lk, text, -1, moodTone(m))
}

func sayView(r *rexMotion, lk look, text string, shown int, tone Tone) []string {
	w := min(lk.width, maxContentCol)
	size := rexSizeFor(w, lk.g.ascii)
	if size == rexGlyph {
		lines := revealLines(strings.Split(wordwrap(text, max(10, w-6)), "\n"), shown)
		return r.render(w, lines, true, tone)
	}
	return r.render(w, bubbleLines(lk, text, bubbleWidth(w, size), tone, shown), true, tone)
}

type sayModel struct {
	lk    look
	rex   *rexMotion
	text  []rune
	typed int
	ticks int
	done  bool
	party bool
}

func newSayModel(lk look, m Mood, text string) *sayModel {
	r := newRexMotion(lk, m)
	r.talking = true
	return &sayModel{lk: lk, rex: r, text: []rune(text), party: m == MoodParty}
}

func (s *sayModel) Init() tea.Cmd {
	return s.next()
}

func (s *sayModel) next() tea.Cmd {
	return tea.Tick(typeEvery, func(time.Time) tea.Msg { return tickMsg{} })
}

func (s *sayModel) finishTyping() tea.Cmd {
	s.typed = len(s.text)
	s.rex.talking = false
	if s.party && !s.rex.party {
		return s.rex.celebrate()
	}
	s.done = true
	return tea.Quit
}

func (s *sayModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		s.lk.width = msg.Width
		s.rex.lk.width = msg.Width
	case tea.KeyPressMsg:
		s.rex.party = false
		s.party = false
		return s, s.finishTyping()
	case tickMsg:
		if s.typed >= len(s.text) {
			return s, s.finishTyping()
		}
		s.ticks++
		s.typed = min(len(s.text), s.typed+2)
		if s.ticks%4 == 0 {
			s.rex.mouth = !s.rex.mouth
		}
		return s, s.next()
	case rexConfettiMsg:
		cmd := s.rex.update(msg)
		if !s.rex.party {
			s.done = true
			return s, tea.Quit
		}
		return s, cmd
	}
	return s, s.rex.update(msg)
}

func (s *sayModel) View() tea.View {
	shown := -1
	if !s.done && s.typed < len(s.text) {
		shown = s.typed
	}
	rows := sayView(s.rex, s.lk, string(s.text), shown, moodTone(s.rex.mood))
	if s.done || !s.party {
		rows = trimBlank(rows)
	}
	return tea.NewView(strings.Join(rows, "\n") + "\n")
}

func (u *UI) Welcome() {
	if u.ShowsRex() {
		u.Say(MoodTalk, Greeting, WelcomeLine)
	}
}

func (u *UI) Finish(url string) {
	u.Success(Success{Title: finishLine, URL: url})
}
