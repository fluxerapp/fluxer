// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"strings"
	"time"

	tea "charm.land/bubbletea/v2"
	"github.com/charmbracelet/harmonica"
)

const (
	talkEvery     = 120 * time.Millisecond
	thinkEvery    = 400 * time.Millisecond
	sleepEvery    = 600 * time.Millisecond
	blinkFor      = 120 * time.Millisecond
	confettiTotal = 24
	confettiFor   = time.Second
	hopVelocity   = -10.0
)

type rexBlinkMsg struct {
	gen    int
	closed bool
}

type rexDecoMsg struct{ gen int }

type rexConfettiMsg struct{ gen int }

type particle struct {
	x, row, glyph, born int
}

type rexMotion struct {
	lk        look
	mood      Mood
	talking   bool
	mouth     bool
	y, vel    float64
	spring    harmonica.Spring
	hopGen    int
	blinkGen  int
	blinkSeed uint32
	closed    bool
	decoGen   int
	deco      int
	party     bool
	partyGen  int
	partyTick int
	particles []particle
	seed      uint32
}

func newRexMotion(lk look, mood Mood) *rexMotion {
	return &rexMotion{
		lk: lk, mood: mood, blinkSeed: 7, seed: 42,
		spring: harmonica.NewSpring(harmonica.FPS(liveFPS), 7.0, 0.35),
	}
}

func (r *rexMotion) rand(seed *uint32, n int) int {
	*seed = *seed*1664525 + 1013904223
	return int((*seed >> 8) % uint32(n))
}

func (r *rexMotion) init() tea.Cmd {
	return tea.Batch(r.scheduleBlink(), r.scheduleDeco(), r.scheduleParty())
}

func (r *rexMotion) setMood(m Mood) tea.Cmd {
	if r.mood == m {
		return nil
	}
	r.mood = m
	r.closed = false
	r.decoGen++
	r.blinkGen++
	return tea.Batch(r.scheduleBlink(), r.scheduleDeco())
}

func (r *rexMotion) think(waiting bool) tea.Cmd {
	switch {
	case waiting && r.mood == MoodIdle:
		return r.setMood(MoodThink)
	case !waiting && r.mood == MoodThink:
		return r.setMood(MoodIdle)
	}
	return nil
}

func (r *rexMotion) scheduleBlink() tea.Cmd {
	if r.lk.still || (r.mood != MoodIdle && r.mood != MoodThink) {
		return nil
	}
	gen := r.blinkGen
	wait := 3*time.Second + time.Duration(r.rand(&r.blinkSeed, 2000))*time.Millisecond
	return tea.Tick(wait, func(time.Time) tea.Msg { return rexBlinkMsg{gen: gen, closed: true} })
}

func (r *rexMotion) scheduleDeco() tea.Cmd {
	if r.lk.still {
		return nil
	}
	every := time.Duration(0)
	switch r.mood {
	case MoodThink:
		every = thinkEvery
	case MoodSleep:
		every = sleepEvery
	}
	if every == 0 {
		return nil
	}
	gen := r.decoGen
	return tea.Tick(every, func(time.Time) tea.Msg { return rexDecoMsg{gen: gen} })
}

func (r *rexMotion) scheduleParty() tea.Cmd {
	if r.lk.still || !r.party || r.partyTick >= liveFPS {
		return nil
	}
	gen := r.partyGen
	return tea.Tick(time.Second/liveFPS, func(time.Time) tea.Msg { return rexConfettiMsg{gen: gen} })
}

func (r *rexMotion) celebrate() tea.Cmd {
	r.party = true
	r.partyGen++
	r.partyTick = 0
	r.particles = nil
	if r.lk.still {
		r.partyTick = liveFPS
		return nil
	}
	return r.scheduleParty()
}

func (r *rexMotion) hop() tea.Cmd {
	if r.lk.still {
		return nil
	}
	r.vel = hopVelocity
	r.hopGen++
	return r.hopFrame()
}

func (r *rexMotion) hopFrame() tea.Cmd {
	gen := r.hopGen
	return tea.Tick(time.Second/liveFPS, func(time.Time) tea.Msg { return rexFrameMsg{gen: gen} })
}

func (r *rexMotion) resting() bool { return r.y == 0 && r.vel == 0 }

func (r *rexMotion) update(msg tea.Msg) tea.Cmd {
	switch msg := msg.(type) {
	case rexFrameMsg:
		if msg.gen != r.hopGen {
			return nil
		}
		r.y, r.vel = r.spring.Update(r.y, r.vel, 0)
		if abs(r.y) < 0.01 && abs(r.vel) < 0.01 {
			r.y, r.vel = 0, 0
			return nil
		}
		return r.hopFrame()
	case rexBlinkMsg:
		if msg.gen != r.blinkGen {
			return nil
		}
		r.closed = msg.closed
		if msg.closed {
			gen := r.blinkGen
			return tea.Tick(blinkFor, func(time.Time) tea.Msg { return rexBlinkMsg{gen: gen} })
		}
		return r.scheduleBlink()
	case rexDecoMsg:
		if msg.gen != r.decoGen {
			return nil
		}
		r.deco++
		return r.scheduleDeco()
	case rexConfettiMsg:
		if msg.gen != r.partyGen {
			return nil
		}
		r.partyTick++
		if len(r.particles) < confettiTotal {
			r.particles = append(r.particles, particle{
				x: r.rand(&r.seed, 40), row: r.rand(&r.seed, 4), glyph: r.rand(&r.seed, len(r.lk.g.Confetti)), born: r.partyTick,
			})
		}
		if r.partyTick >= liveFPS {
			r.party = false
			r.particles = nil
			return nil
		}
		return r.scheduleParty()
	}
	return nil
}

func (r *rexMotion) frame() rexFrame {
	switch {
	case r.talking && r.mouth:
		return frameTalk
	case r.closed && (r.mood == MoodIdle || r.mood == MoodThink):
		return frameBlink
	case r.party:
		return frameParty
	}
	return moodFrame(r.mood)
}

func (r *rexMotion) decoRow(width int) string {
	th := r.lk.th
	switch r.mood {
	case MoodThink:
		dots := []string{"·", "· ·", "· · ·"}
		if r.lk.g.ascii {
			dots = []string{".", ". .", ". . ."}
		}
		d := dots[r.deco%3]
		if r.lk.still {
			d = dots[2]
		}
		return strings.Repeat(" ", max(0, width-6)) + th.Muted.Render(d)
	case MoodSleep:
		z := []string{"z", "Z", "z"}
		col := width - 4 + r.deco%3
		if r.lk.still {
			col = width - 3
		}
		return strings.Repeat(" ", max(0, col)) + th.Muted.Render(z[r.deco%3])
	}
	if r.party || r.mood == MoodParty {
		return r.confettiRow(width)
	}
	return ""
}

func (r *rexMotion) confettiRow(width int) string {
	g := r.lk.g.Confetti
	if !r.party || r.lk.still {
		row := []rune(strings.Repeat(" ", width+12))
		put := func(x int, s string) {
			for i, c := range []rune(s) {
				if x+i < len(row) {
					row[x+i] = c
				}
			}
		}
		put(width-2, g[len(g)-1])
		put(width+4, g[1])
		return r.lk.th.Accent.Render(strings.TrimRight(string(row), " "))
	}
	cells := make([]string, width+14)
	for i := range cells {
		cells[i] = " "
	}
	for i, p := range r.particles {
		age := r.partyTick - p.born
		if age > 12 || p.row != 0 && p.row != 1 {
			continue
		}
		x := p.x % len(cells)
		st := r.lk.th.Accent
		if i%2 == 1 {
			st = r.lk.th.OK
		}
		cells[x] = st.Render(g[p.glyph])
	}
	return strings.TrimRight(strings.Join(cells, ""), " ")
}

func (r *rexMotion) lift() int {
	return max(0, min(1, int(-r.y+0.5)))
}

func (r *rexMotion) view(width int, side []string) []string {
	return r.render(width, side, false, ToneNeutral)
}

func (r *rexMotion) render(width int, side []string, bubble bool, tone Tone) []string {
	size := rexSizeFor(width, r.lk.g.ascii)
	if size == rexGlyph {
		head := rexArt(r.lk, size, 0, r.mood)[0]
		side = wrapAll(side, width-6)
		if len(side) == 0 {
			return []string{head}
		}
		out := []string{head + " " + side[0]}
		for _, s := range side[1:] {
			out = append(out, "      "+s)
		}
		return out
	}
	if !bubble {
		side = wrapAll(side, width-size.width()-5)
	}
	art := rexArt(r.lk, size, r.frame(), r.mood)
	rows := besideRex(r.lk, art, size.width(), side, tailRowFor(size), bubble, tone)
	deco := r.decoRow(size.width())
	if r.lift() > 0 {
		return append(rows, "")
	}
	if deco != "" {
		deco = " " + deco
	}
	return append([]string{deco}, rows...)
}

func tailRowFor(size rexSize) int {
	switch size {
	case rexASCII:
		return 2
	}
	return 1
}
