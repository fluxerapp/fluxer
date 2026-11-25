// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"image/color"
	"regexp"
	"slices"
	"strings"
	"unicode"

	"charm.land/lipgloss/v2"
	"github.com/charmbracelet/x/ansi"
)

type rexSize int

const (
	rexGlyph rexSize = iota
	rexCompact
	rexFull
	rexASCII
)

var rexPalette = map[byte]color.Color{
	'a': lipgloss.Color("#6f6bf2"),
	'd': lipgloss.Color("#4641d9"),
	'l': lipgloss.Color("#b0adff"),
	'w': lipgloss.Color("#ffffff"),
	'e': lipgloss.Color("#1c1a4a"),
	't': lipgloss.Color("#f4f3ff"),
	'm': lipgloss.Color("#2b2794"),
	'c': lipgloss.Color("#2b2794"),
	's': lipgloss.Color("#7fd4ff"),
}

var rexFullIdle = []string{
	".........aaaaaa.",
	"........aaweaaaa",
	"........aaaaaaaa",
	"........aaatttt.",
	"........aaaaaa..",
	"........aaaa....",
	"d......aaaaac...",
	"dd....aalllac...",
	".ddaaaalllla....",
	"..ddaaaaaaa.....",
	"......dd.aa.....",
	"......cc.cc.....",
}

type rexFrame int

const (
	frameIdle rexFrame = iota
	frameBlink
	frameTalk
	frameWorried
	frameSad
	frameParty
	frameSleep
)

var rexFullFrames = map[rexFrame]map[int]string{
	frameBlink:   {1: "........aaddaaaa"},
	frameTalk:    {3: "........aaatttt.", 4: "........aammmm..", 5: "........aaaaat.."},
	frameWorried: {0: ".........aaaaaas", 1: "........aaewaaaa"},
	frameSad:     {1: "........aaddaaaa", 3: "........aaaaaaa.", 6: "d......aaaaa....", 8: ".ddaaaallllac..."},
	frameParty:   {1: "........aawaaaaa", 5: ".......caaaa....", 6: "d......aaaaa...."},
	frameSleep:   {1: "........aaddaaaa", 3: "........aaaaaaa."},
}

var rexCompactIdle = []string{
	"....aaa.",
	"...aawea",
	"...aatt.",
	"...aaa..",
	"d.aaaac.",
	".daalla.",
	"..aa.a..",
	"..cc.c..",
}

var rexCompactFrames = map[rexFrame]map[int]string{
	frameBlink: {1: "...aadda"},
	frameSad:   {1: "...aadda", 2: "...aaaa."},
	frameParty: {4: "d.aaaa..", 3: "..caa..."},
	frameSleep: {1: "...aadda"},
}

var rexASCIIArt = []string{
	"           ____",
	"          / o  \\__",
	"          |  ____>",
	"   _______/  /",
	"  <___      /",
	"      |_|-|_|",
}

const rexASCIIWidth = 22

func moodFrame(m Mood) rexFrame {
	switch m {
	case MoodTalk:
		return frameTalk
	case MoodWorried:
		return frameWorried
	case MoodSad:
		return frameSad
	case MoodParty:
		return frameParty
	case MoodSleep:
		return frameSleep
	}
	return frameIdle
}

func spriteRows(size rexSize, f rexFrame) []string {
	base, frames := rexFullIdle, rexFullFrames
	if size == rexCompact {
		base, frames = rexCompactIdle, rexCompactFrames
	}
	rows := append([]string(nil), base...)
	for i, r := range frames[f] {
		rows[i] = r
	}
	return rows
}

func rexSizeFor(width int, ascii bool) rexSize {
	switch {
	case width < 40, ascii && width < 56:
		return rexGlyph
	case ascii:
		return rexASCII
	case width < 60:
		return rexCompact
	}
	return rexFull
}

func (s rexSize) width() int {
	switch s {
	case rexFull:
		return 16
	case rexCompact:
		return 8
	case rexASCII:
		return rexASCIIWidth
	}
	return 0
}

func pixelOn(p byte) bool {
	switch p {
	case 'a', 'd', 'c', 'm', 'e':
		return true
	}
	return false
}

func rexCell(top, bottom byte, colour bool) string {
	if !colour {
		t, b := pixelOn(top), pixelOn(bottom)
		switch {
		case t && b:
			return "█"
		case t:
			return "▀"
		case b:
			return "▄"
		}
		return " "
	}
	switch {
	case top == '.' && bottom == '.':
		return " "
	case top == '.':
		return lipgloss.NewStyle().Foreground(rexPalette[bottom]).Render("▄")
	case bottom == '.':
		return lipgloss.NewStyle().Foreground(rexPalette[top]).Render("▀")
	}
	return lipgloss.NewStyle().Foreground(rexPalette[top]).Background(rexPalette[bottom]).Render("▀")
}

func renderSprite(rows []string, colour bool) []string {
	var out []string
	for y := 0; y < len(rows); y += 2 {
		var b strings.Builder
		for x := 0; x < len(rows[y]); x++ {
			bottom := byte('.')
			if y+1 < len(rows) {
				bottom = rows[y+1][x]
			}
			b.WriteString(rexCell(rows[y][x], bottom, colour))
		}
		out = append(out, b.String())
	}
	return out
}

func rexASCIIRows(m Mood) []string {
	rows := append([]string(nil), rexASCIIArt...)
	switch m {
	case MoodSad:
		rows[1] = strings.Replace(rows[1], "o", "-", 1)
	case MoodSleep:
		rows[1] = strings.Replace(rows[1], "o", "-", 1)
	}
	return rows
}

func rexArt(lk look, size rexSize, f rexFrame, m Mood) []string {
	switch size {
	case rexFull, rexCompact:
		return renderSprite(spriteRows(size, f), lk.colour)
	case rexASCII:
		rows := rexASCIIRows(m)
		for i, r := range rows {
			rows[i] = lk.th.AccentStrong.Render(r + strings.Repeat(" ", rexASCIIWidth-len(r)))
		}
		return rows
	}
	return []string{lk.th.AccentStrong.Bold(true).Render("Rex") + " " + lk.th.Muted.Render(selectMark(lk))}
}

func padRight(s string, w int) string {
	if d := w - ansi.StringWidth(s); d > 0 {
		return s + strings.Repeat(" ", d)
	}
	return s
}

var unitGap = regexp.MustCompile(`(\d) ((?:[KMGTP]i?B|kB|B|ms|s|h|min|seconds?|minutes?|hours?|days?|weeks?|months?|years?|UTC|vCPUs?|CPUs?|cores?|January|February|March|April|May|June|July|August|September|October|November|December)\b)`)

const heldSpace = "\u00a0"

func holdUnits(s string) string {
	return unitGap.ReplaceAllString(s, "$1"+heldSpace+"$2")
}

func releaseUnits(s string) string {
	return strings.ReplaceAll(s, heldSpace, " ")
}

func breaksLine(r rune) bool {
	return unicode.IsSpace(r) && r != '\u00a0'
}

func wordwrap(s string, w int) string {
	var lines []string
	for _, para := range strings.Split(holdUnits(s), "\n") {
		line := ""
		for _, word := range strings.FieldsFunc(para, breaksLine) {
			switch {
			case line == "":
				line = word
			case ansi.StringWidth(line)+1+ansi.StringWidth(word) <= w:
				line += " " + word
			default:
				lines = append(lines, line)
				line = word
			}
			for ansi.StringWidth(line) > w && w > 0 {
				lines = append(lines, ansi.Cut(line, 0, w))
				line = ansi.Cut(line, w, ansi.StringWidth(line))
			}
		}
		lines = append(lines, line)
	}
	return releaseUnits(strings.Join(lines, "\n"))
}

const heldHyphen = "\u2011"

func holdHyphens(s string) string {
	return holdUnits(strings.ReplaceAll(s, "-", heldHyphen))
}

func releaseHyphens(s string) string {
	return releaseUnits(strings.ReplaceAll(s, heldHyphen, "-"))
}

func wrapKeepHyphens(s string, w int) string {
	return releaseHyphens(ansi.Wrap(breakLongWords(holdHyphens(s), w), w, ""))
}

func breakLongWords(s string, w int) string {
	if w < 2 {
		return s
	}
	fields := strings.Split(s, " ")
	for i, f := range fields {
		var parts []string
		for ansi.StringWidth(f) > w && !strings.Contains(f, "\x1b") {
			cut := wordBreak(f, w)
			if cut <= 0 || cut >= len(f) {
				break
			}
			parts = append(parts, f[:cut])
			f = f[cut:]
		}
		fields[i] = strings.Join(append(parts, f), " ")
	}
	return strings.Join(fields, " ")
}

func wordBreak(f string, w int) int {
	head := ansi.Cut(f, 0, w)
	if i := strings.LastIndex(head, "/"); i > 0 {
		return i + 1
	}
	var cuts []int
	for _, sep := range []string{heldHyphen, ".", "_"} {
		for i, at := 0, 0; ; at += i + len(sep) {
			if i = strings.Index(head[at:], sep); i < 0 {
				break
			}
			if at+i > 0 {
				cuts = append(cuts, at+i+len(sep))
			}
		}
	}
	slices.Sort(cuts)
	for i := len(cuts) - 1; i >= 0; i-- {
		if ansi.StringWidth(f[cuts[i]:]) >= w/3 {
			return cuts[i]
		}
	}
	if len(cuts) == 0 {
		return 0
	}
	return cuts[len(cuts)-1]
}

func wordwrapKeepHyphens(s string, w int) string {
	return releaseHyphens(ansi.Wordwrap(holdHyphens(s), w, ""))
}

func wrapAll(lines []string, w int) []string {
	if w < 10 {
		return lines
	}
	var out []string
	for _, l := range lines {
		out = append(out, strings.Split(wrapKeepHyphens(l, w), "\n")...)
	}
	return out
}

func revealLines(lines []string, shown int) []string {
	if shown < 0 {
		return lines
	}
	out := make([]string, len(lines))
	for i, l := range lines {
		r := []rune(l)
		n := max(0, min(len(r), shown))
		out[i] = string(r[:n]) + strings.Repeat(" ", len(r)-n)
		shown -= len(r) + 1
	}
	return out
}

func bubbleLines(lk look, text string, width int, tone Tone, shown int) []string {
	inner := max(10, width-4)
	wrapped := strings.Split(wordwrap(text, inner), "\n")
	w := 0
	for _, l := range wrapped {
		w = max(w, ansi.StringWidth(l))
	}
	wrapped = revealLines(wrapped, shown)
	b := lk.g.Border
	edge := lk.th.Border
	if tone != ToneNeutral {
		edge = lk.th.tone(tone)
	}
	out := []string{edge.Render(b.TopLeft + strings.Repeat(b.Top, w+2) + b.TopRight)}
	for _, l := range wrapped {
		out = append(out, edge.Render(b.Left)+" "+padRight(l, w)+" "+edge.Render(b.Right))
	}
	out = append(out, edge.Render(b.BottomLeft+strings.Repeat(b.Bottom, w+2)+b.BottomRight))
	return out
}

func bubbleTail(lk look, tone Tone) (string, string) {
	edge := lk.th.Border
	if tone != ToneNeutral {
		edge = lk.th.tone(tone)
	}
	if lk.g.ascii {
		return edge.Render("<"), edge.Render("+")
	}
	return edge.Render("─"), edge.Render("┤")
}

func besideRex(lk look, art []string, artWidth int, side []string, tailRow int, bubble bool, tone Tone) []string {
	h := max(len(art), len(side))
	top := max(0, (len(art)-len(side))/2)
	if bubble && len(side) > 0 {
		top = max(0, min(tailRow-1, len(art)-len(side)))
		if len(side) >= len(art) {
			top = 0
		}
	}
	out := make([]string, 0, h)
	for i := 0; i < h; i++ {
		left := strings.Repeat(" ", artWidth)
		if i < len(art) {
			left = padRight(art[i], artWidth)
		}
		j := i - top
		if j < 0 || j >= len(side) {
			out = append(out, strings.TrimRight(" "+left, " "))
			continue
		}
		gap := "   "
		line := side[j]
		if bubble && i == tailRow && j > 0 && j < len(side)-1 {
			dash, join := bubbleTail(lk, tone)
			gap = "  " + dash
			line = join + cutFirstCell(side[j])
		}
		out = append(out, strings.TrimRight(" "+left+gap+line, " "))
	}
	return out
}

func cutFirstCell(s string) string {
	w := ansi.StringWidth(s)
	if w <= 1 {
		return ""
	}
	return ansi.Cut(s, 1, w)
}
