// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"image/color"
	"strings"

	"charm.land/lipgloss/v2"
)

const (
	Greeting     = "Hi, I'm Rex."
	WelcomeLine  = "Let's get your Fluxer instance running."
	FinishLine   = "All set. Your instance is ready."
	AllGreenLine = "Everything looks good."
	rexWidth     = 16
)

var rexPalette = map[byte]color.Color{
	'a': lipgloss.Color("#6f6bf2"),
	'b': Indigo,
	'c': lipgloss.Color("#2b2794"),
	'e': lipgloss.Color("#1c1a4a"),
}

var rexSprite = []string{
	".........aaaaaa.",
	"........aaeaaaaa",
	"........aaaaaaaa",
	"........aaacc...",
	"........aaaaaa..",
	"........aaaa....",
	"a......aaaaab...",
	"aa....aaaaaab...",
	".aaaaaaaaaaa....",
	"..aaaaaaaaa.....",
	"......aa.aa.....",
	"......bb.bb.....",
}

func rexPixel(x, y int) byte {
	if y >= len(rexSprite) || x >= len(rexSprite[y]) {
		return '.'
	}
	return rexSprite[y][x]
}

func rexCell(top, bottom byte) string {
	switch {
	case top == '.' && bottom == '.':
		return " "
	case top == '.':
		return lipgloss.NewStyle().Foreground(rexPalette[bottom]).Render("▄")
	case bottom == '.':
		return lipgloss.NewStyle().Foreground(rexPalette[top]).Render("▀")
	default:
		return lipgloss.NewStyle().Foreground(rexPalette[top]).Background(rexPalette[bottom]).Render("▀")
	}
}

func rexArt() []string {
	var rows []string
	for y := 0; y < len(rexSprite); y += 2 {
		var b strings.Builder
		for x := 0; x < rexWidth; x++ {
			b.WriteString(rexCell(rexPixel(x, y), rexPixel(x, y+1)))
		}
		rows = append(rows, b.String())
	}
	return rows
}

func (u *UI) Rex(lines ...string) {
	if !u.ShowsRex() {
		return
	}
	art := rexArt()
	first := (len(art) - len(lines)) / 2
	var b strings.Builder
	b.WriteString("\n")
	for i, row := range art {
		if i >= first && i-first < len(lines) {
			row += "   " + lines[i-first]
		}
		b.WriteString(row + "\n")
	}
	b.WriteString("\n")
	u.emit(b.String())
}

func (u *UI) Welcome() {
	u.Rex(Greeting, WelcomeLine)
}

func (u *UI) Finish(url string) {
	if u.ShowsRex() {
		u.Rex(FinishLine)
	} else {
		u.Info("Your instance is ready.")
	}
	if url != "" {
		u.Highlight(url)
	}
}

func (u *UI) AllGreen() {
	u.Rex(AllGreenLine)
}
