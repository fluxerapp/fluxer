// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"strings"

	"charm.land/lipgloss/v2"
	"rsc.io/qr"
)

const qrQuiet = 2

func (u *UI) QR(text string) []string {
	return u.qrRows(text, u.look())
}

func (u *UI) qrRows(text string, lk look) []string {
	if text == "" || !u.rich() || u.accessible || lk.g.ascii || u.Width() < 60 {
		return nil
	}
	return qrArt(text, lk.colour)
}

func qrArt(text string, colour bool) []string {
	code, err := qr.Encode(text, qr.L)
	if err != nil {
		return nil
	}
	n := code.Size + 2*qrQuiet
	light := func(x, y int) bool {
		x, y = x-qrQuiet, y-qrQuiet
		if x < 0 || y < 0 || x >= code.Size || y >= code.Size {
			return true
		}
		return !code.Black(x, y)
	}
	style := lipgloss.NewStyle()
	if colour {
		style = style.Foreground(lipgloss.Color("#ffffff")).Background(lipgloss.Color("#000000"))
	}
	var out []string
	for y := 0; y < n; y += 2 {
		var b strings.Builder
		for x := 0; x < n; x++ {
			top := light(x, y)
			bottom := y+1 < n && light(x, y+1)
			switch {
			case top && bottom:
				b.WriteString("█")
			case top:
				b.WriteString("▀")
			case bottom:
				b.WriteString("▄")
			default:
				b.WriteString(" ")
			}
		}
		out = append(out, style.Render(b.String()))
	}
	return out
}
