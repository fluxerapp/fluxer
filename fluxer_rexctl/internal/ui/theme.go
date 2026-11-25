// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"image/color"

	"charm.land/huh/v2"
	"charm.land/lipgloss/v2"
)

var (
	indigo   = lipgloss.Color("#4641d9")
	lavender = lipgloss.Color("#b0adff")
)

type Theme struct {
	Accent       lipgloss.Style
	AccentStrong lipgloss.Style
	OK           lipgloss.Style
	Warn         lipgloss.Style
	Fail         lipgloss.Style
	Muted        lipgloss.Style
	Border       lipgloss.Style
	Surface      lipgloss.Style
	Heading      lipgloss.Style
	Code         lipgloss.Style
	Selected     lipgloss.Style
	Dim          lipgloss.Style
	Bold         lipgloss.Style
	plain        bool
}

type Glyphs struct {
	OK, Warn, Fail, Active, Pending, Skipped string
	RailTop, Rail, RailBottom                string
	Bullet, Arrow, Ellipsis                  string
	BarFull, BarHead, BarEmpty               string
	Spinner                                  []string
	Spark                                    []string
	Parts                                    [5]string
	Border                                   lipgloss.Border
	Confetti                                 []string
	ascii                                    bool
}

const gap = "   "

var unicodeGlyphs = Glyphs{
	OK: "✓", Warn: "▲", Fail: "✗", Active: "◆", Pending: "◇", Skipped: "–",
	RailTop: "┌", Rail: "│", RailBottom: "└",
	Bullet: "•", Arrow: "→", Ellipsis: "…",
	BarFull: "━", BarHead: "╸", BarEmpty: "─",
	Spinner:  []string{"⣾", "⣽", "⣻", "⢿", "⡿", "⣟", "⣯", "⣷"},
	Spark:    []string{"▁", "▂", "▃", "▄", "▅", "▆", "▇", "█"},
	Parts:    [5]string{"●", "◐", "○", "✗", "·"},
	Border:   lipgloss.RoundedBorder(),
	Confetti: []string{"·", "*", "+", "✦"},
}

var asciiGlyphs = Glyphs{
	OK: "ok", Warn: "!", Fail: "x", Active: ">", Pending: ".", Skipped: "-",
	RailTop: "+", Rail: "|", RailBottom: "+",
	Bullet: "*", Arrow: "->", Ellipsis: "...",
	BarFull: "#", BarHead: ">", BarEmpty: ".",
	Spinner:  []string{"|", "/", "-", "\\"},
	Spark:    []string{"_", ".", "-", "=", "+", "*", "#"},
	Parts:    [5]string{"*", "~", "o", "x", "."},
	Border:   asciiBorder,
	Confetti: []string{".", "*", "+", "o"},
	ascii:    true,
}

var asciiBorder = lipgloss.Border{
	Top: "-", Bottom: "-", Left: "|", Right: "|",
	TopLeft: "+", TopRight: "+", BottomLeft: "+", BottomRight: "+",
	MiddleLeft: "+", MiddleRight: "+", Middle: "+", MiddleTop: "+", MiddleBottom: "+",
}

func glyphsFor(set GlyphSet) Glyphs {
	if set == GlyphsUnicode {
		return unicodeGlyphs
	}
	return asciiGlyphs
}

func (g Glyphs) ASCII() bool { return g.ascii }

func (g Glyphs) state(st State) string {
	switch st {
	case StateActive:
		return g.Active
	case StateOK:
		return g.OK
	case StateWarn:
		return g.Warn
	case StateFail:
		return g.Fail
	case StateSkipped:
		return g.Skipped
	}
	return g.Pending
}

func (g Glyphs) cell(s string) string {
	if !g.ascii {
		return s
	}
	for len(s) < 2 {
		s += " "
	}
	return s
}

func (g Glyphs) part(st State) string {
	switch st {
	case StateOK:
		return g.Parts[0]
	case StateActive, StateWarn:
		return g.Parts[1]
	case StateFail:
		return g.Parts[3]
	case StateSkipped:
		return g.Parts[4]
	}
	return g.Parts[2]
}

func accentColor(ld lipgloss.LightDarkFunc) color.Color {
	return ld(indigo, lavender)
}

func newTheme(dark bool) Theme {
	ld := lipgloss.LightDark(dark)
	c := func(light, dark string) color.Color { return ld(lipgloss.Color(light), lipgloss.Color(dark)) }
	accent := accentColor(ld)
	muted := lipgloss.NewStyle().Foreground(c("#6b6990", "#9a98c0"))
	surface := c("#eeedfb", "#1e1c3a")
	return Theme{
		Accent:       lipgloss.NewStyle().Foreground(accent),
		AccentStrong: lipgloss.NewStyle().Foreground(c("#4641d9", "#6f6bf2")),
		OK:           lipgloss.NewStyle().Foreground(c("#16794c", "#5fd7a7")),
		Warn:         lipgloss.NewStyle().Foreground(c("#8a5a00", "#ffcf66")),
		Fail:         lipgloss.NewStyle().Foreground(c("#b3261e", "#ff7a8a")),
		Muted:        muted,
		Border:       lipgloss.NewStyle().Foreground(c("#c9c7ef", "#3d3a6b")),
		Surface:      lipgloss.NewStyle().Background(surface),
		Heading:      lipgloss.NewStyle().Bold(true).Foreground(accent),
		Code:         lipgloss.NewStyle().Foreground(accent),
		Selected:     lipgloss.NewStyle().Background(surface).Bold(true),
		Dim:          muted,
		Bold:         lipgloss.NewStyle().Bold(true),
	}
}

func plainTheme() Theme {
	s := lipgloss.NewStyle()
	return Theme{Accent: s, AccentStrong: s, OK: s, Warn: s, Fail: s, Muted: s, Border: s, Surface: s, Heading: s, Code: s, Selected: s, Dim: s, Bold: s, plain: true}
}

func (t Theme) tone(tn Tone) lipgloss.Style {
	switch tn {
	case ToneAccent:
		return t.Accent
	case ToneOK:
		return t.OK
	case ToneWarn:
		return t.Warn
	case ToneFail:
		return t.Fail
	}
	return t.Border
}

func (t Theme) state(st State) lipgloss.Style {
	switch st {
	case StateActive:
		return t.Accent
	case StateOK:
		return t.OK
	case StateWarn:
		return t.Warn
	case StateFail:
		return t.Fail
	}
	return t.Muted
}

func huhTheme(dark bool) *huh.Styles {
	t := huh.ThemeBase(dark)
	th := newTheme(dark)
	ld := lipgloss.LightDark(dark)
	accent := accentColor(ld)
	text := ld(lipgloss.Color("235"), lipgloss.Color("252"))
	dim := ld(lipgloss.Color("#6b6990"), lipgloss.Color("#9a98c0"))
	fail := ld(lipgloss.Color("#b3261e"), lipgloss.Color("#ff7a8a"))

	t.Focused.Base = t.Focused.Base.BorderForeground(accent)
	t.Focused.Card = t.Focused.Base
	t.Focused.Title = t.Focused.Title.Foreground(accent).Bold(true)
	t.Focused.NoteTitle = t.Focused.NoteTitle.Foreground(accent).Bold(true).MarginBottom(1)
	t.Focused.Description = t.Focused.Description.Foreground(dim)
	t.Focused.ErrorIndicator = t.Focused.ErrorIndicator.Foreground(fail)
	t.Focused.ErrorMessage = t.Focused.ErrorMessage.Foreground(fail)
	t.Focused.SelectSelector = t.Focused.SelectSelector.Foreground(accent)
	t.Focused.NextIndicator = t.Focused.NextIndicator.Foreground(accent)
	t.Focused.PrevIndicator = t.Focused.PrevIndicator.Foreground(accent)
	t.Focused.Option = t.Focused.Option.Foreground(text)
	t.Focused.SelectedOption = t.Focused.SelectedOption.Foreground(accent)
	t.Focused.UnselectedOption = t.Focused.UnselectedOption.Foreground(text)
	t.Focused.MultiSelectSelector = t.Focused.MultiSelectSelector.Foreground(accent)
	t.Focused.SelectedPrefix = t.Focused.SelectedPrefix.Foreground(th.OK.GetForeground())
	t.Focused.FocusedButton = t.Focused.FocusedButton.Foreground(lipgloss.Color("#ffffff")).Background(indigo)
	t.Focused.Next = t.Focused.FocusedButton
	t.Focused.BlurredButton = t.Focused.BlurredButton.Foreground(text).Background(ld(lipgloss.Color("252"), lipgloss.Color("237")))
	t.Focused.TextInput.Cursor = t.Focused.TextInput.Cursor.Foreground(accent)
	t.Focused.TextInput.Placeholder = t.Focused.TextInput.Placeholder.Foreground(dim)
	t.Focused.TextInput.Prompt = t.Focused.TextInput.Prompt.Foreground(accent)

	t.Blurred = t.Focused
	t.Blurred.Base = t.Focused.Base.BorderStyle(lipgloss.HiddenBorder())
	t.Blurred.Card = t.Blurred.Base
	t.Blurred.NextIndicator = lipgloss.NewStyle()
	t.Blurred.PrevIndicator = lipgloss.NewStyle()

	t.Group.Title = t.Focused.Title
	t.Group.Description = t.Focused.Description
	t.Help.ShortKey = t.Help.ShortKey.Foreground(accent).Bold(true)
	t.Help.ShortDesc = t.Help.ShortDesc.Foreground(dim)
	t.Help.ShortSeparator = t.Help.ShortSeparator.Foreground(dim).Transform(func(string) string { return gap })
	t.Help.FullKey = t.Help.ShortKey
	t.Help.FullDesc = t.Help.ShortDesc
	t.Help.FullSeparator = t.Help.ShortSeparator
	return t
}

func huhPlainTheme(bool) *huh.Styles {
	t := huh.ThemeBase(true)
	clear := func(s lipgloss.Style) lipgloss.Style {
		return s.UnsetForeground().UnsetBackground().UnsetBold().UnsetFaint().UnsetItalic().UnsetUnderline()
	}
	for _, fs := range []*huh.FieldStyles{&t.Focused, &t.Blurred} {
		fs.Title = clear(fs.Title)
		fs.Description = clear(fs.Description)
		fs.ErrorIndicator = clear(fs.ErrorIndicator)
		fs.ErrorMessage = clear(fs.ErrorMessage)
		fs.SelectSelector = clear(fs.SelectSelector)
		fs.Option = clear(fs.Option)
		fs.SelectedOption = clear(fs.SelectedOption)
		fs.UnselectedOption = clear(fs.UnselectedOption)
		fs.NoteTitle = clear(fs.NoteTitle)
		fs.TextInput.Prompt = clear(fs.TextInput.Prompt)
		fs.TextInput.Text = clear(fs.TextInput.Text)
		fs.TextInput.Placeholder = clear(fs.TextInput.Placeholder)
	}
	t.Group.Title = clear(t.Group.Title)
	t.Group.Description = clear(t.Group.Description)
	t.Help.ShortKey, t.Help.ShortDesc = clear(t.Help.ShortKey), clear(t.Help.ShortDesc)
	t.Help.ShortSeparator = clear(t.Help.ShortSeparator).Transform(func(string) string { return gap })
	t.Help.FullKey, t.Help.FullDesc, t.Help.FullSeparator = t.Help.ShortKey, t.Help.ShortDesc, t.Help.ShortSeparator
	return t
}
