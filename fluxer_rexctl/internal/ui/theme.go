// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"image/color"

	"charm.land/fang/v2"
	"charm.land/huh/v2"
	"charm.land/lipgloss/v2"
)

var (
	Indigo   = lipgloss.Color("#4641d9")
	Lavender = lipgloss.Color("#b0adff")
)

type Theme struct {
	Accent  lipgloss.Style
	Dim     lipgloss.Style
	OK      lipgloss.Style
	Warn    lipgloss.Style
	Fail    lipgloss.Style
	Heading lipgloss.Style
}

type Glyphs struct {
	OK   string
	Warn string
	Fail string
}

var (
	RichGlyphs  = Glyphs{OK: "✓", Warn: "!", Fail: "✗"}
	PlainGlyphs = Glyphs{OK: "ok", Warn: "!", Fail: "x"}
)

func AccentColor(ld lipgloss.LightDarkFunc) color.Color {
	return ld(Indigo, Lavender)
}

func NewTheme(dark bool) Theme {
	ld := lipgloss.LightDark(dark)
	return Theme{
		Accent:  lipgloss.NewStyle().Foreground(AccentColor(ld)),
		Dim:     lipgloss.NewStyle().Faint(true),
		OK:      lipgloss.NewStyle().Foreground(lipgloss.Green),
		Warn:    lipgloss.NewStyle().Foreground(lipgloss.Yellow),
		Fail:    lipgloss.NewStyle().Foreground(lipgloss.Red),
		Heading: lipgloss.NewStyle().Bold(true).Foreground(AccentColor(ld)),
	}
}

func plainTheme() Theme {
	s := lipgloss.NewStyle()
	return Theme{Accent: s, Dim: s, OK: s, Warn: s, Fail: s, Heading: s}
}

func FangColorScheme(ld lipgloss.LightDarkFunc) fang.ColorScheme {
	accent := AccentColor(ld)
	cs := fang.DefaultColorScheme(ld)
	cs.Title = accent
	cs.Codeblock = ld(lipgloss.Color("#eeedfb"), lipgloss.Color("#1e1c3a"))
	cs.Program = accent
	cs.Command = accent
	cs.Flag = accent
	cs.QuotedString = ld(lipgloss.Color("#3a3846"), lipgloss.Color("#d6d4e8"))
	cs.ErrorHeader = [2]color.Color{lipgloss.Color("#ffffff"), lipgloss.Red}
	return cs
}

func HuhTheme(dark bool) *huh.Styles {
	t := huh.ThemeBase(dark)
	ld := lipgloss.LightDark(dark)
	accent := AccentColor(ld)
	text := ld(lipgloss.Color("235"), lipgloss.Color("252"))
	dim := ld(lipgloss.Color("243"), lipgloss.Color("246"))

	t.Focused.Base = t.Focused.Base.BorderForeground(accent)
	t.Focused.Card = t.Focused.Base
	t.Focused.Title = t.Focused.Title.Foreground(accent).Bold(true)
	t.Focused.NoteTitle = t.Focused.NoteTitle.Foreground(accent).Bold(true).MarginBottom(1)
	t.Focused.Description = t.Focused.Description.Foreground(dim)
	t.Focused.ErrorIndicator = t.Focused.ErrorIndicator.Foreground(lipgloss.Red)
	t.Focused.ErrorMessage = t.Focused.ErrorMessage.Foreground(lipgloss.Red)
	t.Focused.SelectSelector = t.Focused.SelectSelector.Foreground(accent)
	t.Focused.NextIndicator = t.Focused.NextIndicator.Foreground(accent)
	t.Focused.PrevIndicator = t.Focused.PrevIndicator.Foreground(accent)
	t.Focused.Option = t.Focused.Option.Foreground(text)
	t.Focused.SelectedOption = t.Focused.SelectedOption.Foreground(accent)
	t.Focused.UnselectedOption = t.Focused.UnselectedOption.Foreground(text)
	t.Focused.MultiSelectSelector = t.Focused.MultiSelectSelector.Foreground(accent)
	t.Focused.FocusedButton = t.Focused.FocusedButton.Foreground(lipgloss.Color("#ffffff")).Background(Indigo)
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
	return t
}
