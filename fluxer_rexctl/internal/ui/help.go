// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"fmt"
	"io"
	"regexp"
	"strings"

	"github.com/charmbracelet/x/ansi"
	"github.com/spf13/cobra"
	"github.com/spf13/pflag"
	"golang.org/x/term"
)

const helpMaxWidth = 120

type rowKind int

const (
	rowCommand rowKind = iota
	rowFlag
	rowCode
)

type helpRow struct {
	key, extra, help string
	kind             rowKind
}

type helpSection struct {
	title string
	rows  []helpRow
	own   bool
}

var exitCodeRows = []helpRow{
	{key: fmt.Sprint(ExitOK), help: "ok", kind: rowCode},
	{key: fmt.Sprint(ExitFailed), help: "failed", kind: rowCode},
	{key: fmt.Sprint(ExitUsage), help: "usage", kind: rowCode},
	{key: fmt.Sprint(ExitRefused), help: "refused, or doctor found a problem", kind: rowCode},
	{key: fmt.Sprint(ExitInterrupted), help: "interrupted", kind: rowCode},
}

func (u *UI) Help(c *cobra.Command) {
	u.mu.Lock()
	colour := u.stdoutTTY && colourAllowed(u.opts.Getenv) && !u.opts.NoColor && !u.accessible
	w := u.outw
	u.mu.Unlock()
	t := plainTheme()
	if colour {
		t = newTheme(true)
		if u.colour {
			t = u.themed()
		}
	}
	_, _ = io.WriteString(w, renderHelp(c, t, u.helpWidth()))
}

func (u *UI) helpWidth() int {
	f, ok := u.opts.Stdout.(interface{ Fd() uintptr })
	if !ok || !term.IsTerminal(int(f.Fd())) {
		return 0
	}
	cols, _, err := term.GetSize(int(f.Fd()))
	if err != nil || cols <= 0 {
		return 0
	}
	return min(cols, helpMaxWidth)
}

func renderHelp(c *cobra.Command, t Theme, width int) string {
	var b strings.Builder
	if text := strings.TrimSpace(c.Long); text != "" || c.Short != "" {
		if text == "" {
			text = c.Short
		}
		limit := width
		if limit == 0 {
			limit = helpMaxWidth
		}
		b.WriteString("\n" + indentLines(wrapSpaces(text, limit-2), "  ") + "\n")
	}
	writeHelpTitle(&b, t, "Usage")
	b.WriteString("    " + useLine(c, t, true) + "\n")
	if example := strings.TrimSpace(c.Example); example != "" {
		writeHelpTitle(&b, t, "Examples")
		for _, line := range strings.Split(example, "\n") {
			if line = strings.TrimSpace(line); line != "" {
				b.WriteString("    " + line + "\n")
			}
		}
	}
	var sections []helpSection
	titles := map[string]string{"": "Commands"}
	order := []string{""}
	for _, g := range c.Groups() {
		titles[g.ID] = g.Title
		order = append(order, g.ID)
	}
	byGroup := map[string][]helpRow{}
	for _, sc := range c.Commands() {
		if sc.Hidden {
			continue
		}
		byGroup[sc.GroupID] = append(byGroup[sc.GroupID], helpRow{key: useLine(sc, plainTheme(), false), help: sc.Short})
	}
	for _, id := range order {
		if rows := byGroup[id]; len(rows) > 0 {
			sections = append(sections, helpSection{title: titles[id], rows: rows})
		}
	}
	c.InheritedFlags()
	var flags []helpRow
	c.Flags().VisitAll(func(f *pflag.Flag) {
		if f.Hidden {
			return
		}
		key := "--" + f.Name
		if f.Shorthand != "" {
			key = "-" + f.Shorthand + " " + key
		}
		row := helpRow{key: key, help: Capitalise(f.Usage), kind: rowFlag}
		switch f.DefValue {
		case "true":
			if f.Value.Type() == "bool" && !strings.Contains(f.Usage, "--no-"+f.Name) {
				row.extra = " (on unless you pass --" + f.Name + "=false)"
			}
		case "", "false", "0", "[]":
		default:
			row.extra = " (" + f.DefValue + ")"
		}
		flags = append(flags, row)
	})
	if len(flags) > 0 {
		sections = append(sections, helpSection{title: "Flags", rows: flags})
	}
	if !c.HasParent() {
		sections = append(sections, helpSection{title: "Exit codes", rows: exitCodeRows, own: true})
	}
	shared := 10
	for _, s := range sections {
		if !s.own {
			shared = max(shared, keyColumn(s.rows))
		}
	}
	for _, s := range sections {
		space := shared
		if s.own {
			space = max(8, keyColumn(s.rows))
		}
		writeHelpTitle(&b, t, s.title)
		for _, r := range s.rows {
			b.WriteString(helpLine(r, t, space, width))
		}
	}
	b.WriteString("\n")
	return b.String()
}

func keyColumn(rows []helpRow) int {
	n := 0
	for _, r := range rows {
		n = max(n, ansi.StringWidth(r.key)+2)
	}
	return n
}

func writeHelpTitle(b *strings.Builder, t Theme, title string) {
	b.WriteString("\n  " + t.Heading.Render(title) + "\n\n")
}

func helpLine(r helpRow, t Theme, space, width int) string {
	key := t.Accent.Render(r.key)
	switch r.kind {
	case rowCode:
		key = t.Bold.Render(r.key)
	case rowCommand:
		if name, rest, ok := strings.Cut(r.key, " "); ok {
			key = t.Accent.Render(name) + t.Muted.Render(" "+rest)
		}
	}
	head := "    " + key
	col := 4 + space
	text := r.help + r.extra
	if width == 0 || col+ansi.StringWidth(text) <= width {
		return head + strings.Repeat(" ", space-ansi.StringWidth(r.key)) + styleHelpText(text, r, t) + "\n"
	}
	if width-col < 24 {
		indent := strings.Repeat(" ", 6)
		return head + "\n" + indent + styleHelpText(strings.ReplaceAll(wrapSpaces(text, width-len(indent)), "\n", "\n"+indent), r, t) + "\n"
	}
	body := strings.ReplaceAll(wrapSpaces(text, width-col), "\n", "\n"+strings.Repeat(" ", col))
	return head + strings.Repeat(" ", space-ansi.StringWidth(r.key)) + styleHelpText(body, r, t) + "\n"
}

func styleHelpText(s string, r helpRow, t Theme) string {
	if r.extra == "" || !strings.HasSuffix(s, strings.TrimSpace(r.extra)) {
		return s
	}
	cut := len(s) - len(strings.TrimSpace(r.extra))
	return s[:cut] + t.Muted.Render(s[cut:])
}

var bracketArgs = regexp.MustCompile(`\[[^\]]*\]`)

func useLine(c *cobra.Command, t Theme, complete bool) string {
	u := c.Use
	if complete {
		u = c.UseLine()
	}
	hasFlags := strings.Contains(u, "[flags]") || strings.Contains(u, "[--flags]") || c.HasFlags() || c.HasPersistentFlags() || c.HasAvailableFlags()
	hasCommands := strings.Contains(u, "[command]") || c.HasAvailableSubCommands()
	for _, k := range []string{"[args]", "[flags]", "[--flags]", "[command]"} {
		u = strings.ReplaceAll(u, k, "")
	}
	var extra []string
	for _, arg := range bracketArgs.FindAllString(u, -1) {
		u = strings.Replace(u, arg, "", 1)
		extra = append(extra, arg)
	}
	var tail []string
	if hasCommands {
		tail = append(tail, "[command]")
	}
	tail = append(tail, extra...)
	if hasFlags {
		tail = append(tail, "[--flags]")
	}
	line := t.Code.Render(strings.Join(strings.Fields(u), " "))
	if len(tail) > 0 {
		line += t.Muted.Render(" " + strings.Join(tail, " "))
	}
	return line
}

func Capitalise(s string) string {
	if s == "" {
		return s
	}
	return strings.ToUpper(s[:1]) + s[1:]
}

func indentLines(s, indent string) string {
	lines := strings.Split(s, "\n")
	for i, l := range lines {
		if l != "" {
			lines[i] = indent + l
		}
	}
	return strings.Join(lines, "\n")
}

func wrapSpaces(s string, limit int) string {
	if limit <= 0 {
		return s
	}
	var out []string
	for _, para := range strings.Split(holdUnits(s), "\n") {
		var cur strings.Builder
		width := 0
		for _, word := range strings.Split(para, " ") {
			ww := ansi.StringWidth(word)
			switch {
			case width == 0:
				cur.WriteString(word)
				width = ww
			case width+1+ww <= limit:
				cur.WriteString(" " + word)
				width += 1 + ww
			default:
				out = append(out, cur.String())
				cur.Reset()
				cur.WriteString(word)
				width = ww
			}
		}
		out = append(out, cur.String())
	}
	return releaseUnits(strings.Join(out, "\n"))
}
