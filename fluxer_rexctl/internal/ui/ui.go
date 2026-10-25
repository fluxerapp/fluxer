// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"regexp"
	"strings"
	"sync"
	"unicode"

	"charm.land/lipgloss/v2"
	"github.com/charmbracelet/colorprofile"
	"github.com/charmbracelet/x/ansi"
	"golang.org/x/term"
)

type Mode int

const (
	ModeRich Mode = iota
	ModePlain
	ModeJSON
)

type Options struct {
	Context context.Context
	Yes     bool
	JSON    bool
	Quiet   bool
	Verbose bool
	Stdin   io.Reader
	Stdout  io.Writer
	Stderr  io.Writer
}

type UI struct {
	opts        Options
	mode        Mode
	interactive bool
	accessible  bool
	colour      bool
	stdoutTTY   bool
	stderrTTY   bool
	dark        bool
	theme       Theme
	glyphs      Glyphs
	errw        io.Writer
	outw        io.Writer

	themeOnce sync.Once
	mu        sync.Mutex
	spin      *spinner
	self      *regexp.Regexp
	selfName  string
}

func New(opts Options) *UI {
	if opts.Stdin == nil {
		opts.Stdin = os.Stdin
	}
	if opts.Stdout == nil {
		opts.Stdout = os.Stdout
	}
	if opts.Stderr == nil {
		opts.Stderr = os.Stderr
	}
	u := &UI{
		opts:       opts,
		accessible: os.Getenv("ACCESSIBLE") != "",
		stdoutTTY:  isTerminal(opts.Stdout),
		stderrTTY:  isTerminal(opts.Stderr),
		dark:       true,
		errw:       opts.Stderr,
		outw:       opts.Stdout,
	}
	u.colour = u.stderrTTY && colourAllowed()
	switch {
	case opts.JSON:
		u.mode = ModeJSON
	case u.colour:
		u.mode = ModeRich
	default:
		u.mode = ModePlain
	}
	u.interactive = isTerminal(opts.Stdin) && u.stderrTTY && !opts.Yes && !opts.JSON
	if u.colour {
		u.theme = NewTheme(true)
		u.glyphs = RichGlyphs
		u.errw = colorprofile.NewWriter(opts.Stderr, os.Environ())
	} else {
		u.theme = plainTheme()
		u.glyphs = PlainGlyphs
	}
	if u.stdoutTTY && colourAllowed() {
		u.outw = colorprofile.NewWriter(opts.Stdout, os.Environ())
	}
	return u
}

func colourAllowed() bool {
	return os.Getenv("NO_COLOR") == "" && os.Getenv("TERM") != "dumb"
}

func isTerminal(v any) bool {
	f, ok := v.(interface{ Fd() uintptr })
	return ok && term.IsTerminal(int(f.Fd()))
}

func hasDarkBackground(in io.Reader, out io.Writer) bool {
	fin, ok := in.(*os.File)
	if !ok || !term.IsTerminal(int(fin.Fd())) {
		return true
	}
	fout, ok := out.(*os.File)
	if !ok {
		return true
	}
	return lipgloss.HasDarkBackground(fin, fout)
}

func (u *UI) Mode() Mode        { return u.mode }
func (u *UI) Interactive() bool { return u.interactive }
func (u *UI) Yes() bool         { return u.opts.Yes }
func (u *UI) Quiet() bool       { return u.opts.Quiet }
func (u *UI) Verbose() bool     { return u.opts.Verbose }
func (u *UI) Stdin() io.Reader  { return u.opts.Stdin }
func (u *UI) Stdout() io.Writer { return u.opts.Stdout }
func (u *UI) Stderr() io.Writer { return u.opts.Stderr }
func (u *UI) Theme() Theme      { return u.themed() }
func (u *UI) Glyphs() Glyphs    { return u.glyphs }
func (u *UI) ShowsRex() bool    { return u.mode == ModeRich && !u.opts.Quiet }

func (u *UI) themed() Theme {
	u.themeOnce.Do(func() {
		if u.colour && !u.accessible {
			u.dark = hasDarkBackground(u.opts.Stdin, u.opts.Stderr)
			u.theme = NewTheme(u.dark)
		}
	})
	return u.theme
}

func (u *UI) CallSelf(name string, commands []string) {
	if name == "" || len(commands) == 0 {
		return
	}
	u.self = regexp.MustCompile(`(^|[\s(])rexctl (` + strings.Join(commands, "|") + `)\b`)
	u.selfName = strings.ReplaceAll(name, "$", "$$")
}

func (u *UI) emit(s string) {
	u.mu.Lock()
	defer u.mu.Unlock()
	if u.spin != nil {
		u.spin.clear()
	}
	if u.self != nil {
		s = u.self.ReplaceAllString(s, "${1}"+u.selfName+" ${2}")
	}
	fmt.Fprint(u.errw, s)
}

func (u *UI) glyphLine(glyph string, style lipgloss.Style, msg, fix string) {
	var b strings.Builder
	b.WriteString(style.Render(glyph))
	b.WriteString(" ")
	b.WriteString(msg)
	b.WriteString("\n")
	if fix != "" {
		b.WriteString("  ")
		b.WriteString(fix)
		b.WriteString("\n")
	}
	u.emit(b.String())
}

func (u *UI) OK(format string, args ...any) {
	if u.opts.Quiet || u.mode == ModeJSON {
		return
	}
	u.glyphLine(u.glyphs.OK, u.theme.OK, fmt.Sprintf(format, args...), "")
}

func (u *UI) Warn(msg, fix string) {
	u.glyphLine(u.glyphs.Warn, u.theme.Warn, msg, fix)
}

func (u *UI) Fail(msg, fix string) {
	u.glyphLine(u.glyphs.Fail, u.theme.Fail, msg, fix)
}

func (u *UI) Info(format string, args ...any) {
	if u.opts.Quiet || u.mode == ModeJSON {
		return
	}
	u.emit(fmt.Sprintf(format, args...) + "\n")
}

func (u *UI) Detail(format string, args ...any) {
	if !u.opts.Verbose {
		return
	}
	u.emit(u.theme.Dim.Render(fmt.Sprintf(format, args...)) + "\n")
}

func (u *UI) Result(s string) {
	fmt.Fprintln(u.opts.Stdout, s)
}

func (u *UI) Highlight(s string) {
	if !u.stdoutTTY {
		fmt.Fprintln(u.opts.Stdout, s)
		return
	}
	style := lipgloss.NewStyle()
	if u.outw != u.opts.Stdout {
		u.themed()
		style = style.Bold(true).Foreground(AccentColor(lipgloss.LightDark(u.dark)))
	}
	fmt.Fprintf(u.outw, "\n    %s\n\n", style.Render(s))
}

func (u *UI) JSON(v any) error {
	enc := json.NewEncoder(u.opts.Stdout)
	enc.SetIndent("", "  ")
	enc.SetEscapeHTML(false)
	return enc.Encode(v)
}

func (u *UI) Table(headers []string, rows [][]string) {
	widths := make([]int, len(headers))
	for i, h := range headers {
		widths[i] = ansi.StringWidth(h)
	}
	for _, r := range rows {
		for i, c := range r {
			if i < len(widths) {
				widths[i] = max(widths[i], ansi.StringWidth(c))
			}
		}
	}
	format := func(cells []string) string {
		parts := make([]string, len(cells))
		for i, c := range cells {
			if i == len(cells)-1 || i >= len(widths) {
				parts[i] = c
				continue
			}
			parts[i] = c + strings.Repeat(" ", widths[i]-ansi.StringWidth(c))
		}
		return strings.TrimRight(strings.Join(parts, "   "), " ")
	}
	total := 3 * (len(widths) - 1)
	for _, w := range widths {
		total += w
	}
	if cols := u.columns(); cols > 0 && total > cols && len(headers) > 2 {
		u.records(headers, rows)
		return
	}
	head := format(headers)
	if u.outw != u.opts.Stdout {
		head = lipgloss.NewStyle().Bold(true).Render(head)
	}
	fmt.Fprintln(u.outw, head)
	for _, r := range rows {
		fmt.Fprintln(u.outw, format(r))
	}
}

func (u *UI) columns() int {
	f, ok := u.opts.Stdout.(interface{ Fd() uintptr })
	if !ok || !term.IsTerminal(int(f.Fd())) {
		return 0
	}
	w, _, err := term.GetSize(int(f.Fd()))
	if err != nil {
		return 0
	}
	return w
}

func (u *UI) records(headers []string, rows [][]string) {
	label := 0
	for _, h := range headers {
		label = max(label, ansi.StringWidth(h))
	}
	for i, r := range rows {
		if i > 0 {
			fmt.Fprintln(u.outw)
		}
		for j, h := range headers {
			if j >= len(r) || r[j] == "" {
				continue
			}
			name := h + strings.Repeat(" ", label-ansi.StringWidth(h))
			if u.outw != u.opts.Stdout {
				name = lipgloss.NewStyle().Bold(true).Render(name)
			}
			fmt.Fprintf(u.outw, "%s   %s\n", name, r[j])
		}
	}
}

func (u *UI) ReportError(err error) {
	if err == nil {
		return
	}
	var ee *ExitError
	if !errors.As(err, &ee) {
		u.Fail(sentence(err.Error()), "")
		return
	}
	if ee.Code == ExitInterrupted && ee.Msg == "" {
		return
	}
	msg := ee.Msg
	if msg == "" && ee.Err != nil {
		msg = sentence(ee.Err.Error())
	}
	if ee.Code == ExitUsage {
		var b strings.Builder
		b.WriteString(msg + "\n")
		if ee.Fix != "" {
			b.WriteString(ee.Fix + "\n")
		}
		u.emit(b.String())
	} else {
		u.Fail(msg, ee.Fix)
	}
	if ee.Msg == "" {
		return
	}
	for cause := ee.Err; cause != nil; {
		var inner *ExitError
		if !errors.As(cause, &inner) {
			u.Detail("%v", cause)
			return
		}
		if inner.Err == nil {
			u.Detail("%v", inner)
			return
		}
		cause = inner.Err
	}
}

func sentence(s string) string {
	s = strings.TrimSpace(s)
	if s == "" {
		return s
	}
	r := []rune(s)
	r[0] = unicode.ToUpper(r[0])
	s = string(r)
	if !strings.HasSuffix(s, ".") && !strings.HasSuffix(s, "?") {
		s += "."
	}
	return s
}

func QuietTerminal() func() {
	if colourAllowed() || !isTerminal(os.Stdout) {
		return func() {}
	}
	real := os.Stdout
	r, w, err := os.Pipe()
	if err != nil {
		return func() {}
	}
	done := make(chan struct{})
	go func() {
		_, _ = io.Copy(real, r)
		close(done)
	}()
	os.Stdout = w
	return func() {
		os.Stdout = real
		_ = w.Close()
		<-done
		_ = r.Close()
	}
}

func JoinAnd(items []string) string { return joinLast(items, " and ") }

func JoinOr(items []string) string { return joinLast(items, " or ") }

func joinLast(items []string, last string) string {
	if len(items) < 2 {
		return strings.Join(items, "")
	}
	return strings.Join(items[:len(items)-1], ", ") + last + items[len(items)-1]
}
