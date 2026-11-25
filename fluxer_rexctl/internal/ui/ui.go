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
	"slices"
	"strings"
	"sync"
	"time"
	"unicode"

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
	Context     context.Context
	Yes         bool
	JSON        bool
	Quiet       bool
	Verbose     bool
	NoColor     bool
	Plain       bool
	Accessible  bool
	NoRex       bool
	NoAnimation bool
	Progress    ProgressFormat
	Glyphs      GlyphSet
	Stdin       io.Reader
	Stdout      io.Writer
	Stderr      io.Writer
	Getenv      func(string) string
	Now         func() time.Time
	Width       int
}

type Display struct {
	NoColor     bool
	Plain       bool
	Accessible  bool
	NoRex       bool
	NoAnimation bool
	Progress    ProgressFormat
}

type UI struct {
	opts        Options
	display     Display
	tier        Tier
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
	linesRep    *linesReporter
	jsonRep     *jsonReporter

	themeOnce sync.Once
	mu        sync.Mutex
	liveMu    sync.Mutex
	live      *liveRun
	stack     []string
	failTail  []string
	failLog   string
	failShown []string
	steps     int
	changed   bool
	self      *regexp.Regexp
	selfName  string
	command   string
	began     time.Time
	ended     bool
	wrote     bool
	blank     bool
	endsNL    bool
}

func New(opts Options) *UI {
	u := &UI{}
	u.SetOptions(opts)
	return u
}

func (u *UI) SetDisplay(d Display) {
	u.mu.Lock()
	u.display = d
	opts := u.opts
	u.mu.Unlock()
	u.SetOptions(opts)
}

func (u *UI) SetOptions(opts Options) {
	if opts.Stdin == nil {
		opts.Stdin = os.Stdin
	}
	if opts.Stdout == nil {
		opts.Stdout = os.Stdout
	}
	if opts.Stderr == nil {
		opts.Stderr = os.Stderr
	}
	if opts.Getenv == nil {
		opts.Getenv = os.Getenv
	}
	if opts.Now == nil {
		opts.Now = time.Now
	}
	u.mu.Lock()
	defer u.mu.Unlock()
	d := u.display
	opts.NoColor = opts.NoColor || d.NoColor
	opts.Plain = opts.Plain || d.Plain
	opts.Accessible = opts.Accessible || d.Accessible
	opts.NoRex = opts.NoRex || d.NoRex
	opts.NoAnimation = opts.NoAnimation || d.NoAnimation
	if opts.Progress == "" || opts.Progress == ProgressAuto {
		opts.Progress = d.Progress
	}
	if opts.Progress == "" {
		opts.Progress = ProgressAuto
	}
	u.opts = opts
	u.stdoutTTY = isTerminal(opts.Stdout)
	u.stderrTTY = isTerminal(opts.Stderr)
	u.accessible = opts.Accessible || opts.Getenv("ACCESSIBLE") != ""
	u.tier = detectTier(opts, u.stderrTTY, u.accessible)
	u.colour = colourOn(opts, u.tier, u.stderrTTY, u.accessible)
	u.interactive = isTerminal(opts.Stdin) && u.stderrTTY && !opts.Yes && !opts.JSON && !opts.Plain && u.tier != TierJSON &&
		(u.tier != TierLines || u.accessible)
	u.dark = true
	u.themeOnce = sync.Once{}
	u.errw = opts.Stderr
	u.outw = opts.Stdout
	if u.colour {
		u.theme = newTheme(true)
		u.errw = colorprofile.NewWriter(opts.Stderr, environ(opts.Getenv))
		if forcedColour(opts.Getenv) && !u.stderrTTY {
			u.errw = &colorprofile.Writer{Forward: opts.Stderr, Profile: colorprofile.ANSI256}
		}
	} else {
		u.theme = plainTheme()
	}
	u.glyphs = glyphsFor(detectGlyphs(opts, u.accessible))
	if u.stdoutTTY && colourAllowed(opts.Getenv) && !opts.NoColor && !u.accessible {
		u.outw = colorprofile.NewWriter(opts.Stdout, environ(opts.Getenv))
	}
	u.linesRep = newLinesReporter(u)
	u.jsonRep = &jsonReporter{u: u, sent: map[string]time.Time{}}
}

func forcedColour(getenv func(string) string) bool {
	return forced(getenv("FORCE_COLOR")) || forced(getenv("CLICOLOR_FORCE"))
}

func environ(getenv func(string) string) []string {
	var out []string
	for _, k := range []string{"TERM", "COLORTERM", "NO_COLOR", "CLICOLOR", "CLICOLOR_FORCE", "FORCE_COLOR", "TERM_PROGRAM", "WT_SESSION", "LANG", "LC_ALL", "LC_CTYPE"} {
		if v := getenv(k); v != "" {
			out = append(out, k+"="+v)
		}
	}
	return out
}

func colourAllowed(getenv func(string) string) bool {
	return getenv("NO_COLOR") == "" && getenv("TERM") != "dumb"
}

func isTerminal(v any) bool {
	f, ok := v.(interface{ Fd() uintptr })
	return ok && term.IsTerminal(int(f.Fd()))
}

func (u *UI) Tier() Tier { return u.tier }

func (u *UI) Mode() Mode {
	switch {
	case u.opts.JSON:
		return ModeJSON
	case u.tier == TierLive || u.tier == TierStill:
		return ModeRich
	}
	return ModePlain
}

func (u *UI) Interactive() bool { return u.interactive }
func (u *UI) Accessible() bool  { return u.accessible }
func (u *UI) Colour() bool      { return u.colour }
func (u *UI) Yes() bool         { return u.opts.Yes }
func (u *UI) Quiet() bool       { return u.opts.Quiet }
func (u *UI) Verbose() bool     { return u.opts.Verbose }
func (u *UI) Stdin() io.Reader  { return u.opts.Stdin }
func (u *UI) Stdout() io.Writer { return u.opts.Stdout }
func (u *UI) Stderr() io.Writer { return u.opts.Stderr }
func (u *UI) Theme() Theme      { return u.themed() }
func (u *UI) Glyphs() Glyphs    { return u.glyphs }

func (u *UI) rich() bool { return u.tier == TierLive || u.tier == TierStill }

func (u *UI) ShowsRex() bool {
	return u.rich() && !u.opts.Quiet && !u.opts.JSON && !u.opts.NoRex &&
		!u.accessible && u.opts.Getenv("REXCTL_NO_REX") == ""
}

func (u *UI) Width() int {
	if u.opts.Width > 0 {
		return u.opts.Width
	}
	if f, ok := u.opts.Stderr.(interface{ Fd() uintptr }); ok && term.IsTerminal(int(f.Fd())) {
		if w, _, err := term.GetSize(int(f.Fd())); err == nil && w > 0 {
			return w
		}
	}
	return 80
}

func (u *UI) themed() Theme {
	u.themeOnce.Do(func() {
		if u.colour {
			u.dark = u.detectDark()
			u.theme = newTheme(u.dark)
		}
	})
	return u.theme
}

func (u *UI) CallSelf(name string, commands []string) {
	if name == "" || len(commands) == 0 {
		return
	}
	u.self = regexp.MustCompile(`(^|[\s(` + "`" + `])rexctl (` + strings.Join(commands, "|") + `)\b`)
	u.selfName = strings.ReplaceAll(name, "$", "$$")
}

func (u *UI) rewriteSelf(s string) string {
	if u.self == nil {
		return s
	}
	return u.self.ReplaceAllString(s, "${1}"+u.selfName+" ${2}")
}

func (u *UI) Begin(command string) {
	u.mu.Lock()
	u.command = command
	u.began = u.opts.Now()
	u.ended = false
	u.mu.Unlock()
	if u.tier == TierJSON {
		u.jsonRep.event("run.start", "command", command, "runtime", "", "release", "")
	}
}

func (u *UI) End(code int) {
	u.mu.Lock()
	began, ended := u.began, u.ended
	u.ended = true
	u.mu.Unlock()
	if u.tier == TierJSON && !began.IsZero() && !ended {
		u.jsonRep.event("run.done", "exit", code, "elapsed_ms", u.opts.Now().Sub(began).Milliseconds())
	}
}

func (u *UI) gap() string {
	u.mu.Lock()
	defer u.mu.Unlock()
	if u.wrote && !u.blank {
		return "\n"
	}
	return ""
}

func (u *UI) viewLeft() {
	u.mu.Lock()
	u.wrote, u.blank, u.endsNL = true, false, true
	u.mu.Unlock()
}

func (u *UI) emit(s string) {
	if s == "" {
		return
	}
	s = u.rewriteSelf(s)
	u.mu.Lock()
	lr := u.live
	if u.blank {
		s = strings.TrimLeft(s, "\n")
	}
	if s == "" {
		u.mu.Unlock()
		return
	}
	onlyNL := strings.Trim(s, "\n") == ""
	u.blank = strings.HasSuffix(s, "\n\n") || onlyNL && (u.endsNL || len(s) > 1)
	u.endsNL = strings.HasSuffix(s, "\n")
	u.wrote = true
	u.mu.Unlock()
	if lr != nil && lr.print(strings.TrimSuffix(s, "\n")) {
		return
	}
	u.mu.Lock()
	defer u.mu.Unlock()
	_, _ = io.WriteString(u.errw, s)
}

func linePrefix(word string) string {
	return word + strings.Repeat(" ", max(1, 6-len(word)))
}

func (u *UI) glyphLine(st State, word, msg, fix string) {
	var b strings.Builder
	if u.rich() {
		th := u.themed()
		glyph := u.glyphs.cell(u.glyphs.state(st))
		pad := strings.Repeat(" ", ansi.StringWidth(glyph)+1)
		w := min(u.Width(), maxContentCol) - len(pad)
		for i, l := range wrapAll([]string{printable(msg)}, w) {
			if i == 0 {
				b.WriteString(th.state(st).Render(glyph) + " " + l + "\n")
				continue
			}
			b.WriteString(pad + l + "\n")
		}
		for _, l := range wrapAll([]string{printable(fix)}, w) {
			if fix != "" {
				b.WriteString(pad + l + "\n")
			}
		}
	} else {
		th := u.theme
		b.WriteString(th.state(st).Render(linePrefix(word)) + printable(msg) + "\n")
		if fix != "" {
			b.WriteString(strings.Repeat(" ", 6) + printable(fix) + "\n")
		}
	}
	u.emit(b.String())
}

func (u *UI) OK(format string, args ...any) {
	if u.opts.Quiet || u.opts.JSON {
		return
	}
	msg := fmt.Sprintf(format, args...)
	if u.tier == TierJSON {
		u.jsonRep.event("info", "message", printable(msg))
		return
	}
	u.glyphLine(StateOK, "ok", msg, "")
}

func (u *UI) Warn(msg, fix string) {
	fix, _ = u.verboseFix(nil, fix)
	if u.tier == TierJSON {
		u.jsonRep.event("warn", "message", printable(msg), "fix", printable(fix))
		return
	}
	u.glyphLine(StateWarn, "warn", msg, fix)
}

func (u *UI) Fail(msg, fix string) {
	if u.tier == TierJSON {
		u.jsonFail(msg, fix, "")
		return
	}
	u.glyphLine(StateFail, "fail", msg, fix)
}

func (u *UI) jsonFail(msg, fix, cause string) {
	u.jsonRep.event("step.fail", "id", "", "message", printable(msg), "fix", printable(fix), "cause", printable(cause), "log_path", "")
}

func (u *UI) Info(format string, args ...any) {
	if u.opts.Quiet || u.opts.JSON {
		return
	}
	msg := fmt.Sprintf(format, args...)
	if u.tier == TierJSON {
		u.jsonRep.event("info", "message", printable(msg))
		return
	}
	msg = printable(msg)
	if u.rich() {
		msg = wrapKeepHyphens(msg, min(u.Width(), maxContentCol))
	}
	u.emit(msg + "\n")
}

func (u *UI) Detail(format string, args ...any) {
	if !u.opts.Verbose || u.tier == TierJSON {
		return
	}
	u.emit(u.themed().Muted.Render(printable(fmt.Sprintf(format, args...))) + "\n")
}

func (u *UI) Result(s string) {
	fmt.Fprintln(u.opts.Stdout, s)
}

func (u *UI) JSON(v any) error {
	enc := json.NewEncoder(u.opts.Stdout)
	enc.SetIndent("", "  ")
	enc.SetEscapeHTML(false)
	return enc.Encode(v)
}

func (u *UI) ReportError(err error) {
	if err == nil {
		return
	}
	var ee *ExitError
	if !errors.As(err, &ee) {
		ee = &ExitError{Code: ExitFailed, Msg: Sentence(err.Error())}
	}
	msg := exitMessage(ee)
	if ee.Code == ExitInterrupted && msg == "" && ee.asked && u.tier != TierJSON {
		u.mu.Lock()
		changed := u.changed
		u.mu.Unlock()
		if !changed {
			u.Info("Nothing changed.")
		}
		return
	}
	if ee.Code == ExitOK || msg == "" {
		return
	}
	fix, verboseCause := u.verboseFix(ee, ee.Fix)
	if u.opts.JSON {
		u.jsonError(ee.Code, msg, ee.Fix)
	}
	u.mu.Lock()
	tail, logPath, stepTail := ee.Tail, ee.LogPath, false
	if len(tail) == 0 && u.failTail != nil {
		tail, stepTail = u.failTail, true
	}
	if logPath == "" {
		logPath = u.failLog
	}
	shown := slices.Contains(u.failShown, msg)
	u.mu.Unlock()
	cause := causeText(ee)
	switch {
	case shown && (u.tier == TierJSON || u.tier == TierLines):
	case u.tier == TierJSON:
		u.jsonFail(msg, fix, verboseCause)
	case ee.Code == ExitUsage:
		u.usageLines(msg, fix)
	case u.rich() && !u.opts.Quiet:
		if cause != "" {
			fix = dropVerboseAsk(fix)
		}
		u.failureCard(ee.Code, msg, fix, cause, tail, logPath)
	default:
		u.Fail(msg, fix)
		if cause != "" {
			u.Detail("%s", cause)
		}
		if !stepTail {
			u.failureTail(tail, logPath)
		}
	}
}

func (u *UI) jsonError(code int, msg, fix string) {
	doc := map[string]any{"error": map[string]any{"code": code, "message": printable(msg), "fix": printable(fix)}}
	_ = u.JSON(doc)
}

func (u *UI) verboseFix(err error, fix string) (string, string) {
	if !u.opts.Verbose {
		return fix, ""
	}
	var ee *ExitError
	cause := ""
	if errors.As(err, &ee) {
		cause = causeText(ee)
	}
	if u.tier == TierJSON && cause == "" {
		return fix, ""
	}
	return dropVerboseAsk(fix), cause
}

func dropVerboseAsk(fix string) string {
	var kept []string
	for _, s := range strings.SplitAfter(fix, ". ") {
		if !strings.Contains(s, "--verbose") {
			kept = append(kept, s)
		} else if _, then, ok := strings.Cut(s, " to see why, then "); ok {
			kept = append(kept, Sentence(then)+" ")
		}
	}
	return strings.TrimSpace(strings.Join(kept, ""))
}

func causeText(ee *ExitError) string {
	if ee.Msg == "" {
		return ""
	}
	for cause := ee.Err; cause != nil; {
		var inner *ExitError
		if !errors.As(cause, &inner) {
			return Sentence(cause.Error())
		}
		if inner.Err == nil {
			if inner.Msg == ee.Msg {
				return ""
			}
			return inner.Error()
		}
		cause = inner.Err
	}
	return ""
}

func (u *UI) usageLines(msg, fix string) {
	if u.rich() {
		lk := u.look()
		line := lk.th.Fail.Render(lk.g.Fail) + " " + Inline(msg) + "\n"
		if fix != "" {
			line += "  " + lk.th.Accent.Render(lk.g.Arrow) + " " + u.rewriteSelf(Inline(fix)) + "\n"
		}
		u.emit(line)
		return
	}
	var b strings.Builder
	b.WriteString(printable(msg) + "\n")
	if fix != "" {
		b.WriteString(printable(fix) + "\n")
	}
	u.emit(b.String())
}

func (u *UI) failureTail(tail []string, logPath string) {
	if u.tier == TierJSON || len(tail) == 0 && logPath == "" {
		return
	}
	var b strings.Builder
	for _, line := range tail {
		b.WriteString("    | " + printable(line) + "\n")
	}
	if logPath != "" {
		b.WriteString(linePrefix("") + "Full log " + logPath + "\n")
	}
	u.emit(u.theme.Muted.Render(strings.TrimSuffix(b.String(), "\n")) + "\n")
}

func (u *UI) Cmd(command string) string {
	return u.themed().Accent.Render(u.rewriteSelf(Inline(command)))
}

func (u *UI) Hyperlink(url, text string) string {
	if text == "" {
		text = url
	}
	if !u.colour || !u.stderrTTY || !u.rich() {
		return text
	}
	return ansi.SetHyperlink(url) + text + ansi.ResetHyperlink()
}

func (u *UI) Copy(text string) bool {
	if !u.stderrTTY || !u.rich() {
		return false
	}
	u.mu.Lock()
	defer u.mu.Unlock()
	_, err := io.WriteString(u.opts.Stderr, ansi.SetSystemClipboard(text))
	return err == nil
}

func printable(s string) string {
	return strings.Map(func(r rune) rune {
		if r == '\n' || r == '\t' || !unicode.IsControl(r) {
			return r
		}
		return -1
	}, ansi.Strip(s))
}

func Inline(s string) string {
	return strings.Map(func(r rune) rune {
		if r == '\n' || r == '\t' {
			return ' '
		}
		return r
	}, printable(s))
}

func cells(row []string) []string {
	out := make([]string, len(row))
	for i, c := range row {
		out[i] = Inline(c)
	}
	return out
}

func Sentence(s string) string {
	s = strings.TrimSpace(s)
	if s == "" {
		return s
	}
	r := []rune(s)
	r[0] = unicode.ToUpper(r[0])
	s = string(r)
	if !strings.HasSuffix(s, ".") && !strings.HasSuffix(s, "?") && !strings.HasSuffix(s, "!") {
		s += "."
	}
	return s
}

func JoinAnd(items []string) string { return joinLast(items, " and ") }

func JoinOr(items []string) string { return joinLast(items, " or ") }

func joinLast(items []string, last string) string {
	if len(items) < 2 {
		return strings.Join(items, "")
	}
	return strings.Join(items[:len(items)-1], ", ") + last + items[len(items)-1]
}

func termSize(fd uintptr) (int, int, error) {
	return term.GetSize(int(fd))
}
