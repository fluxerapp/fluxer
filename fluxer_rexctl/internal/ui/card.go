// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"fmt"
	"net/url"
	"slices"
	"strings"
	"time"

	"github.com/charmbracelet/x/ansi"
)

type Tone int

const (
	ToneNeutral Tone = iota
	ToneAccent
	ToneOK
	ToneWarn
	ToneFail
)

type State int

const (
	StatePending State = iota
	StateActive
	StateOK
	StateWarn
	StateFail
	StateSkipped
)

func (s State) String() string {
	switch s {
	case StateActive:
		return "active"
	case StateOK:
		return "ok"
	case StateWarn:
		return "warn"
	case StateFail:
		return "fail"
	case StateSkipped:
		return "skipped"
	}
	return "pending"
}

type KV struct {
	Key   string
	Value string
	State State
	Hint  string
	Edit  rune
}

type CodeBlock struct {
	Title   string
	Lang    string
	Text    string
	SavedTo string
	Copy    bool
}

type Card struct {
	Title    string
	Tone     Tone
	Rows     []KV
	Body     []string
	Plan     []PlanItem
	Code     *CodeBlock
	QR       string
	Commands []string
	Footer   []string
}

type Success struct {
	Title     string
	URL       string
	Rows      []KV
	Next      []string
	Elapsed   time.Duration
	Celebrate bool
	QR        string
}

const (
	cardMax    = 76
	cardNarrow = 50
)

const RexPlanTitle = "Rex's plan"

func (u *UI) planHeader() string {
	if u.ShowsRex() {
		return "Rex will"
	}
	return "rexctl will"
}

func (u *UI) cardTitle(title string) string {
	if title == RexPlanTitle && !u.ShowsRex() {
		return "The plan"
	}
	return title
}

func (u *UI) silent() bool {
	return u.opts.Quiet || u.opts.JSON || u.tier == TierJSON
}

func (u *UI) cardWidth() int {
	return min(u.Width()-2, cardMax)
}

func box(lk look, title string, edge Tone, width int, body []string) []string {
	if width < cardNarrow {
		var out []string
		if title != "" {
			for _, l := range wrapAll([]string{title}, width-2) {
				out = append(out, "  "+lk.th.Bold.Render(l))
			}
		}
		for _, l := range wrapAll(body, width-2) {
			out = append(out, strings.TrimRight("  "+l, " "))
		}
		return out
	}
	b := lk.g.Border
	es := lk.th.Border
	if edge == ToneWarn || edge == ToneFail {
		es = lk.th.tone(edge)
	}
	inner := width - 5
	top := b.TopLeft + b.Top
	if title != "" {
		t := ansi.Truncate(title, inner-2, lk.g.Ellipsis)
		top = es.Render(top) + " " + t + " "
		top += es.Render(strings.Repeat(b.Top, max(0, width-ansi.StringWidth(top)-1)) + b.TopRight)
	} else {
		top = es.Render(top + strings.Repeat(b.Top, width-3) + b.TopRight)
	}
	out := []string{top}
	for _, l := range body {
		l = ansi.Truncate(l, inner, lk.g.Ellipsis)
		out = append(out, es.Render(b.Left)+"  "+padRight(l, inner)+" "+es.Render(b.Right))
	}
	out = append(out, es.Render(b.BottomLeft+strings.Repeat(b.Bottom, width-2)+b.BottomRight))
	return out
}

func kvBlock(lk look, rows []KV, width int) []string {
	keyW, editW := 0, 0
	for _, r := range rows {
		keyW = max(keyW, ansi.StringWidth(Inline(r.Key)))
		if r.Edit != 0 {
			editW = 3
		}
	}
	var out []string
	for _, r := range rows {
		pre := ""
		if editW > 0 {
			pre = "   "
			if r.Edit != 0 {
				pre = lk.th.Accent.Bold(true).Render(string(r.Edit)) + "  "
			}
		}
		key := Inline(r.Key)
		pre += lk.th.Muted.Render(key) + strings.Repeat(" ", keyW-ansi.StringWidth(key)+3)
		if r.State != StatePending {
			pre += lk.th.state(r.State).Render(lk.g.cell(lk.g.state(r.State))) + " "
		}
		indent := ansi.StringWidth(pre)
		value := Inline(r.Value)
		if r.Hint != "" {
			value += "  " + lk.th.Muted.Render(Inline(r.Hint))
		}
		room := width - indent
		if room < 12 || ansi.StringWidth(value) <= room {
			out = append(out, strings.TrimRight(pre+value, " "))
			continue
		}
		if head := Inline(r.Value) + "  "; r.Hint != "" && ansi.StringWidth(head) <= room/2 {
			hang := ansi.StringWidth(head)
			parts := strings.Split(wrapKeepHyphens(Inline(r.Hint), room-hang), "\n")
			out = append(out, pre+head+lk.th.Muted.Render(parts[0]))
			for _, p := range parts[1:] {
				out = append(out, strings.Repeat(" ", indent+hang)+lk.th.Muted.Render(p))
			}
			continue
		}
		parts := strings.Split(wrapKeepHyphens(value, room), "\n")
		out = append(out, pre+parts[0])
		for _, p := range parts[1:] {
			out = append(out, strings.Repeat(" ", indent)+p)
		}
	}
	return out
}

func kvPlain(rows []KV) []string {
	var out []string
	for _, r := range rows {
		line := Inline(r.Key) + ": "
		if r.Key == "" {
			line = "  "
		}
		if r.State != StatePending {
			line += r.State.String() + ", "
		}
		line += strings.TrimRight(Inline(r.Value), " ")
		if r.Hint != "" {
			line += " (" + Inline(r.Hint) + ")"
		}
		out = append(out, line)
	}
	return out
}

func codeBlock(lk look, c CodeBlock, width int) []string {
	var out []string
	if c.Title != "" {
		out = append(out, lk.th.Bold.Render(Inline(c.Title)))
	}
	lines := strings.Split(strings.TrimRight(printable(c.Text), "\n"), "\n")
	w := 0
	for _, l := range lines {
		w = max(w, ansi.StringWidth(strings.ReplaceAll(l, "\t", "    ")))
	}
	w = min(w+2, width)
	for _, l := range lines {
		l = strings.ReplaceAll(l, "\t", "    ")
		l = ansi.Truncate(" "+l, w, lk.g.Ellipsis)
		if lk.th.plain {
			out = append(out, "  "+strings.TrimRight(l, " "))
			continue
		}
		out = append(out, lk.th.Surface.Render(padRight(l, w)))
	}
	if c.SavedTo != "" {
		out = append(out, lk.th.Muted.Render("Saved to "+c.SavedTo))
	}
	return out
}

func (u *UI) block(lines []string) {
	lines = trimBlank(lines)
	if len(lines) == 0 {
		return
	}
	u.emit(u.gap() + strings.Join(lines, "\n") + "\n\n")
}

func trimBlank(lines []string) []string {
	for len(lines) > 0 && strings.TrimSpace(ansi.Strip(lines[0])) == "" {
		lines = lines[1:]
	}
	for len(lines) > 0 && strings.TrimSpace(ansi.Strip(lines[len(lines)-1])) == "" {
		lines = lines[:len(lines)-1]
	}
	return lines
}

func (u *UI) Section(title string) {
	if u.silent() {
		return
	}
	if !u.rich() {
		u.emit("== " + Inline(title) + "\n")
		return
	}
	u.emit(u.gap() + u.themed().Bold.Render(Inline(title)) + "\n")
}

func (u *UI) KV(rows ...KV) {
	if u.silent() || len(rows) == 0 {
		return
	}
	if !u.rich() {
		u.emit(strings.Join(kvPlain(rows), "\n") + "\n")
		return
	}
	u.emit(strings.Join(kvBlock(u.look(), rows, u.Width()-2), "\n") + "\n")
}

func (u *UI) cardBody(lk look, c Card, inner int) []string {
	var body []string
	rows := kvBlock(lk, c.Rows, inner)
	if qr := u.qrRows(c.QR, lk); len(qr) > 0 && inner >= 40 {
		rowsW, qw := 0, ansi.StringWidth(qr[0])
		for _, r := range rows {
			rowsW = max(rowsW, ansi.StringWidth(r))
		}
		if rowsW+2+qw <= inner {
			rows = besideQR(rows, qr, inner)
		} else {
			rows = append(append(rows, ""), qr...)
		}
	}
	body = append(body, rows...)
	if len(c.Body) > 0 {
		if len(body) > 0 {
			body = append(body, "")
		}
		for _, b := range c.Body {
			body = append(body, strings.Split(wordwrap(Inline(b), inner), "\n")...)
		}
	}
	if len(c.Plan) > 0 {
		if len(body) > 0 {
			body = append(body, "")
		}
		body = append(body, lk.th.Bold.Render(u.planHeader()))
		body = append(body, planBlock(lk, c.Plan, inner)...)
	}
	if c.Code != nil {
		if len(body) > 0 {
			body = append(body, "")
		}
		body = append(body, codeBlock(lk, *c.Code, inner)...)
	}
	return body
}

func besideQR(rows, qr []string, inner int) []string {
	h := max(len(rows), len(qr))
	qw := 0
	for _, q := range qr {
		qw = max(qw, ansi.StringWidth(q))
	}
	left := inner - qw - 2
	out := make([]string, h)
	for i := range h {
		l := ""
		if i < len(rows) {
			l = ansi.Truncate(rows[i], left, "")
		}
		q := ""
		if i < len(qr) {
			q = qr[i]
		}
		out[i] = padRight(l, left+2) + q
	}
	return out
}

func (u *UI) cardLines(c Card) []string {
	if !u.rich() {
		return u.cardPlain(c)
	}
	lk := u.look()
	w := u.cardWidth()
	inner := w - 5
	if w < cardNarrow {
		inner = w - 2
	}
	title := Inline(u.cardTitle(c.Title))
	if title != "" {
		title = lk.th.Bold.Render(title)
		if c.Tone == ToneFail || c.Tone == ToneWarn {
			st := StateFail
			if c.Tone == ToneWarn {
				st = StateWarn
			}
			title = lk.th.state(st).Render(lk.g.state(st)) + " " + title
		}
	}
	out := box(lk, title, c.Tone, w, u.cardBody(lk, c, inner))
	for _, cmd := range c.Commands {
		for _, l := range commandLines(u.rewriteSelf(Inline(cmd)), w-4) {
			out = append(out, "  "+lk.th.Code.Render(l))
		}
	}
	for _, f := range c.Footer {
		for _, l := range wrapAll([]string{u.footerLine(lk, f)}, w-2) {
			out = append(out, "  "+l)
		}
	}
	return out
}

func commandLines(cmd string, width int) []string {
	if ansi.StringWidth(cmd) <= width || width < 20 {
		return []string{cmd}
	}
	var out []string
	line := ""
	for _, word := range strings.Fields(cmd) {
		switch {
		case line == "":
			line = word
		case ansi.StringWidth(line)+1+ansi.StringWidth(word)+2 <= width:
			line += " " + word
		default:
			out = append(out, line+" \\")
			line = "    " + word
		}
	}
	return append(out, line)
}

func (u *UI) footerLine(lk look, s string) string {
	return lk.th.Muted.Render(u.rewriteSelf(Inline(s)))
}

func (u *UI) cardPlain(c Card) []string {
	var out []string
	if c.Title != "" {
		out = append(out, "== "+Inline(u.cardTitle(c.Title)))
	}
	out = append(out, kvPlain(c.Rows)...)
	for _, b := range c.Body {
		out = append(out, Inline(b))
	}
	if len(c.Plan) > 0 {
		out = append(out, u.planHeader())
		out = append(out, planPlain(c.Plan)...)
	}
	if c.Code != nil {
		out = append(out, codePlain(*c.Code)...)
	}
	for _, f := range append(slices.Clone(c.Commands), c.Footer...) {
		out = append(out, u.rewriteSelf(Inline(f)))
	}
	return out
}

func codePlain(c CodeBlock) []string {
	var out []string
	if c.Title != "" {
		out = append(out, Inline(c.Title))
	}
	for _, l := range strings.Split(strings.TrimRight(printable(c.Text), "\n"), "\n") {
		out = append(out, strings.TrimRight("    "+l, " "))
	}
	if c.SavedTo != "" {
		out = append(out, "Saved to "+c.SavedTo)
	}
	return out
}

func (u *UI) Card(c Card) {
	if u.silent() {
		return
	}
	u.block(u.cardLines(c))
}

func (u *UI) Notice(t Tone, msg, fix string) {
	if u.tier == TierJSON {
		if t == ToneWarn || t == ToneFail {
			u.Warn(msg, fix)
			return
		}
		u.Info("%s", msg)
		return
	}
	if !u.rich() {
		word := "note"
		switch t {
		case ToneWarn:
			word = "warn"
		case ToneFail:
			word = "fail"
		case ToneOK:
			word = "ok"
		}
		if u.silent() && t != ToneWarn && t != ToneFail {
			return
		}
		var b strings.Builder
		b.WriteString(linePrefix(word) + printable(Inline(msg)) + "\n")
		if fix != "" {
			b.WriteString(linePrefix("") + printable(Inline(fix)) + "\n")
		}
		u.emit(b.String())
		return
	}
	if u.silent() && t != ToneWarn && t != ToneFail {
		return
	}
	lk := u.look()
	st := lk.th.tone(t)
	if t == ToneNeutral {
		st = lk.th.Border
	}
	glyph := ""
	switch t {
	case ToneOK:
		glyph = lk.th.OK.Render(lk.g.OK) + " "
	case ToneWarn:
		glyph = lk.th.Warn.Render(lk.g.Warn) + " "
	case ToneFail:
		glyph = lk.th.Fail.Render(lk.g.Fail) + " "
	}
	lines := []string{glyph + Inline(msg)}
	if fix != "" {
		lines = append(lines, strings.Repeat(" ", ansi.StringWidth(glyph))+lk.th.Muted.Render(u.rewriteSelf(Inline(fix))))
	}
	w := 0
	for _, l := range lines {
		w = max(w, ansi.StringWidth(l))
	}
	w = min(w, u.Width()-6)
	b := lk.g.Border
	out := []string{st.Render(b.TopLeft + strings.Repeat(b.Top, w+2) + b.TopRight)}
	for _, l := range lines {
		out = append(out, st.Render(b.Left)+" "+padRight(ansi.Truncate(l, w, lk.g.Ellipsis), w)+" "+st.Render(b.Right))
	}
	out = append(out, st.Render(b.BottomLeft+strings.Repeat(b.Bottom, w+2)+b.BottomRight))
	u.emit(u.gap() + strings.Join(out, "\n") + "\n")
}

func (u *UI) Code(c CodeBlock) {
	if u.silent() {
		return
	}
	if !u.rich() {
		u.emit(strings.Join(codePlain(c), "\n") + "\n")
		return
	}
	lk := u.look()
	lines := codeBlock(lk, c, min(u.Width()-2, maxContentCol))
	if c.Copy && u.interactive {
		lines = append(lines, keyHints(lk, [][2]string{{"c", "copy"}, {"enter", "continue"}}))
		u.block(lines)
		if u.waitKey("c") {
			if u.Copy(c.Text) {
				u.OK("Copied to your clipboard.")
			}
		}
		return
	}
	u.block(lines)
}

func (u *UI) Next(commands ...string) {
	if u.silent() || len(commands) == 0 {
		return
	}
	if !u.rich() {
		parts := make([]string, len(commands))
		for i, c := range commands {
			parts[i] = Inline(c)
		}
		u.emit(u.rewriteSelf("next: "+strings.Join(parts, ", ")) + "\n")
		return
	}
	u.emit(u.nextLine(commands) + "\n")
}

func (u *UI) nextLine(commands []string) string {
	lk := u.look()
	w := min(u.Width(), maxContentCol) - 2
	sep := gap
	line := "  " + lk.th.Muted.Render("Next") + " "
	indent := "       "
	var lines []string
	first := true
	for _, c := range commands {
		part := u.Cmd(c)
		switch {
		case first:
			line += part
		case ansi.StringWidth(line)+ansi.StringWidth(sep)+ansi.StringWidth(part) <= w:
			line += sep + part
		default:
			lines = append(lines, line)
			line = indent + part
		}
		first = false
	}
	return strings.Join(append(lines, line), "\n")
}

func (u *UI) Highlight(s string) {
	s = Inline(s)
	if !u.stdoutTTY || u.accessible {
		fmt.Fprintln(u.opts.Stdout, s)
		return
	}
	lk := u.look()
	if u.outw == u.opts.Stdout {
		lk.th = plainTheme()
	}
	w := min(max(ansi.StringWidth(s)+8, 30), u.Width()-2)
	if w < ansi.StringWidth(s)+6 {
		fmt.Fprintf(u.outw, "\n    %s\n", s)
		return
	}
	inner := w - 5
	pad := (inner - ansi.StringWidth(s)) / 2
	body := []string{"", strings.Repeat(" ", pad) + lk.th.Accent.Bold(true).Render(s), ""}
	fmt.Fprintln(u.outw)
	fmt.Fprintln(u.outw, strings.Join(box(lk, "", ToneAccent, w, body), "\n"))
}

func (u *UI) Table(headers []string, rows [][]string) {
	headers = cells(headers)
	clean := make([][]string, len(rows))
	for i, r := range rows {
		clean[i] = cells(r)
	}
	rows = clean
	widths := make([]int, len(headers))
	numeric := make([]bool, len(headers))
	for i, h := range headers {
		widths[i] = ansi.StringWidth(h)
		numeric[i] = len(rows) > 0
	}
	for _, r := range rows {
		for i, c := range r {
			if i < len(widths) {
				widths[i] = max(widths[i], ansi.StringWidth(c))
				if c != "" && !isNumber(c) {
					numeric[i] = false
				}
			}
		}
	}
	format := func(cs []string) string {
		parts := make([]string, len(cs))
		for i, c := range cs {
			if i >= len(widths) {
				parts[i] = c
				continue
			}
			pad := strings.Repeat(" ", widths[i]-ansi.StringWidth(c))
			if numeric[i] {
				parts[i] = pad + c
			} else {
				parts[i] = c + pad
			}
		}
		return strings.TrimRight(strings.Join(parts, "   "), " ")
	}
	total := 3 * (len(widths) - 1)
	for _, w := range widths {
		total += w
	}
	if u.accessible || (u.columns() > 0 && total > u.columns() && len(headers) > 2) {
		u.records(headers, rows)
		return
	}
	head := format(headers)
	if u.outw != u.opts.Stdout {
		head = u.themed().Muted.Render(head)
	}
	fmt.Fprintln(u.outw, head)
	for _, r := range rows {
		fmt.Fprintln(u.outw, format(r))
	}
}

func isNumber(s string) bool {
	s = strings.TrimSuffix(strings.TrimSpace(s), "%")
	if s == "" {
		return false
	}
	for _, r := range s {
		if (r < '0' || r > '9') && r != '.' && r != ',' && r != '-' {
			return false
		}
	}
	return true
}

func (u *UI) columns() int {
	if u.opts.Width > 0 {
		return u.opts.Width
	}
	if !isTerminal(u.opts.Stdout) {
		return 0
	}
	f := u.opts.Stdout.(interface{ Fd() uintptr })
	w, _, err := termSize(f.Fd())
	if err != nil {
		return 0
	}
	return w
}

func (u *UI) records(headers []string, rows [][]string) {
	w, key := u.columns(), 0
	for _, h := range headers {
		key = max(key, ansi.StringWidth(h)+2)
	}
	for i, r := range rows {
		if i > 0 {
			fmt.Fprintln(u.outw)
		}
		for j, h := range headers {
			if j >= len(r) || r[j] == "" {
				continue
			}
			if u.accessible || w < key+20 {
				fmt.Fprintf(u.outw, "%s: %s\n", h, r[j])
				continue
			}
			label := h + ":" + strings.Repeat(" ", key-ansi.StringWidth(h)-1)
			for k, l := range strings.Split(wordwrapKeepHyphens(r[j], w-key), "\n") {
				if k > 0 {
					label = strings.Repeat(" ", key)
				}
				fmt.Fprintln(u.outw, label+l)
			}
		}
	}
}

func hostOf(raw string) string {
	p, err := url.Parse(raw)
	if err != nil || p.Host == "" {
		return raw
	}
	return p.Host
}

func (u *UI) Success(s Success) {
	if u.tier == TierJSON {
		u.jsonRep.event("info", "message", printable(strings.TrimSpace(s.Title+" "+s.URL)))
		return
	}
	if u.silent() {
		if s.URL != "" {
			u.Highlight(s.URL)
		}
		return
	}
	title := cmpOr(s.Title, liveLine)
	if !u.rich() {
		line := strings.TrimSuffix(Inline(title), ".")
		if s.URL != "" {
			line += " at " + s.URL
		}
		u.emit(linePrefix("done") + line + "\n")
		if len(s.Rows) > 0 {
			u.emit(strings.Join(kvPlain(s.Rows), "\n") + "\n")
		}
		u.Next(s.Next...)
		return
	}
	lk := u.look()
	if u.ShowsRex() {
		u.successRex(lk, title, s.Elapsed, s.Celebrate)
	} else {
		head := lk.th.OK.Render(lk.g.OK) + " " + lk.th.Bold.Render(Inline(title))
		if s.Elapsed > 0 {
			head += "  " + lk.th.Muted.Render(Elapsed(s.Elapsed))
		}
		u.block([]string{head})
	}
	rows := s.Rows
	if s.URL != "" {
		rows = append([]KV{{Key: "Open", Value: u.Hyperlink(s.URL, s.URL)}}, rows...)
	}
	qr := s.QR
	if qr == "" {
		qr = s.URL
	}
	cardTitle := ""
	if s.URL != "" {
		cardTitle = hostOf(s.URL)
	}
	lines := u.cardLines(Card{Title: cardTitle, Tone: ToneOK, Rows: rows, QR: qr})
	if len(s.Next) > 0 {
		lines = append(lines, u.nextLine(s.Next))
	}
	u.block(lines)
}

func (u *UI) successRex(lk look, title string, elapsed time.Duration, celebrate bool) {
	side := lk.th.Bold.Render(Inline(title))
	if elapsed > 0 {
		side += "  " + lk.th.Muted.Render(Elapsed(elapsed))
	}
	mood := MoodIdle
	if celebrate {
		mood = MoodParty
	}
	if celebrate && u.tier == TierLive && u.liveCapable() && u.live == nil {
		u.emit(u.gap())
		if _, err := u.runProgram(newPartyModel(lk, side), true); err == nil {
			u.viewLeft()
			return
		}
	}
	r := newRexMotion(lk, mood)
	u.block(sayView(r, lk, side, -1, ToneOK))
}

func splitTitle(s string, room int) (string, string) {
	if ansi.StringWidth(s) <= room {
		return s, ""
	}
	i := strings.Index(s, ". ")
	if i < 0 {
		return s, ""
	}
	return s[:i+1], strings.TrimSpace(s[i+2:])
}

func (u *UI) failureCard(code int, msg, fix, cause string, tail []string, logPath string) {
	lk := u.look()
	tone, st := ToneFail, StateFail
	title := Inline(msg)
	var body []string
	if code == ExitInterrupted {
		tone, st = ToneWarn, StateWarn
		first, rest, _ := strings.Cut(title, ". ")
		switch {
		case title == "" || title == stoppedLine+".":
			title = stoppedLine
		case rest != "":
			title = strings.TrimSuffix(first, ".")
			body = append(body, rest)
		default:
			title = strings.TrimSuffix(title, ".")
		}
	} else {
		var rest string
		title, rest = splitTitle(title, u.cardWidth()-8)
		if rest != "" {
			body = append(body, rest)
		}
		if cause != "" {
			body = append(body, cause)
		}
	}
	if u.ShowsRex() && (code == ExitFailed || code == ExitInterrupted) {
		mood := MoodSad
		if tone == ToneWarn {
			mood = MoodWorried
		}
		r := newRexMotion(lk, mood)
		line := failureLine
		if tone == ToneWarn {
			line = "I stopped where you asked me to."
		}
		u.block(sayView(r, lk, line, -1, tone))
	}
	w := u.cardWidth()
	inner := w - 5
	if w < cardNarrow {
		inner = w - 2
	}
	var wrapped []string
	for _, b := range body {
		wrapped = append(wrapped, strings.Split(wordwrap(Inline(b), inner), "\n")...)
	}
	if len(tail) > 0 {
		if len(wrapped) > 0 {
			wrapped = append(wrapped, "")
		}
		for _, l := range tail {
			wrapped = append(wrapped, lk.th.Muted.Render(lk.g.Rail+" "+ansi.Truncate(Inline(l), inner-2, lk.g.Ellipsis)))
		}
	}
	if logPath != "" {
		if len(wrapped) > 0 {
			wrapped = append(wrapped, "")
		}
		wrapped = append(wrapped, kvBlock(lk, []KV{{Key: "Full log", Value: logPath}}, inner)...)
	}
	mark := lk.th.state(st).Render(lk.g.state(st)) + " "
	head := mark + lk.th.Bold.Render(title)
	var lines []string
	if len(wrapped) == 0 && w >= cardNarrow {
		hang := strings.Repeat(" ", ansi.StringWidth(mark))
		for i, l := range wrapAll([]string{title}, w-len(hang)) {
			if i == 0 {
				lines = append(lines, mark+lk.th.Bold.Render(l))
				continue
			}
			lines = append(lines, hang+lk.th.Bold.Render(l))
		}
	} else {
		lines = box(lk, head, tone, w, wrapped)
	}
	if fix != "" {
		arrow := lk.th.Accent.Render(lk.g.Arrow) + " "
		pad := strings.Repeat(" ", ansi.StringWidth(arrow))
		for i, l := range wrapAll([]string{u.rewriteSelf(Inline(fix))}, w-2-len(pad)) {
			if i == 0 {
				lines = append(lines, "  "+arrow+l)
				continue
			}
			lines = append(lines, "  "+pad+l)
		}
	}
	u.block(lines)
}
