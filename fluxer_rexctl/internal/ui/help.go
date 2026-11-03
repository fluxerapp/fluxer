// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"bytes"
	"io"
	"os"
	"regexp"
	"strings"

	"github.com/charmbracelet/x/ansi"
	"golang.org/x/term"
)

type HelpWriter struct {
	f      *os.File
	titles map[string]string
}

func NewHelpWriter(f *os.File, titles ...string) *HelpWriter {
	w := &HelpWriter{f: f, titles: map[string]string{}}
	for _, t := range append([]string{"Usage", "Examples", "Commands", "Flags"}, titles...) {
		w.titles[strings.ToUpper(t)] = t
	}
	return w
}

func (w *HelpWriter) Fd() uintptr                       { return w.f.Fd() }
func (w *HelpWriter) Read(p []byte) (int, error)        { return w.f.Read(p) }
func (w *HelpWriter) Close() error                      { return w.f.Close() }
func (w *HelpWriter) WriteString(s string) (int, error) { return w.Write([]byte(s)) }

var trailingSpace = regexp.MustCompile(`[ \t]+(\x1b\[[0-9;]*m)*(\r?\n|$)`)

func (w *HelpWriter) trim(p []byte) []byte {
	if isTerminal(w.f) && colourAllowed() {
		return p
	}
	if !colourAllowed() {
		p = []byte(ansi.Strip(string(p)))
	}
	return trailingSpace.ReplaceAll(p, []byte("$1$2"))
}

var helpItem = regexp.MustCompile(`^( {2,}\S.*?\S?)( {2,})\S`)

func (w *HelpWriter) width() int {
	if !isTerminal(w.f) {
		return 0
	}
	cols, _, err := term.GetSize(int(w.f.Fd()))
	if err != nil {
		return 0
	}
	return min(cols, 120)
}

func (w *HelpWriter) wrap(p []byte) []byte {
	limit := w.width()
	line, ok := strings.CutSuffix(string(p), "\n")
	if limit == 0 || !ok || strings.Contains(line, "\n") || ansi.StringWidth(line) <= limit {
		return p
	}
	m := helpItem.FindStringSubmatchIndex(ansi.Strip(line))
	if m == nil {
		return p
	}
	plain := ansi.Strip(line)
	key := ansi.StringWidth(plain[:m[3]])
	col := ansi.StringWidth(plain[:m[5]])
	total := ansi.StringWidth(line)
	head := ansi.Cut(line, 0, key)
	help := ansi.Cut(line, col, total)
	if limit-col < 24 {
		indent := strings.Repeat(" ", 6)
		body := ansi.Wordwrap(help, limit-len(indent), "")
		return []byte(head + "\n" + indent + strings.ReplaceAll(body, "\n", "\n"+indent) + "\n")
	}
	indent := strings.Repeat(" ", col)
	body := ansi.Wordwrap(help, limit-col, "")
	return []byte(ansi.Cut(line, 0, col) + strings.ReplaceAll(body, "\n", "\n"+indent) + "\n")
}

func (w *HelpWriter) Write(p []byte) (int, error) {
	shout := strings.TrimSpace(ansi.Strip(string(p)))
	title, ok := w.titles[shout]
	if !ok {
		if _, err := w.f.Write(w.trim(w.wrap(p))); err != nil {
			return 0, err
		}
		return len(p), nil
	}
	if _, err := w.f.Write(w.trim(bytes.Replace(p, []byte(shout), []byte(title), 1))); err != nil {
		return 0, err
	}
	return len(p), nil
}

var _ io.ReadWriteCloser = (*HelpWriter)(nil)
