// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"bytes"
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/charmbracelet/x/ansi"
)

const (
	backgroundTimeout = 150 * time.Millisecond
	lateReplyGrace    = 350 * time.Millisecond
)

func da1Seen(buf []byte) bool {
	i := bytes.Index(buf, []byte("\x1b[?"))
	return i >= 0 && bytes.IndexByte(buf[i:], 'c') > 0
}

func parseBackgroundReply(buf []byte) (string, bool) {
	i := bytes.Index(buf, []byte("]11;"))
	if i < 0 {
		return "", false
	}
	rest := buf[i+4:]
	end := bytes.IndexAny(rest, "\x07\x1b")
	if end < 0 {
		return "", false
	}
	c := ansi.XParseColor(string(rest[:end]))
	if c == nil {
		return "", false
	}
	r, g, b, _ := c.RGBA()
	lum := 0.2126*float64(r)/0xffff + 0.7152*float64(g)/0xffff + 0.0722*float64(b)/0xffff
	if lum < 0.5 {
		return "dark", true
	}
	return "light", true
}

func backgroundFromEnv(getenv func(string) string) (bool, bool) {
	v := getenv("COLORFGBG")
	if v == "" {
		return false, false
	}
	parts := strings.Split(v, ";")
	n, err := strconv.Atoi(parts[len(parts)-1])
	if err != nil {
		return false, false
	}
	return n < 7 || n == 8, true
}

func (u *UI) detectDark() bool {
	if dark, ok := backgroundFromEnv(u.opts.Getenv); ok {
		return dark
	}
	if u.opts.JSON || !u.stderrTTY || u.tier == TierLines || u.tier == TierJSON {
		return true
	}
	in, ok := u.opts.Stdin.(*os.File)
	if !ok || !isTerminal(in) {
		return true
	}
	out, ok := u.opts.Stderr.(*os.File)
	if !ok {
		return true
	}
	kind, ok := queryBackground(in, out, backgroundTimeout)
	return !ok || kind == "dark"
}
