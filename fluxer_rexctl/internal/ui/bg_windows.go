// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"os"
	"time"

	xwindows "github.com/charmbracelet/x/windows"
	"golang.org/x/sys/windows"
)

func queryBackground(in, out *os.File, timeout time.Duration) (string, bool) {
	h := windows.Handle(in.Fd())
	var mode uint32
	if err := windows.GetConsoleMode(h, &mode); err != nil {
		return "", false
	}
	raw := mode&^(windows.ENABLE_ECHO_INPUT|windows.ENABLE_LINE_INPUT|windows.ENABLE_PROCESSED_INPUT) | windows.ENABLE_VIRTUAL_TERMINAL_INPUT
	if err := windows.SetConsoleMode(h, raw); err != nil {
		return "", false
	}
	defer func() { _ = windows.SetConsoleMode(h, mode) }()
	if _, err := out.WriteString("\x1b]11;?\x07\x1b[c"); err != nil {
		return "", false
	}
	deadline := time.Now().Add(timeout)
	grace := deadline.Add(lateReplyGrace)
	var buf []byte
	var recs [64]xwindows.InputRecord
	for {
		limit := deadline
		if time.Now().After(deadline) {
			limit = grace
		}
		left := time.Until(limit)
		if left <= 0 {
			break
		}
		ev, err := windows.WaitForSingleObject(h, uint32(left.Milliseconds())+1)
		if err != nil {
			break
		}
		if ev == uint32(windows.WAIT_TIMEOUT) {
			continue
		}
		if ev != windows.WAIT_OBJECT_0 {
			break
		}
		var n uint32
		if err := xwindows.ReadConsoleInput(h, &recs[0], uint32(len(recs)), &n); err != nil {
			break
		}
		for _, r := range recs[:n] {
			if r.EventType != xwindows.KEY_EVENT {
				continue
			}
			if k := r.KeyEvent(); k.KeyDown && k.Char != 0 {
				buf = append(buf, string(k.Char)...)
			}
		}
		if da1Seen(buf) {
			return parseBackgroundReply(buf)
		}
	}
	_ = windows.FlushConsoleInputBuffer(h)
	return parseBackgroundReply(buf)
}
