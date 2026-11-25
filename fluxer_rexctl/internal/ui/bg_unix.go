// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build linux || darwin

package ui

import (
	"os"
	"time"

	"golang.org/x/sys/unix"
	"golang.org/x/term"
)

func queryBackground(in, out *os.File, timeout time.Duration) (string, bool) {
	fd := int(in.Fd())
	state, err := term.MakeRaw(fd)
	if err != nil {
		return "", false
	}
	defer func() { _ = term.Restore(fd, state) }()
	if _, err := out.WriteString("\x1b]11;?\x07\x1b[c"); err != nil {
		return "", false
	}
	deadline := time.Now().Add(timeout)
	grace := deadline.Add(lateReplyGrace)
	var buf []byte
	chunk := make([]byte, 256)
	for {
		limit := deadline
		if time.Now().After(deadline) {
			limit = grace
		}
		left := time.Until(limit)
		if left <= 0 {
			flushInput(fd)
			return parseBackgroundReply(buf)
		}
		fds := []unix.PollFd{{Fd: int32(fd), Events: unix.POLLIN}}
		n, err := unix.Poll(fds, int(left.Milliseconds())+1)
		if err == unix.EINTR {
			continue
		}
		if err != nil {
			flushInput(fd)
			return parseBackgroundReply(buf)
		}
		if n == 0 {
			continue
		}
		r, err := unix.Read(fd, chunk)
		if err != nil || r <= 0 {
			flushInput(fd)
			return parseBackgroundReply(buf)
		}
		buf = append(buf, chunk[:r]...)
		if da1Seen(buf) {
			return parseBackgroundReply(buf)
		}
	}
}
