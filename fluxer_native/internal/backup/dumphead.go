// SPDX-License-Identifier: AGPL-3.0-or-later

package backup

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"strconv"
	"strings"
)

type DumpVersion struct {
	Server int
	PGDump int
}

func (d DumpVersion) Major() int { return max(d.Server, d.PGDump) }

const (
	maxHeaderString = 1 << 10
	headSize        = 8 << 10
)

type headBuffer struct{ buf []byte }

func (b *headBuffer) Write(p []byte) (int, error) {
	if room := headSize - len(b.buf); room > 0 {
		b.buf = append(b.buf, p[:min(room, len(p))]...)
	}
	return len(p), nil
}

func ReadDumpVersion(r io.Reader) (DumpVersion, error) {
	h := headReader{r: bufio.NewReader(r)}
	if magic := h.bytes(5); string(magic) != "PGDMP" {
		return DumpVersion{}, errors.New("backup: the database dump has no PGDMP header")
	}
	vmaj, vmin := h.byte(), h.byte()
	vrev := byte(0)
	if vmaj > 1 || vmaj == 1 && vmin > 0 {
		vrev = h.byte()
	}
	version := int(vmaj)<<16 | int(vmin)<<8 | int(vrev)
	if version < 1<<16|10<<8 {
		return DumpVersion{}, fmt.Errorf("backup: the database dump uses archive format %d.%d, which is older than any PostgreSQL Fluxer supports", vmaj, vmin)
	}
	h.intSize = int(h.byte())
	h.byte()
	h.byte()
	if version >= 1<<16|15<<8 {
		h.byte()
	} else {
		h.int()
	}
	for range 7 {
		h.int()
	}
	h.str()
	server, dump := h.str(), h.str()
	if h.err != nil {
		return DumpVersion{}, fmt.Errorf("backup: reading the database dump header: %w", h.err)
	}
	d := DumpVersion{Server: leadingMajor(server), PGDump: leadingMajor(dump)}
	if d.Major() == 0 {
		return d, fmt.Errorf("backup: the database dump header names no PostgreSQL version (%q, %q)", server, dump)
	}
	return d, nil
}

func leadingMajor(s string) int {
	end := strings.IndexFunc(s, func(r rune) bool { return r < '0' || r > '9' })
	if end >= 0 {
		s = s[:end]
	}
	n, _ := strconv.Atoi(s)
	return n
}

type headReader struct {
	r       *bufio.Reader
	intSize int
	err     error
}

func (h *headReader) bytes(n int) []byte {
	if h.err != nil {
		return nil
	}
	buf := make([]byte, n)
	_, h.err = io.ReadFull(h.r, buf)
	return buf
}

func (h *headReader) byte() byte {
	b := h.bytes(1)
	if h.err != nil {
		return 0
	}
	return b[0]
}

func (h *headReader) int() int {
	if h.intSize <= 0 || h.intSize > 8 {
		if h.err == nil {
			h.err = fmt.Errorf("integer size %d", h.intSize)
		}
		return 0
	}
	sign := h.byte()
	n := 0
	for i, b := range h.bytes(h.intSize) {
		n |= int(b) << (8 * i)
	}
	if sign != 0 {
		n = -n
	}
	return n
}

func (h *headReader) str() string {
	n := h.int()
	if n <= 0 || h.err != nil {
		return ""
	}
	if n > maxHeaderString {
		h.err = fmt.Errorf("a string of %d bytes", n)
		return ""
	}
	return string(h.bytes(n))
}

func checkDumpMajor(dump DumpVersion, target int) error {
	if target == 0 || dump.Major() <= target {
		return nil
	}
	return fmt.Errorf("backup: this backup is from PostgreSQL %d and this machine has %d. Install postgresql-%d from apt.postgresql.org, or restore on a machine with PostgreSQL %d or later",
		dump.Major(), target, dump.Major(), dump.Major())
}
