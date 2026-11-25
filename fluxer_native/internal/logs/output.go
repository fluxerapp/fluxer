// SPDX-License-Identifier: AGPL-3.0-or-later

package logs

import (
	"bufio"
	"bytes"
	"encoding/binary"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net"
	"os"
	"strconv"
	"strings"
	"sync"
)

const (
	PrioErr     = 3
	PrioWarning = 4
	PrioInfo    = 6
)

const JournalSocket = "/run/systemd/journal/socket"

const MaxLine = 64 << 10

type Output struct {
	mu      sync.Mutex
	journal *net.UnixConn
	w       io.Writer
}

func New() *Output {
	o := &Output{w: os.Stdout}
	if !stdoutIsJournal(os.Getenv("JOURNAL_STREAM")) {
		return o
	}
	if conn, err := dialJournal(JournalSocket); err == nil {
		o.journal = conn
	}
	return o
}

func NewWriter(w io.Writer) *Output {
	return &Output{w: w}
}

func dialJournal(socket string) (*net.UnixConn, error) {
	return net.DialUnix("unixgram", nil, &net.UnixAddr{Name: socket, Net: "unixgram"})
}

func stdoutIsJournal(stream string) bool {
	devStr, inoStr, ok := strings.Cut(stream, ":")
	if !ok {
		return false
	}
	dev, err1 := strconv.ParseUint(devStr, 10, 64)
	ino, err2 := strconv.ParseUint(inoStr, 10, 64)
	if err1 != nil || err2 != nil {
		return false
	}
	gotDev, gotIno, ok := stdoutID()
	return ok && gotDev == dev && gotIno == ino
}

const (
	Stdout = "stdout"
	Stderr = "stderr"
)

func (o *Output) Child(svc, stream, text string) {
	o.write("fluxer-"+svc, svc, Priority(text), stream, text)
}

func (o *Output) Logf(prio int, format string, args ...any) {
	o.write("fluxerd", "fluxerd", prio, "", fmt.Sprintf(format, args...))
}

func (o *Output) write(ident, label string, prio int, stream, text string) {
	o.mu.Lock()
	defer o.mu.Unlock()
	if o.journal != nil {
		if _, err := o.journal.Write(journalEntry(ident, prio, stream, text)); err == nil {
			return
		}
	}
	fmt.Fprintf(o.w, "[%s] %s\n", label, text)
}

func journalEntry(ident string, prio int, stream, text string) []byte {
	var b bytes.Buffer
	journalField(&b, "MESSAGE", text)
	journalField(&b, "PRIORITY", strconv.Itoa(prio))
	journalField(&b, "SYSLOG_IDENTIFIER", ident)
	if stream != "" {
		journalField(&b, "FLUXER_STREAM", stream)
	}
	return b.Bytes()
}

var (
	errMarks  = []string{" ERROR ", "ERROR:", "[ERROR]", "[error]", "level=error", " FATAL ", "FATAL:", "[FATAL]", "PANIC:", "[critical]", "CRITICAL"}
	warnMarks = []string{" WARN ", "WARN:", "[WARN]", "[warn]", "[warning]", "level=warn", "WARNING"}
)

func Priority(text string) int {
	if strings.HasPrefix(text, "{") {
		var line struct {
			Level any `json:"level"`
		}
		if json.Unmarshal([]byte(text), &line) == nil {
			return levelPriority(line.Level)
		}
	}
	head := text[:min(len(text), 160)]
	for _, m := range errMarks {
		if strings.Contains(head, m) {
			return PrioErr
		}
	}
	for _, m := range warnMarks {
		if strings.Contains(head, m) {
			return PrioWarning
		}
	}
	return PrioInfo
}

func levelPriority(level any) int {
	switch v := level.(type) {
	case float64:
		switch {
		case v >= 50:
			return PrioErr
		case v >= 40:
			return PrioWarning
		}
	case string:
		switch strings.ToLower(v) {
		case "error", "err", "fatal", "critical", "panic", "dpanic":
			return PrioErr
		case "warn", "warning":
			return PrioWarning
		}
	}
	return PrioInfo
}

func journalField(b *bytes.Buffer, key, value string) {
	if !strings.Contains(value, "\n") {
		b.WriteString(key)
		b.WriteByte('=')
		b.WriteString(value)
		b.WriteByte('\n')
		return
	}
	b.WriteString(key)
	b.WriteByte('\n')
	var size [8]byte
	binary.LittleEndian.PutUint64(size[:], uint64(len(value)))
	b.Write(size[:])
	b.WriteString(value)
	b.WriteByte('\n')
}

func Pump(r io.Reader, line func(text string, continued bool)) {
	br := bufio.NewReaderSize(r, MaxLine)
	continued := false
	for {
		chunk, err := br.ReadSlice('\n')
		if len(chunk) > 0 {
			line(strings.TrimRight(string(chunk), "\r\n"), continued)
		}
		continued = errors.Is(err, bufio.ErrBufferFull)
		if err != nil && !continued {
			return
		}
	}
}

type Demux struct {
	Fallback string
	last     string
}

var rustCrates = map[string]string{
	"fluxer_snowflakes": "snowflakes",
	"fluxer_users":      "users",
	"fluxer_messages":   "messages",
	"fluxer_gifs":       "gifs",
	"fluxer_unfurl":     "unfurl",
	"fluxer_push":       "push",
	"fluxer_app_proxy":  "app-proxy",
	"fluxer_admin":      "admin",
}

func (d *Demux) Svc(text string, continued bool) string {
	if continued && d.last != "" {
		return d.last
	}
	d.last = d.svc(text)
	return d.last
}

func (d *Demux) svc(text string) string {
	if !strings.HasPrefix(text, "{") {
		return d.Fallback
	}
	var line struct {
		Spans []struct {
			Svc string `json:"svc"`
		} `json:"spans"`
		Target string `json:"target"`
	}
	if json.Unmarshal([]byte(text), &line) != nil {
		return d.Fallback
	}
	if len(line.Spans) > 0 && line.Spans[0].Svc != "" {
		if _, known := rustCrates["fluxer_"+strings.ReplaceAll(line.Spans[0].Svc, "-", "_")]; known {
			return line.Spans[0].Svc
		}
	}
	crate, _, _ := strings.Cut(line.Target, "::")
	if svc, ok := rustCrates[crate]; ok {
		return svc
	}
	return d.Fallback
}
