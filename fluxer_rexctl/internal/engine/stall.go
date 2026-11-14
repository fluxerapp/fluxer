// SPDX-License-Identifier: AGPL-3.0-or-later

package engine

import (
	"bytes"
	"context"
	"errors"
	"io"
	"sync"
	"time"
)

const (
	helperStallLimit  = 3 * time.Minute
	helperSilentLimit = 45 * time.Second
	heartbeatLine     = "rexctl-alive"
	heartbeatScript   = `(while sleep 10; do echo ` + heartbeatLine + ` >&2; done) & alive=$!
"$@"
s=$?
kill $alive 2>/dev/null
exit $s`
)

var ErrHelperStalled = errors.New("rexctl stopped the helper container after the engine stopped sending its output")

type activity struct {
	mu    sync.Mutex
	start time.Time
	last  time.Time
	busy  int
}

func (a *activity) writer(w io.Writer) io.Writer { return activityWriter{a, w} }

type activityWriter struct {
	a *activity
	w io.Writer
}

func (w activityWriter) Write(b []byte) (int, error) {
	w.a.mu.Lock()
	w.a.busy++
	w.a.mu.Unlock()
	n, err := w.w.Write(b)
	w.a.mu.Lock()
	w.a.busy--
	w.a.last = time.Now()
	w.a.mu.Unlock()
	return n, err
}

func (a *activity) stalled(limit time.Duration) bool {
	a.mu.Lock()
	defer a.mu.Unlock()
	return a.busy == 0 && time.Since(a.last) > limit
}

func (a *activity) silent(limit time.Duration) bool {
	a.mu.Lock()
	defer a.mu.Unlock()
	return a.busy == 0 && a.last.Equal(a.start) && time.Since(a.start) > limit
}

func (a *activity) watch(ctx context.Context, cancel context.CancelCauseFunc) {
	t := time.NewTicker(10 * time.Second)
	defer t.Stop()
	for {
		select {
		case <-ctx.Done():
			return
		case <-t.C:
			if a.stalled(helperStallLimit) || a.silent(helperSilentLimit) {
				cancel(ErrHelperStalled)
				return
			}
		}
	}
}

type heartbeatFilter struct {
	w       io.Writer
	partial []byte
}

func (f *heartbeatFilter) Write(b []byte) (int, error) {
	f.partial = append(f.partial, b...)
	for {
		i := bytes.IndexByte(f.partial, '\n')
		if i < 0 {
			return len(b), nil
		}
		line := f.partial[:i+1]
		if !bytes.Equal(bytes.TrimSpace(line), []byte(heartbeatLine)) {
			if _, err := f.w.Write(line); err != nil {
				return len(b), err
			}
		}
		f.partial = f.partial[i+1:]
	}
}

func (f *heartbeatFilter) flush() {
	if len(f.partial) > 0 {
		_, _ = f.w.Write(f.partial)
		f.partial = nil
	}
}
