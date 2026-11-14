// SPDX-License-Identifier: AGPL-3.0-or-later

package logs

import "sync"

const RingLines = 2000

const followBuffer = 1024

type Ring struct {
	mu    sync.Mutex
	lines []string
	next  int
	full  bool
	subs  map[chan string]struct{}
}

func NewRing(size int) *Ring {
	return &Ring{lines: make([]string, size), subs: map[chan string]struct{}{}}
}

func (r *Ring) Add(line string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.lines[r.next] = line
	r.next = (r.next + 1) % len(r.lines)
	if r.next == 0 {
		r.full = true
	}
	for ch := range r.subs {
		select {
		case ch <- line:
		default:
		}
	}
}

func (r *Ring) Lines() []string {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.snapshot()
}

func (r *Ring) Follow() ([]string, <-chan string, func()) {
	r.mu.Lock()
	defer r.mu.Unlock()
	ch := make(chan string, followBuffer)
	r.subs[ch] = struct{}{}
	cancel := func() {
		r.mu.Lock()
		delete(r.subs, ch)
		r.mu.Unlock()
	}
	return r.snapshot(), ch, cancel
}

func (r *Ring) snapshot() []string {
	if !r.full {
		return append([]string(nil), r.lines[:r.next]...)
	}
	out := make([]string, 0, len(r.lines))
	out = append(out, r.lines[r.next:]...)
	return append(out, r.lines[:r.next]...)
}
