// SPDX-License-Identifier: AGPL-3.0-or-later

package engine

import (
	"context"
	"strings"
	"sync"

	"github.com/docker/compose/v5/pkg/api"
)

type bus struct {
	mu      sync.Mutex
	project string
	onEvent func(Event)
	last    map[string]Event
}

func (b *bus) listen(project string, onEvent func(Event)) func() {
	b.mu.Lock()
	b.project = project
	b.onEvent = onEvent
	b.last = map[string]Event{}
	b.mu.Unlock()
	return func() {
		b.mu.Lock()
		b.onEvent = nil
		b.last = nil
		b.mu.Unlock()
	}
}

func (b *bus) Start(context.Context, string) {}

func (b *bus) Done(string, bool) {}

func (b *bus) On(events ...api.Resource) {
	b.mu.Lock()
	defer b.mu.Unlock()
	for _, r := range events {
		ev := toEvent(b.project, r)
		if b.onEvent == nil {
			continue
		}
		if prev, ok := b.last[ev.ID]; ok && prev == ev {
			continue
		}
		b.last[ev.ID] = ev
		b.onEvent(ev)
	}
}

func toEvent(project string, r api.Resource) Event {
	kind, name, _ := strings.Cut(r.ID, " ")
	ev := Event{
		ID:      r.ID,
		Parent:  r.ParentID,
		Kind:    strings.ToLower(kind),
		Text:    r.Text,
		Status:  eventStatus(r.Status),
		Current: r.Current,
		Total:   r.Total,
	}
	if r.Status == api.Error && r.Details != "" {
		ev.Text = r.Details
	}
	if ev.Kind == "container" {
		ev.Service = serviceOf(project, name)
	}
	return ev
}

func serviceOf(project, container string) string {
	s := strings.TrimPrefix(container, "/")
	if project != "" {
		s = strings.TrimPrefix(s, project+"-")
	}
	if i := strings.LastIndexByte(s, '-'); i > 0 && isDigits(s[i+1:]) {
		s = s[:i]
	}
	return s
}

func isDigits(s string) bool {
	if s == "" {
		return false
	}
	for _, r := range s {
		if r < '0' || r > '9' {
			return false
		}
	}
	return true
}

func eventStatus(s api.EventStatus) EventStatus {
	switch s {
	case api.Done:
		return EventDone
	case api.Warning:
		return EventWarning
	case api.Error:
		return EventError
	}
	return EventWorking
}
