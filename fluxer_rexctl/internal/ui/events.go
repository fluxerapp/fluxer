// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"time"

	tea "charm.land/bubbletea/v2"
)

type reporter interface {
	Send(msg tea.Msg)
}

type stepPlanned struct {
	id, title string
}

type stepStarted struct {
	id, parent, title string
	stage             [2]int
	at                time.Time
}

type stepTitle struct {
	id, title string
}

type stepWait struct {
	id, title, waitFor string
	since, at          time.Time
}

type stepProgress struct {
	id               string
	cur, total, rate int64
	share            bool
	at               time.Time
}

type stepItem struct {
	id, item, label, detail string
	state                   State
	cur, total              int64
	at                      time.Time
}

type stepLog struct {
	id, line string
}

type stepDone struct {
	id, summary string
	state       State
	at, started time.Time
}

type stepFailed struct {
	id, msg, fix, logPath string
	cause                 string
	tail                  []string
	at, started           time.Time
	warn                  bool
}

type stepSkipped struct {
	id, title string
}

type stepWarn struct {
	id, msg, fix string
}

type stageMsg struct {
	n, of     int
	id, title string
}

type allDone struct{}

type tickMsg struct{ gen int }

type springMsg struct{}

type rexFrameMsg struct{ gen int }
