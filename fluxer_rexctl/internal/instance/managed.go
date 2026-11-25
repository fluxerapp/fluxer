// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"errors"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
)

var errEjected = errors.New("instance: managed by hand after rexctl eject")

type EjectedError struct {
	At       time.Time
	Tool     string
	Dir      string
	Detached bool
}

func (e *EjectedError) Error() string {
	return "You manage this instance yourself since " + day(e.At) + "."
}

func (e *EjectedError) Fix() string {
	if e.Detached {
		return "It runs from compose.yaml in " + e.Dir + ". Run " + e.Tool + " ps there to check it."
	}
	return "Install the last rexctl that has rexctl takeover from https://github.com/fluxerapp/fluxer/releases, take the instance over with it, then run rexctl upgrade, which offers the move."
}

func (e *EjectedError) Start() string {
	return "Run " + e.Tool + " up -d in " + e.Dir + "."
}

func (e *EjectedError) Unwrap() error { return errEjected }

func ComposeTool(kind string) string {
	if kind == string(engine.Podman) {
		return "podman compose"
	}
	return "docker compose"
}

type MovedError struct {
	Dir       string
	To        string
	Release   string
	CleanedUp bool
}

func (e *MovedError) Error() string {
	if e.CleanedUp {
		return "Fluxer moved from " + e.Dir + " to " + e.To + " and runs there in the new layout. The old containers and their volumes are removed."
	}
	return "Fluxer moved from " + e.Dir + " to " + e.To + " and runs there in the new layout, so the old containers stay stopped."
}

func (i *Instance) Moved() *MovedError {
	if i == nil || i.State == nil || i.State.MovedTo == "" || gone(i.State.MovedTo) {
		return nil
	}
	moved := &MovedError{Dir: i.Dir, To: i.State.MovedTo}
	if to, err := Find(i.State.MovedTo); err == nil {
		moved.Release, moved.CleanedUp = to.State.Release, to.State.MovedFrom == nil
	}
	return moved
}

func (i *Instance) Managed() error {
	if moved := i.Moved(); moved != nil {
		return moved
	}
	if i == nil || i.State == nil || i.State.Ejected == nil {
		return nil
	}
	return &EjectedError{At: i.State.Ejected.At, Tool: ComposeTool(i.State.Engine.Kind), Dir: i.Dir, Detached: i.State.IsContainer()}
}

func day(t time.Time) string {
	if t.IsZero() {
		return "you ran rexctl eject"
	}
	t = t.Local()
	if t.Year() != time.Now().Year() {
		return t.Format("2 January 2006")
	}
	return t.Format("2 January")
}
