// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"errors"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
)

var ErrEjected = errors.New("instance: managed by hand after rexctl eject")

type EjectedError struct {
	At   time.Time
	Tool string
	Dir  string
}

func (e *EjectedError) Error() string {
	return "You manage this instance yourself since " + day(e.At) + "."
}

func (e *EjectedError) Fix() string {
	return "Run rexctl takeover to let rexctl manage it again."
}

func (e *EjectedError) Start() string {
	return "Run " + e.Tool + " up -d in " + e.Dir + "."
}

func (e *EjectedError) Unwrap() error { return ErrEjected }

func ComposeTool(kind string) string {
	if kind == string(engine.Podman) {
		return "podman compose"
	}
	return "docker compose"
}

func (i *Instance) Managed() error {
	if i == nil || i.State == nil || i.State.Ejected == nil {
		return nil
	}
	return &EjectedError{At: i.State.Ejected.At, Tool: ComposeTool(i.State.Engine.Kind), Dir: i.Dir}
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
