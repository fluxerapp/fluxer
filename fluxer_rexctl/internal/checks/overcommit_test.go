// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"errors"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
)

func stubOvercommit(t *testing.T, state host.OvercommitState, enable func(context.Context) error) {
	t.Helper()
	prevState, prevEnable := overcommit, enableOvercommit
	t.Cleanup(func() { overcommit, enableOvercommit = prevState, prevEnable })
	overcommit = func() host.OvercommitState { return state }
	enableOvercommit = enable
}

func TestOvercommitInAContainerGivesTheHostCommand(t *testing.T) {
	stubOvercommit(t, host.OvercommitLocked, func(context.Context) error {
		t.Fatal("offered a fix that can't work")
		return nil
	})
	c, _ := Get("memory-overcommit")
	r := RunOne(context.Background(), &Env{prepared: true}, c)
	if r.Status != Warn || r.Fixable {
		t.Fatalf("result = %+v, want a warning without a fix", r)
	}
	if want := "This machine is a container and can't change it. Run this on the container host: " + host.OvercommitCommand; r.Fix != want {
		t.Fatalf("fix = %q, want %q", r.Fix, want)
	}
}

func TestOvercommitFixThatChangesNothingFails(t *testing.T) {
	stubOvercommit(t, host.OvercommitOff, func(context.Context) error {
		return host.ErrOvercommitUnchanged
	})
	c, _ := Get("memory-overcommit")
	r := RunOne(context.Background(), &Env{prepared: true}, c)
	if !r.Fixable || !strings.Contains(r.Manual, host.OvercommitCommand) {
		t.Fatalf("result = %+v", r)
	}
	if _, err := ApplyFix(context.Background(), &Env{prepared: true}, "memory-overcommit"); !errors.Is(err, host.ErrOvercommitUnchanged) {
		t.Fatalf("ApplyFix error = %v, want %v", err, host.ErrOvercommitUnchanged)
	}
}
