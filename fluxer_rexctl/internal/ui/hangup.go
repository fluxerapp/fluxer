// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"context"
	"errors"
	"os"
	"os/signal"
	"syscall"
)

var errHangup = errors.New("ui: the terminal went away")

type hangupKey struct{}

func SignalContext() (context.Context, context.CancelFunc) {
	quit, stopQuit := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	ctx, cancel := context.WithCancelCause(context.WithValue(quit, hangupKey{}, quit))
	hup := make(chan os.Signal, 1)
	signal.Notify(hup, syscall.SIGHUP)
	go func() {
		select {
		case <-hup:
			signal.Ignore(syscall.SIGPIPE)
			cancel(errHangup)
		case <-ctx.Done():
		}
	}()
	return ctx, func() {
		signal.Stop(hup)
		cancel(nil)
		stopQuit()
	}
}

func OutliveHangup(ctx context.Context) (context.Context, context.CancelFunc) {
	quit, ok := ctx.Value(hangupKey{}).(context.Context)
	if !ok {
		return context.WithCancel(ctx)
	}
	return context.WithCancel(quit)
}
