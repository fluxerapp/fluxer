// SPDX-License-Identifier: AGPL-3.0-or-later

package notify

import (
	"context"
	"net"
	"os"
	"strconv"
	"syscall"
	"time"
)

const (
	systemdVar  = "NOTIFY_SOCKET"
	ownVar      = "FLUXERD_NOTIFY_SOCKET"
	watchdogVar = "WATCHDOG_USEC"
)

func Detach() error {
	path, ok := os.LookupEnv(systemdVar)
	if !ok {
		return nil
	}
	self, err := os.Executable()
	if err != nil {
		return err
	}
	if err := os.Unsetenv(systemdVar); err != nil {
		return err
	}
	if err := os.Setenv(ownVar, path); err != nil {
		return err
	}
	return syscall.Exec(self, os.Args, os.Environ())
}

func Ready() error { return send("READY=1") }

const watchdogEvery = 2 * time.Minute

func Watchdog(ctx context.Context, check func()) {
	if os.Getenv(ownVar) == "" {
		return
	}
	every := watchdogEvery
	if usec, err := strconv.ParseInt(os.Getenv(watchdogVar), 10, 64); err == nil && usec > 0 {
		every = time.Duration(usec) * time.Microsecond
	} else if err := send("WATCHDOG=1\nWATCHDOG_USEC=" + strconv.FormatInt(every.Microseconds(), 10)); err != nil {
		return
	}
	t := time.NewTicker(every / 2)
	defer t.Stop()
	for {
		select {
		case <-ctx.Done():
			return
		case <-t.C:
			check()
			_ = send("WATCHDOG=1")
		}
	}
}

func Stopping() error { return send("STOPPING=1\nSTATUS=stopping") }

func Status(line string) error { return send("STATUS=" + line) }

func Extend(d time.Duration) error {
	return send("EXTEND_TIMEOUT_USEC=" + strconv.FormatInt(d.Microseconds(), 10))
}

func send(state string) error {
	path := os.Getenv(ownVar)
	if path == "" {
		return nil
	}
	conn, err := net.DialUnix("unixgram", nil, &net.UnixAddr{Name: path, Net: "unixgram"})
	if err != nil {
		return err
	}
	defer conn.Close()
	_, err = conn.Write([]byte(state))
	return err
}
