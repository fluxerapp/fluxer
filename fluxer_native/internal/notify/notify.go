// SPDX-License-Identifier: AGPL-3.0-or-later

package notify

import (
	"net"
	"os"
	"syscall"
)

const (
	systemdVar = "NOTIFY_SOCKET"
	ownVar     = "FLUXERD_NOTIFY_SOCKET"
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

func Stopping() error { return send("STOPPING=1") }

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
