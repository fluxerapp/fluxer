// SPDX-License-Identifier: AGPL-3.0-or-later

package main

import (
	"errors"
	"flag"
	"io"
	"net"
	"net/http"
	"os"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

func cmdDial(args []string) error {
	fs := flag.NewFlagSet("dial", flag.ContinueOnError)
	runtimeDir := socketFlag(fs)
	if err := fs.Parse(args); err != nil {
		return exitError(2)
	}
	l := spec.Layout{Runtime: *runtimeDir}
	targets := map[string][2]string{
		"control":  {"unix", l.ControlSocket()},
		"operator": {"unix", l.OperatorSocket()},
		"api":      {"tcp", spec.Addr(spec.PortAPI)},
	}
	target, ok := targets[fs.Arg(0)]
	if fs.NArg() != 1 || !ok {
		return errors.New("usage: fluxerd dial control|operator|api")
	}
	conn, err := net.Dial(target[0], target[1])
	if err != nil {
		return err
	}
	return pipe(conn, os.Stdin, os.Stdout)
}

func pipe(conn net.Conn, in io.Reader, out io.Writer) error {
	defer conn.Close()
	go io.Copy(conn, in)
	_, err := io.Copy(out, conn)
	return err
}

func controlServer(h http.Handler) *http.Server {
	srv := &http.Server{Handler: h}
	srv.SetKeepAlivesEnabled(false)
	return srv
}
