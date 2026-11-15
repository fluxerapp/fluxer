// SPDX-License-Identifier: AGPL-3.0-or-later

package main

import (
	"errors"
	"fmt"
	"net"
	"path/filepath"
	"slices"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

const rexctlRuntimeRoot = "/run/rexctl"

var errOtherInstance = errors.New("another Fluxer instance runs on this machine")

func otherInstance(l spec.Layout, specs []spec.Child) error {
	own := l.ControlSocket()
	sockets, _ := filepath.Glob(filepath.Join(rexctlRuntimeRoot, "*", "fluxerd.sock"))
	sockets = append(sockets, spec.Layout{Runtime: spec.DefaultRuntimeDir}.ControlSocket())
	for _, sock := range sockets {
		if sock == own {
			continue
		}
		if c, err := net.DialTimeout("unix", sock, time.Second); err == nil {
			c.Close()
			return fmt.Errorf("%w: the fluxerd behind %s answers. Stop it first, with systemctl stop fluxerd for an install.sh install or rexctl stop in its folder for a rexctl one", errOtherInstance, sock)
		}
	}
	ports := []int{spec.PortNATS}
	if slices.ContainsFunc(specs, func(c spec.Child) bool { return c.Name == spec.Postgres }) {
		ports = append(ports, spec.PortPostgres)
	}
	for _, port := range ports {
		ln, err := net.Listen("tcp", spec.Addr(port))
		if err != nil {
			return fmt.Errorf("%w or another program holds %s: %v. Stop it first", errOtherInstance, spec.Addr(port), err)
		}
		ln.Close()
	}
	return nil
}
