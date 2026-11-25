// SPDX-License-Identifier: AGPL-3.0-or-later

package main

import (
	"errors"
	"fmt"
	"maps"
	"net"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

var errOtherInstance = errors.New("another Fluxer instance runs on this machine")

func otherInstance(specs []spec.Child) ([]string, error) {
	owners := map[int]string{spec.PortNATS: "nats"}
	core := map[int]bool{spec.PortNATS: true}
	for _, c := range specs {
		for _, port := range spec.ChildPorts[c.Name] {
			owners[port] = c.Name
			core[port] = c.Core
		}
	}
	var busy, warnings []string
	for _, port := range slices.Sorted(maps.Keys(owners)) {
		ln, err := net.Listen("tcp", spec.Addr(port))
		if err == nil {
			ln.Close()
			continue
		}
		who := "another program"
		if name := portHolder(port); name != "" {
			who = name
		}
		held := fmt.Sprintf("%s holds %s, which %s needs", who, spec.Addr(port), owners[port])
		if !core[port] {
			warnings = append(warnings, held+", so "+owners[port]+" stays down until it is free")
			continue
		}
		busy = append(busy, held)
	}
	if len(busy) > 0 {
		return nil, fmt.Errorf("%w, or another program uses Fluxer's ports: %s. Stop it first", errOtherInstance, strings.Join(busy, ", "))
	}
	return warnings, nil
}

func portHolder(port int) string {
	inodes := map[string]bool{}
	for _, table := range []string{"/proc/net/tcp", "/proc/net/tcp6"} {
		data, err := os.ReadFile(table)
		if err != nil {
			continue
		}
		for line := range strings.Lines(string(data)) {
			f := strings.Fields(line)
			if len(f) < 10 || f[3] != "0A" {
				continue
			}
			_, hexPort, ok := strings.Cut(f[1], ":")
			if p, err := strconv.ParseUint(hexPort, 16, 32); ok && err == nil && int(p) == port {
				inodes["socket:["+f[9]+"]"] = true
			}
		}
	}
	if len(inodes) == 0 {
		return ""
	}
	procs, _ := filepath.Glob("/proc/[0-9]*")
	for _, dir := range procs {
		fds, _ := os.ReadDir(filepath.Join(dir, "fd"))
		for _, fd := range fds {
			if target, err := os.Readlink(filepath.Join(dir, "fd", fd.Name())); err == nil && inodes[target] {
				comm, _ := os.ReadFile(filepath.Join(dir, "comm"))
				return fmt.Sprintf("%s (pid %s)", strings.TrimSpace(string(comm)), filepath.Base(dir))
			}
		}
	}
	return ""
}
