// SPDX-License-Identifier: AGPL-3.0-or-later

package main

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/lock"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

var selfCgroup = "/proc/self/cgroup"

type runtimeLookup struct {
	data     string
	dir      string
	unit     string
	explicit bool
	recorded bool
	err      error
}

func lookupRuntime(l spec.Layout, explicit bool) runtimeLookup {
	r := runtimeLookup{data: l.Data, dir: l.Runtime, explicit: explicit}
	if explicit {
		return r
	}
	data, err := os.ReadFile(l.RuntimeRecord())
	if errors.Is(err, fs.ErrNotExist) {
		return r
	}
	if err != nil {
		r.err = err
		return r
	}
	if held, err := lock.Held(l.LockFile()); err == nil && !held {
		return r
	}
	lines := strings.Split(string(data), "\n")
	if dir := strings.TrimSpace(lines[0]); dir != "" {
		r.dir, r.recorded = dir, true
	}
	if len(lines) > 1 {
		r.unit = strings.TrimSpace(lines[1])
	}
	return r
}

func recordRuntime(l spec.Layout) (func(), error) {
	if err := os.Remove(l.RuntimeRecord()); err != nil && !errors.Is(err, fs.ErrNotExist) {
		return nil, err
	}
	if err := os.WriteFile(l.RuntimeRecord(), []byte(l.Runtime+"\n"+serviceUnit()+"\n"), 0o600); err != nil {
		return nil, err
	}
	return func() { _ = os.Remove(l.RuntimeRecord()) }, nil
}

func serviceUnit() string {
	if os.Getenv("INVOCATION_ID") == "" {
		return ""
	}
	data, err := os.ReadFile(selfCgroup)
	if err != nil {
		return ""
	}
	return unitFromCgroup(string(data))
}

func unitFromCgroup(cgroup string) string {
	for line := range strings.SplitSeq(cgroup, "\n") {
		path, ok := strings.CutPrefix(line, "0::")
		if !ok {
			continue
		}
		parts := strings.Split(strings.TrimSpace(path), "/")
		if slices.ContainsFunc(parts, func(p string) bool { return strings.HasPrefix(p, "user@") }) {
			return ""
		}
		for _, part := range slices.Backward(parts) {
			if strings.HasSuffix(part, ".service") {
				return part
			}
		}
	}
	return ""
}

func (r runtimeLookup) unreachable(socket string, err error, instead string) error {
	switch {
	case r.explicit:
		return fmt.Errorf("Fluxer's operator socket %s isn't reachable (%w). The folder comes from %s. Check that Fluxer is running, or unset %s to use the folder of the running fluxerd", socket, err, spec.RuntimeDirEnv, spec.RuntimeDirEnv)
	case r.err != nil:
		return fmt.Errorf("fluxerd couldn't find the socket of the running Fluxer (%w). Run fluxerd as the user Fluxer runs as or root", r.err)
	case r.recorded:
		return fmt.Errorf("Fluxer's operator socket %s isn't reachable (%w). It's the socket of the fluxerd with the data folder %s. Run fluxerd health and try again once it prints ready", socket, err, r.data)
	}
	tail := ""
	if instead != "" {
		tail = ". rexctl manages this instance, so run " + instead + " instead"
	}
	return fmt.Errorf("Fluxer's operator socket %s isn't reachable (%w). No fluxerd is running with the data folder %s. Start Fluxer, or set %s to the data folder of the instance you mean%s", socket, err, r.data, spec.DataDirEnv, tail)
}
