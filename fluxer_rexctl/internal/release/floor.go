// SPDX-License-Identifier: AGPL-3.0-or-later

package release

import (
	"errors"
	"fmt"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
)

var (
	ErrEngineTooOld = errors.New("release: the engine is older than the release needs")
	ErrOlderSchema  = errors.New("release: the release can't read this instance's state")
)

type EngineTooOldError struct {
	Kind    engine.Kind
	Have    string
	Need    string
	Release string
}

func (e *EngineTooOldError) Error() string {
	return fmt.Sprintf("release: %s needs %s %s or newer, the engine has %s", e.Release, e.Kind, e.Need, e.Have)
}

func (e *EngineTooOldError) Is(target error) bool { return target == ErrEngineTooOld }

type OlderSchemaError struct {
	Have    int
	Reads   int
	Release string
}

func (e *OlderSchemaError) Error() string {
	return fmt.Sprintf("release: %s reads state schema %d, this instance has %d", e.Release, e.Reads, e.Have)
}

func (e *OlderSchemaError) Is(target error) bool { return target == ErrOlderSchema }

func CheckTarget(e *engine.Engine, to *Release, have int) error {
	if to == nil {
		return nil
	}
	if to.StateSchema > 0 && to.StateSchema < have {
		return &OlderSchemaError{Have: have, Reads: to.StateSchema, Release: to.Version}
	}
	if e == nil {
		return nil
	}
	need := to.MinDocker
	if e.Kind == engine.Podman {
		need = to.MinPodman
	}
	if need == "" || e.Version == "" {
		return nil
	}
	if compareEngine(e.Version, need) < 0 {
		return &EngineTooOldError{Kind: e.Kind, Have: e.Version, Need: need, Release: to.Version}
	}
	return nil
}

func compareEngine(a, b string) int {
	pa, pb := engineVersion(a), engineVersion(b)
	for i := range pa {
		if pa[i] != pb[i] {
			if pa[i] < pb[i] {
				return -1
			}
			return 1
		}
	}
	return 0
}

func engineVersion(v string) [3]int {
	var out [3]int
	v = strings.TrimPrefix(strings.TrimSpace(v), "v")
	if i := strings.IndexAny(v, "-+ ~"); i >= 0 {
		v = v[:i]
	}
	for i, part := range strings.SplitN(v, ".", 3) {
		n, err := strconv.Atoi(part)
		if err != nil {
			break
		}
		out[i] = n
	}
	return out
}
