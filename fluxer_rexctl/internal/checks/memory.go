// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"fmt"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	minMemory  = 1536 << 20
	goodMemory = 3584 << 20
)

func init() {
	Register(Check{ID: "memory", Phases: Preinstall | Installed, Run: runMemory})
}

func runMemory(ctx context.Context, e *Env) Result {
	total, err := host.Memory()
	if err != nil || total == 0 {
		return skip("Couldn't read how much memory this machine has.")
	}
	return memoryResult(total)
}

func memoryResult(total uint64) Result {
	have := ui.Memory(int64(total))
	switch {
	case total < minMemory:
		return fail(fmt.Sprintf("This machine has %s of memory and Fluxer needs at least 1.5 GB.", have),
			"Move Fluxer to a machine with more memory.")
	case total < goodMemory:
		return warn(fmt.Sprintf("This machine has %s of memory. Fluxer sizes itself to fit and works for a few people, and a busy instance needs 4 GB.", have),
			"If Fluxer gets slow, add memory, or run rexctl config set FLUXER_SEARCH_ENGINE=none to turn off search.")
	}
	return pass(fmt.Sprintf("This machine has %s of memory.", have))
}
