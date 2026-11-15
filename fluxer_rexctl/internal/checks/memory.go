// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"fmt"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
)

const minMemory = 3584 << 20

func init() {
	Register(Check{ID: "memory", Phases: Preinstall | Installed, Run: runMemory})
}

func runMemory(ctx context.Context, e *Env) Result {
	total, err := host.Memory()
	if err != nil || total == 0 {
		return skip("Couldn't read how much memory this machine has.")
	}
	if total < minMemory {
		return fail(fmt.Sprintf("This machine has %s of memory and Fluxer needs at least 4 GB.", gb(total)),
			"Move Fluxer to a machine with more memory.")
	}
	return pass(fmt.Sprintf("This machine has %s of memory.", gb(total)))
}
