// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"
	"maps"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
)

func upgradePull(ctx context.Context, inst *instance.Instance, e *engine.Engine) uint64 {
	return checks.ReleaseSize(ctx, e, slices.Sorted(maps.Values(inst.State.Images.Refs)))
}

func spaceFix(e *engine.Engine, sp checks.Space, again string) string {
	if sp.Images {
		return fmt.Sprintf("Free up space there, for example with %s image prune -a, which removes the images no container uses. Then run %s again.", strings.ToLower(e.Kind.Name()), again)
	}
	return fmt.Sprintf("Free up space there, then run %s again.", again)
}
