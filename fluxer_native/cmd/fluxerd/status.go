// SPDX-License-Identifier: AGPL-3.0-or-later

package main

import (
	"context"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/notify"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/supervise"
)

func unitStatus(h spec.Health, wasReady bool) string {
	switch {
	case h.Stopping:
		return "stopping"
	case h.Ready && len(h.Degraded) > 0:
		return "degraded, not healthy " + strings.Join(h.Degraded, ", ")
	case h.Ready:
		return "ready"
	case wasReady || len(h.Failed) > 0:
		return "degraded, waiting for " + strings.Join(append(h.Failed, h.Pending...), ", ")
	}
	return "starting"
}

func reportStatus(ctx context.Context, sup *supervise.Supervisor) {
	last := ""
	wasReady := false
	for {
		changed := sup.Changes()
		h := sup.Health()
		wasReady = wasReady || h.Ready
		if line := unitStatus(h, wasReady); line != last {
			last = line
			notify.Status(line)
		}
		select {
		case <-ctx.Done():
			return
		case <-changed:
		}
	}
}
