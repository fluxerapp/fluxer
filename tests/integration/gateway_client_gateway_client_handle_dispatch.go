/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

package integration

import (
	"encoding/json"
	"fmt"
)

func (g *gatewayClient) handleDispatch(message gatewayPayload) {

	var seq int64
	if message.Sequence != nil {
		newSeq := int64(*message.Sequence)
		for {
			current := g.sequence.Load()
			if newSeq <= current {
				seq = current
				break
			}
			if g.sequence.CompareAndSwap(current, newSeq) {
				seq = newSeq
				break
			}
		}
	} else {
		seq = g.sequence.Load()
	}

	dispatch := gatewayDispatch{
		Type:     message.Type,
		Data:     message.Data,
		Sequence: seq,
	}

	switch message.Type {
	case "READY":
		var ready struct {
			SessionID string `json:"session_id"`
		}
		if err := json.Unmarshal(message.Data, &ready); err == nil && ready.SessionID != "" {
			g.sessionMu.Lock()
			g.sessionID = ready.SessionID
			g.sessionMu.Unlock()
		}
		select {
		case g.readySignal <- struct{}{}:
		default:
		}

	case "RESUMED":
		select {
		case g.resumedSignal <- struct{}{}:
		default:
		}
	}

	if g.closed.Load() {
		return
	}
	select {
	case g.dispatchCh <- dispatch:
	default:
		panic(fmt.Sprintf("gateway dispatch channel full, dropping %s event (channel buffer: %d, test may not be consuming events)", dispatch.Type, cap(g.dispatchCh)))
	}
}
