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
)

func (g *zstdGatewayClient) handlePayload(payload gatewayPayload) {
	switch payload.Op {
	case gatewayOpHello:
		var hello gatewayHelloPayload
		if err := json.Unmarshal(payload.Data, &hello); err == nil {
			select {
			case g.helloCh <- hello:
			default:
			}
		}
	case gatewayOpHeartbeatAck:
	case gatewayOpDispatch:
		seq := int64(0)
		if payload.Sequence != nil {
			seq = int64(*payload.Sequence)
		}
		g.sequence.Store(seq)
		dispatch := gatewayDispatch{
			Type:     payload.Type,
			Data:     payload.Data,
			Sequence: seq,
		}

		switch payload.Type {
		case "READY":
			g.pendingMu.Lock()
			g.pendingDispatch = append(g.pendingDispatch, dispatch)
			g.pendingMu.Unlock()
			close(g.readySignal)
		case "RESUMED":
			close(g.resumedSignal)
		default:
			select {
			case g.dispatchCh <- dispatch:
			default:
				g.pendingMu.Lock()
				g.pendingDispatch = append(g.pendingDispatch, dispatch)
				g.pendingMu.Unlock()
			}
		}
	}
}
