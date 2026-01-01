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
	"errors"
	"fmt"
	"time"
)

func (g *gatewayClient) readLoop() {
	defer g.wg.Done()
	defer g.stopHeartbeat()
	defer close(g.dispatchCh)

	for {
		if g.closed.Load() {
			return
		}
		_ = g.conn.SetReadDeadline(time.Now().Add(90 * time.Second))
		_, payload, err := g.conn.ReadMessage()
		if err != nil {
			if !g.closed.Load() {
				g.reportError(fmt.Errorf("gateway read failed: %w", err))
			}
			return
		}

		var message gatewayPayload
		if err := json.Unmarshal(payload, &message); err != nil {
			g.reportError(fmt.Errorf("failed to decode gateway payload: %w", err))
			continue
		}

		switch message.Op {
		case gatewayOpHello:
			var hello gatewayHelloPayload
			if err := json.Unmarshal(message.Data, &hello); err != nil {
				g.reportError(fmt.Errorf("failed to decode hello: %w", err))
				continue
			}
			select {
			case g.helloCh <- hello:
			default:
			}
		case gatewayOpGatewayError:
			var gatewayErr struct {
				Code    string `json:"code"`
				Message string `json:"message"`
			}
			if err := json.Unmarshal(message.Data, &gatewayErr); err != nil {
				g.reportError(fmt.Errorf("failed to decode gateway error: %w", err))
				continue
			}
			g.reportError(fmt.Errorf("gateway error %s: %s", gatewayErr.Code, gatewayErr.Message))
		case gatewayOpDispatch:
			g.handleDispatch(message)
		case gatewayOpHeartbeat:
			g.sendHeartbeat()
		case gatewayOpHeartbeatAck:
		case gatewayOpReconnect:
			g.reportError(errors.New("gateway requested reconnect"))
		case gatewayOpInvalidSession:
			g.reportError(errors.New("gateway session invalid"))
		}
	}
}
