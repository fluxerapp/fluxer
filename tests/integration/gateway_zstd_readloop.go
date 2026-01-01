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
	"time"

	"github.com/gorilla/websocket"
)

func (g *zstdGatewayClient) readLoop() {
	defer g.wg.Done()

	for {
		if g.closed.Load() {
			return
		}

		_ = g.conn.SetReadDeadline(time.Now().Add(90 * time.Second))
		msgType, data, err := g.conn.ReadMessage()
		if err != nil {
			if !g.closed.Load() {
				select {
				case g.errCh <- err:
				default:
				}
			}
			return
		}

		var jsonData []byte

		if msgType == websocket.BinaryMessage {
			jsonData, err = g.zstdDecoder.DecodeAll(data, nil)
			if err != nil {
				select {
				case g.errCh <- err:
				default:
				}
				continue
			}
		} else {
			jsonData = data
		}

		var payload gatewayPayload
		if err := json.Unmarshal(jsonData, &payload); err != nil {
			continue
		}

		g.handlePayload(payload)
	}
}
