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
	"time"

	"github.com/gorilla/websocket"
)

func (g *gatewayClient) Close() {
	if !g.closed.CompareAndSwap(false, true) {
		return
	}
	g.stopHeartbeat()
	if g.conn != nil {
		_ = g.conn.SetWriteDeadline(time.Now().Add(5 * time.Second))
		_ = g.conn.WriteMessage(websocket.CloseMessage, websocket.FormatCloseMessage(websocket.CloseNormalClosure, "tests complete"))
		g.conn.Close()
	}
	g.wg.Wait()
}
