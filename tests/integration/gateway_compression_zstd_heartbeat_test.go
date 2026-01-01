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
)

func (g *zstdGatewayClient) startHeartbeat(interval time.Duration) {
	g.heartbeatMu.Lock()
	defer g.heartbeatMu.Unlock()

	if g.heartbeatTimer != nil {
		g.heartbeatTimer.Stop()
	}

	g.heartbeatTimer = time.NewTicker(interval)
	go func() {
		for {
			select {
			case <-g.heartbeatTimer.C:
				g.sendHeartbeat()
			case <-g.heartbeatDone:
				return
			}
		}
	}()
}
