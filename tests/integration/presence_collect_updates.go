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
	"testing"
	"time"
)

func collectPresenceUpdates(t testing.TB, gw *gatewayClient, duration time.Duration) []json.RawMessage {
	t.Helper()
	var presences []json.RawMessage
	deadline := time.Now().Add(duration)
	for time.Now().Before(deadline) {
		remaining := time.Until(deadline)
		if remaining <= 0 {
			break
		}
		eventName, data := gw.NextDispatch(remaining)
		if eventName == "" {
			break
		}
		if eventName == "PRESENCE_UPDATE" || eventName == "PRESENCE_UPDATE_BULK" {
			presences = append(presences, data)
		}
	}
	return presences
}
