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

func waitForMessageDeleteBulk(t testing.TB, gw interface {
	WaitForEvent(t testing.TB, eventType string, timeout time.Duration, match func(json.RawMessage) bool) gatewayDispatch
}, channelID string, messageIDs []string) {
	t.Helper()
	timeout := 10 * time.Second
	gw.WaitForEvent(t, "MESSAGE_DELETE_BULK", timeout, func(data json.RawMessage) bool {
		var del struct {
			IDs       []string `json:"ids"`
			ChannelID string   `json:"channel_id"`
		}
		if err := json.Unmarshal(data, &del); err != nil {
			return false
		}
		if del.ChannelID != channelID {
			return false
		}
		idSet := make(map[string]bool)
		for _, id := range del.IDs {
			idSet[id] = true
		}
		for _, expectedID := range messageIDs {
			if !idSet[expectedID] {
				return false
			}
		}
		return true
	})
}
