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

func waitForMessageEvent(t testing.TB, gw interface {
	WaitForEvent(t testing.TB, eventType string, timeout time.Duration, match func(json.RawMessage) bool) gatewayDispatch
}, eventType, messageID string, contentCheck func(string) bool) {
	t.Helper()
	timeout := 10 * time.Second
	gw.WaitForEvent(t, eventType, timeout, func(data json.RawMessage) bool {
		var message struct {
			ID      string `json:"id"`
			Content string `json:"content"`
		}
		if err := json.Unmarshal(data, &message); err != nil {
			return false
		}
		if message.ID != messageID {
			return false
		}
		if contentCheck != nil && !contentCheck(message.Content) {
			return false
		}
		return true
	})
}
