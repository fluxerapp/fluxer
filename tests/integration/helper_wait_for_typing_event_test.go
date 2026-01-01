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

func waitForTypingEvent(t testing.TB, gw interface {
	WaitForEvent(t testing.TB, eventType string, timeout time.Duration, match func(json.RawMessage) bool) gatewayDispatch
}, channelID, userID string) {
	t.Helper()
	timeout := 10 * time.Second
	gw.WaitForEvent(t, "TYPING_START", timeout, func(data json.RawMessage) bool {
		var typing struct {
			ChannelID string `json:"channel_id"`
			UserID    string `json:"user_id"`
		}
		if err := json.Unmarshal(data, &typing); err != nil {
			return false
		}
		return typing.ChannelID == channelID && typing.UserID == userID
	})
}
