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

func TestGatewayFriendPresenceWithoutGuild(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)

	createFriendship(t, client, user1, user2)

	user1Socket := newGatewayClient(t, client, user1.Token)
	t.Cleanup(user1Socket.Close)
	user2Socket := newGatewayClient(t, client, user2.Token)
	t.Cleanup(user2Socket.Close)

	ready1 := user1Socket.WaitForEvent(t, "READY", 15*time.Second, nil)
	ready2 := user2Socket.WaitForEvent(t, "READY", 15*time.Second, nil)

	assertUserInReady(t, ready1.Data, user2.UserID)
	assertUserInReady(t, ready2.Data, user1.UserID)

	matchPresence := func(expectedUserID string) func(json.RawMessage) bool {
		return func(raw json.RawMessage) bool {
			var payload map[string]any
			if err := json.Unmarshal(raw, &payload); err != nil {
				t.Fatalf("failed to decode presence update: %v", err)
			}
			user, ok := payload["user"].(map[string]any)
			if !ok {
				return false
			}
			id, ok := user["id"].(string)
			if !ok || id != expectedUserID {
				return false
			}
			_, hasGuild := payload["guild_id"]
			return !hasGuild
		}
	}

	user1Socket.WaitForEvent(t, "PRESENCE_UPDATE", 30*time.Second, matchPresence(user2.UserID))
}
