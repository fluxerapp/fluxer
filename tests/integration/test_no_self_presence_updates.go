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
	"fmt"
	"testing"
	"time"
)

// TestNoSelfPresenceUpdates verifies that a user never receives PRESENCE_UPDATE
// events for their own presence.
func TestNoSelfPresenceUpdates(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)

	ensureSessionStarted(t, client, user1.Token)
	ensureSessionStarted(t, client, user2.Token)

	createFriendship(t, client, user1, user2)

	guild := createGuild(t, client, user1.Token, fmt.Sprintf("NoSelf Guild %d", time.Now().UnixNano()))
	channelID := parseSnowflake(t, guild.SystemChannel)

	invite := createChannelInvite(t, client, user1.Token, channelID)
	joinGuild(t, client, user2.Token, invite.Code)

	user1Socket := newGatewayClient(t, client, user1.Token)
	t.Cleanup(user1Socket.Close)

	ready1 := user1Socket.WaitForEvent(t, "READY", 15*time.Second, nil)
	user1ID := extractUserIDFromReady(t, ready1.Data)

	drainPresenceEvents(t, user1Socket, 500*time.Millisecond)

	user2Socket := newGatewayClient(t, client, user2.Token)
	t.Cleanup(user2Socket.Close)

	user2Socket.WaitForEvent(t, "READY", 15*time.Second, nil)

	presences := collectPresenceUpdates(t, user1Socket, 2*time.Second)

	for _, p := range presences {
		pUserID := extractUserIDFromPresence(t, p)
		if pUserID == user1ID {
			t.Fatalf("user1 received their own PRESENCE_UPDATE with guild_id: %v", hasGuildID(p))
		}
	}
}
