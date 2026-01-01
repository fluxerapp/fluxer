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

// TestNoSelfPresenceInGuildBulk verifies that when receiving bulk presence updates
// for a guild, the user's own presence is filtered out.
func TestNoSelfPresenceInGuildBulk(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)

	ensureSessionStarted(t, client, user1.Token)

	guild := createGuild(t, client, user1.Token, fmt.Sprintf("BulkTest Guild %d", time.Now().UnixNano()))

	user1Socket := newGatewayClient(t, client, user1.Token)
	t.Cleanup(user1Socket.Close)

	ready1 := user1Socket.WaitForEvent(t, "READY", 15*time.Second, nil)
	user1ID := extractUserIDFromReady(t, ready1.Data)

	presences := extractPresencesFromReady(t, ready1.Data)
	for _, p := range presences {
		pUserID := extractUserIDFromPresenceMap(t, p)
		if pUserID == user1ID {
			t.Fatalf("READY included user's own presence in presences array")
		}
	}

	drainPresenceEvents(t, user1Socket, 500*time.Millisecond)

	presenceUpdates := collectPresenceUpdates(t, user1Socket, 1*time.Second)
	for _, p := range presenceUpdates {
		pUserID := extractUserIDFromPresence(t, p)
		if pUserID == user1ID {
			guildIDVal := extractGuildIDFromPresence(p)
			t.Fatalf("user received their own PRESENCE_UPDATE for guild %s", guildIDVal)
		}
	}

	_ = guild
}
