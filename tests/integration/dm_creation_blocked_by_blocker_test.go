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
	"net/http"
	"testing"
)

// TestDMCreationBlockedByBlocker verifies that a user cannot create a DM with someone
// who has blocked them, even if they share a guild.
func TestDMCreationBlockedByBlocker(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)

	guild := createGuild(t, client, user1.Token, "Test Community")
	invite := createChannelInvite(t, client, user1.Token, parseSnowflake(t, guild.SystemChannel))
	joinGuild(t, client, user2.Token, invite.Code)

	resp, err := client.putJSONWithAuth(fmt.Sprintf("/users/@me/relationships/%s", user2.UserID), map[string]int{"type": relationshipBlocked}, user1.Token)
	if err != nil {
		t.Fatalf("failed to block user: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.postJSONWithAuth("/users/@me/channels", map[string]string{"recipient_id": user1.UserID}, user2.Token)
	if err != nil {
		t.Fatalf("failed to attempt DM creation: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusForbidden && resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("expected 403 or 400 for DM to blocker, got %d", resp.StatusCode)
	}
}
