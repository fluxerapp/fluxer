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
	"net/http"
	"testing"
)

// TestDMCreationAllowedWithFriendship verifies that friends can create DMs with each other.
func TestDMCreationAllowedWithFriendship(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)

	createFriendship(t, client, user1, user2)

	resp, err := client.postJSONWithAuth("/users/@me/channels", map[string]string{"recipient_id": user2.UserID}, user1.Token)
	if err != nil {
		t.Fatalf("failed to create DM: %v", err)
	}
	defer resp.Body.Close()

	assertStatus(t, resp, http.StatusOK)

	var channel minimalChannelResponse
	decodeJSONResponse(t, resp, &channel)
	if channel.ID == "" {
		t.Fatalf("expected channel ID in response")
	}
}
