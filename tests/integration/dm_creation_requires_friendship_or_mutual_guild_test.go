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

// TestDMCreationRequiresFriendshipOrMutualGuild verifies that users cannot create
// DMs with strangers who they don't share a guild with and aren't friends with.
func TestDMCreationRequiresFriendshipOrMutualGuild(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)

	resp, err := client.postJSONWithAuth("/users/@me/channels", map[string]string{"recipient_id": user2.UserID}, user1.Token)
	if err != nil {
		t.Fatalf("failed to attempt DM creation: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusForbidden && resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("expected 403 or 400 for DM with stranger, got %d", resp.StatusCode)
	}
}
