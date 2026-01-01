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

func createFriendship(t testing.TB, client *testClient, user1, user2 testAccount) {
	t.Helper()
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/users/@me/relationships/%s", user2.UserID), nil, user1.Token)
	if err != nil {
		t.Fatalf("failed to send friend request: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.putJSONWithAuth(fmt.Sprintf("/users/@me/relationships/%s", user1.UserID), nil, user2.Token)
	if err != nil {
		t.Fatalf("failed to accept friend request: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()
}
