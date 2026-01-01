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

func TestRelationshipCannotSendFriendRequestToBlockedUser(t *testing.T) {
	client := newTestClient(t)
	alice := createTestAccount(t, client)
	bob := createTestAccount(t, client)

	resp, err := client.putJSONWithAuth(
		fmt.Sprintf("/users/@me/relationships/%s", bob.UserID),
		map[string]int{"type": relationshipBlocked},
		alice.Token,
	)
	if err != nil {
		t.Fatalf("failed to block user: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/users/@me/relationships/%s", bob.UserID), nil, alice.Token)
	if err != nil {
		t.Fatalf("failed to attempt friend request: %v", err)
	}
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("expected 400, got %d", resp.StatusCode)
	}
	var errResp struct {
		Code string `json:"code"`
	}
	decodeJSONResponse(t, resp, &errResp)
	if errResp.Code != "CANNOT_SEND_FRIEND_REQUEST_TO_BLOCKED_USER" {
		t.Fatalf("expected CANNOT_SEND_FRIEND_REQUEST_TO_BLOCKED_USER, got %s", errResp.Code)
	}
}
