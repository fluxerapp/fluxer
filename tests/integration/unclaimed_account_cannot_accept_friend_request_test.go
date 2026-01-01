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
	"fmt"
	"net/http"
	"testing"
)

// TestUnclaimedAccountCannotAcceptFriendRequest verifies that unclaimed accounts
// cannot accept incoming friend requests.
func TestUnclaimedAccountCannotAcceptFriendRequest(t *testing.T) {
	client := newTestClient(t)

	senderAccount := createTestAccount(t, client)
	receiverAccount := createTestAccount(t, client)

	resp, err := client.postJSONWithAuth(
		fmt.Sprintf("/users/@me/relationships/%s", receiverAccount.UserID),
		map[string]interface{}{},
		senderAccount.Token,
	)
	if err != nil {
		t.Fatalf("failed to send friend request: %v", err)
	}
	resp.Body.Close()
	if resp.StatusCode != http.StatusOK && resp.StatusCode != http.StatusCreated && resp.StatusCode != http.StatusNoContent {
		t.Fatalf("failed to send friend request, got status %d", resp.StatusCode)
	}

	unclaimAccount(t, client, receiverAccount.UserID)

	resp, err = client.putJSONWithAuth(
		fmt.Sprintf("/users/@me/relationships/%s", senderAccount.UserID),
		map[string]int{"type": 1},
		receiverAccount.Token,
	)
	if err != nil {
		t.Fatalf("failed to attempt friend request acceptance: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("expected 400 for unclaimed account accepting friend request, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	// Verify the error code is UNCLAIMED_ACCOUNT_RESTRICTED
	var errorResp struct {
		Code string `json:"code"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&errorResp); err != nil {
		t.Fatalf("failed to decode error response: %v", err)
	}

	if errorResp.Code != "UNCLAIMED_ACCOUNT_RESTRICTED" {
		t.Fatalf("expected error code UNCLAIMED_ACCOUNT_RESTRICTED, got %s", errorResp.Code)
	}

	t.Log("Unclaimed account cannot accept friend request test passed")
}
