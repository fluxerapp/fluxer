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

// TestUnclaimedAccountCannotJoinGroupDM verifies that unclaimed accounts
// cannot join group DMs via invite.
func TestUnclaimedAccountCannotJoinGroupDM(t *testing.T) {
	client := newTestClient(t)

	ownerAccount := createTestAccount(t, client)
	friend1Account := createTestAccount(t, client)
	friend2Account := createTestAccount(t, client)

	createFriendship(t, client, ownerAccount, friend1Account)
	createFriendship(t, client, ownerAccount, friend2Account)

	resp, err := client.postJSONWithAuth("/users/@me/channels", map[string]interface{}{
		"recipients": []string{friend1Account.UserID},
	}, ownerAccount.Token)
	if err != nil {
		t.Fatalf("failed to create group DM: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Fatalf("failed to create group DM, got status %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var channel struct {
		ID string `json:"id"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&channel); err != nil {
		t.Fatalf("failed to decode channel response: %v", err)
	}

	resp, err = client.postJSONWithAuth(
		fmt.Sprintf("/channels/%s/invites", channel.ID),
		map[string]interface{}{},
		ownerAccount.Token,
	)
	if err != nil {
		t.Fatalf("failed to create group DM invite: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Fatalf("failed to create group DM invite, got status %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var inviteResp struct {
		Code string `json:"code"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&inviteResp); err != nil {
		t.Fatalf("failed to decode invite response: %v", err)
	}

	unclaimAccount(t, client, friend2Account.UserID)

	resp, err = client.postJSONWithAuth("/invites/"+inviteResp.Code, nil, friend2Account.Token)
	if err != nil {
		t.Fatalf("failed to attempt group DM join: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("expected 400 for unclaimed account joining group DM, got %d: %s", resp.StatusCode, readResponseBody(resp))
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

	t.Log("Unclaimed account cannot join group DM test passed")
}
