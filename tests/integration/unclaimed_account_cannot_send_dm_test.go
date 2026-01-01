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
	"net/http"
	"testing"
)

// TestUnclaimedAccountCannotSendDM verifies that unclaimed accounts
// cannot send direct messages even if they share mutual guilds with the target user.
func TestUnclaimedAccountCannotSendDM(t *testing.T) {
	client := newTestClient(t)

	unclaimedAccount := createTestAccount(t, client)
	targetAccount := createTestAccount(t, client)

	guild := createGuild(t, client, unclaimedAccount.Token, "Test Guild")
	invite := createChannelInvite(t, client, unclaimedAccount.Token, parseSnowflake(t, guild.SystemChannel))
	joinGuild(t, client, targetAccount.Token, invite.Code)

	unclaimAccount(t, client, unclaimedAccount.UserID)

	resp, err := client.postJSONWithAuth("/users/@me/channels", map[string]string{
		"recipient_id": targetAccount.UserID,
	}, unclaimedAccount.Token)
	if err != nil {
		t.Fatalf("failed to attempt DM creation: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("expected 400 for unclaimed account DM creation, got %d: %s", resp.StatusCode, readResponseBody(resp))
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

	t.Log("Unclaimed account cannot send DM test passed")
}
