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

// TestUnclaimedAccountCannotSendGuildMessage verifies that unclaimed accounts
// cannot send messages in guild channels.
func TestUnclaimedAccountCannotSendGuildMessage(t *testing.T) {
	client := newTestClient(t)

	ownerAccount := createTestAccount(t, client)
	memberAccount := createTestAccount(t, client)

	guild := createGuild(t, client, ownerAccount.Token, "Test Guild")

	invite := createChannelInvite(t, client, ownerAccount.Token, parseSnowflake(t, guild.SystemChannel))
	joinGuild(t, client, memberAccount.Token, invite.Code)

	unclaimAccount(t, client, memberAccount.UserID)

	resp, err := client.postJSONWithAuth(
		fmt.Sprintf("/channels/%s/messages", guild.SystemChannel),
		map[string]string{"content": "Hello world"},
		memberAccount.Token,
	)
	if err != nil {
		t.Fatalf("failed to attempt message send: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("expected 400 for unclaimed account sending guild message, got %d: %s", resp.StatusCode, readResponseBody(resp))
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

	t.Log("Unclaimed account cannot send guild message test passed")
}
