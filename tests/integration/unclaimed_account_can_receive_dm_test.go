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

// TestUnclaimedAccountCanReceiveDM verifies that claimed accounts can send DMs
// to unclaimed accounts.
func TestUnclaimedAccountCanReceiveDM(t *testing.T) {
	client := newTestClient(t)

	claimedAccount := createTestAccount(t, client)
	unclaimedAccount := createTestAccount(t, client)

	guild := createGuild(t, client, claimedAccount.Token, "Test Guild")
	invite := createChannelInvite(t, client, claimedAccount.Token, parseSnowflake(t, guild.SystemChannel))
	joinGuild(t, client, unclaimedAccount.Token, invite.Code)

	unclaimAccount(t, client, unclaimedAccount.UserID)

	resp, err := client.postJSONWithAuth("/users/@me/channels", map[string]string{
		"recipient_id": unclaimedAccount.UserID,
	}, claimedAccount.Token)
	if err != nil {
		t.Fatalf("failed to attempt DM creation: %v", err)
	}
	defer resp.Body.Close()

	assertStatus(t, resp, http.StatusOK)

	var channel minimalChannelResponse
	decodeJSONResponse(t, resp, &channel)
	if channel.ID == "" {
		t.Fatalf("expected channel ID in response")
	}

	t.Log("Unclaimed account can receive DM test passed")
}
