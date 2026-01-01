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

// TestUnclaimedAccountCanJoinGuildByInvite verifies that unclaimed accounts
// CAN join guilds/communities via invites (unless DISALLOW_UNCLAIMED_ACCOUNTS is set).
func TestUnclaimedAccountCanJoinGuildByInvite(t *testing.T) {
	client := newTestClient(t)

	ownerAccount := createTestAccount(t, client)
	memberAccount := createTestAccount(t, client)

	guild := createGuild(t, client, ownerAccount.Token, "Test Guild")

	invite := createChannelInvite(t, client, ownerAccount.Token, parseSnowflake(t, guild.SystemChannel))

	unclaimAccount(t, client, memberAccount.UserID)

	resp, err := client.postJSONWithAuth("/invites/"+invite.Code, nil, memberAccount.Token)
	if err != nil {
		t.Fatalf("failed to join guild: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Fatalf("expected 200 for unclaimed account joining guild, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	t.Log("Unclaimed account can join guild by invite test passed")
}
