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
	"slices"
	"testing"
)

// TestClaimedAccountOwnerCanEnableInvitesAfterClaim verifies that after an unclaimed account owner
// claims their account, they can enable invites by toggling off INVITES_DISABLED.
func TestClaimedAccountOwnerCanEnableInvitesAfterClaim(t *testing.T) {
	client := newTestClient(t)

	account := createTestAccount(t, client)

	unclaimAccount(t, client, account.UserID)

	resp, err := client.postJSONWithAuth("/guilds", map[string]string{
		"name": "Preview Guild",
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to create guild: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusOK)

	var guild guildCreateResponse
	decodeJSONResponse(t, resp, &guild)

	if !slices.Contains(guild.Features, "INVITES_DISABLED") {
		t.Fatalf("expected preview guild to have INVITES_DISABLED feature")
	}

	newPassword := uniquePassword()
	claimResp, err := client.patchJSONWithAuth("/users/@me", map[string]any{
		"new_password": newPassword,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to claim account: %v", err)
	}
	defer claimResp.Body.Close()
	assertStatus(t, claimResp, http.StatusOK)

	updateResp, err := client.patchJSONWithAuth("/guilds/"+guild.ID, map[string]any{
		"features": []string{},
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to update guild features: %v", err)
	}
	defer updateResp.Body.Close()
	assertStatus(t, updateResp, http.StatusOK)

	var updatedGuild guildCreateResponse
	decodeJSONResponse(t, updateResp, &updatedGuild)

	if slices.Contains(updatedGuild.Features, "INVITES_DISABLED") {
		t.Fatalf("INVITES_DISABLED should be removable after owner claims account, got features: %v", updatedGuild.Features)
	}

	t.Log("Claimed account owner can enable invites after claim test passed")
}
