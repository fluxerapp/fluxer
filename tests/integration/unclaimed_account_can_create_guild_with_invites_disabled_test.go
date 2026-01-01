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

// TestUnclaimedAccountCanCreateGuildWithInvitesDisabled verifies that unclaimed accounts
// can create preview guilds, which automatically have INVITES_DISABLED feature enabled.
func TestUnclaimedAccountCanCreateGuildWithInvitesDisabled(t *testing.T) {
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

	if resp.StatusCode != http.StatusOK {
		t.Fatalf("expected 200 for unclaimed account creating guild, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var guild guildCreateResponse
	decodeJSONResponse(t, resp, &guild)

	if guild.ID == "" {
		t.Fatalf("guild response missing id")
	}

	if !slices.Contains(guild.Features, "INVITES_DISABLED") {
		t.Fatalf("expected preview guild to have INVITES_DISABLED feature, got features: %v", guild.Features)
	}

	t.Log("Unclaimed account can create guild with INVITES_DISABLED test passed")
}
