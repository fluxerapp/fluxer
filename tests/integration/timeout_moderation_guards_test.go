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
	"time"
)

func TestTimeoutModerationGuards(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	moderator := createTestAccount(t, client)
	targetModerator := createTestAccount(t, client)
	higherMember := createTestAccount(t, client)

	for _, account := range []struct {
		name  string
		token string
	}{
		{"owner", owner.Token},
		{"moderator", moderator.Token},
		{"targetModerator", targetModerator.Token},
		{"higherMember", higherMember.Token},
	} {
		ensureSessionStarted(t, client, account.token)
	}

	guild := createGuild(t, client, owner.Token, "Timeout Moderation Guard Guild")
	guildID := parseSnowflake(t, guild.ID)

	invite := createChannelInvite(t, client, owner.Token, parseSnowflake(t, guild.SystemChannel))
	for _, token := range []string{moderator.Token, targetModerator.Token, higherMember.Token} {
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, token)
		if err != nil {
			t.Fatalf("failed to accept invite: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	}

	createRole := func(name string, permissions int64) string {
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), map[string]any{
			"name":        name,
			"permissions": fmt.Sprintf("%d", permissions),
		}, owner.Token)
		if err != nil {
			t.Fatalf("failed to create role %s: %v", name, err)
		}
		assertStatus(t, resp, http.StatusOK)
		var role struct {
			ID string `json:"id"`
		}
		decodeJSONResponse(t, resp, &role)
		resp.Body.Close()
		return role.ID
	}

	const moderatePermission = 1 << 40
	modRole := createRole("Timeout Moderator Role", moderatePermission)
	juniorModRole := createRole("Junior Moderator Role", moderatePermission)
	higherRole := createRole("Higher Role", 0)

	resp, err := client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), []map[string]any{
		{"id": higherRole, "position": 3},
		{"id": modRole, "position": 2},
		{"id": juniorModRole, "position": 1},
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to reorder roles: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	assignRole := func(userID, roleID string) {
		resp, err := client.putWithAuth(
			fmt.Sprintf("/guilds/%d/members/%s/roles/%s", guildID, userID, roleID),
			owner.Token,
		)
		if err != nil {
			t.Fatalf("failed to assign role %s to user %s: %v", roleID, userID, err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	}

	assignRole(moderator.UserID, modRole)
	assignRole(targetModerator.UserID, juniorModRole)
	assignRole(higherMember.UserID, higherRole)

	attemptTimeout := func(token, targetUserID string) {
		timeout := time.Now().UTC().Add(15 * time.Minute).Format(time.RFC3339)
		resp, err := client.patchJSONWithAuth(
			fmt.Sprintf("/guilds/%d/members/%s", guildID, targetUserID),
			map[string]any{"communication_disabled_until": timeout},
			token,
		)
		if err != nil {
			t.Fatalf("timeout request failed: %v", err)
		}
		if resp.StatusCode != http.StatusForbidden {
			t.Fatalf("expected 403 when timing out %s, got %d", targetUserID, resp.StatusCode)
		}
		resp.Body.Close()
	}

	attemptTimeout(moderator.Token, targetModerator.UserID)
	attemptTimeout(moderator.Token, higherMember.UserID)
}
