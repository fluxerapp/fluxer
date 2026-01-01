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

func TestGuildRolePermissions(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)
	ensureSessionStarted(t, client, owner.Token)
	ensureSessionStarted(t, client, member.Token)

	guild := createGuild(t, client, owner.Token, "Role Perms Guild")
	guildID := parseSnowflake(t, guild.ID)
	channelID := parseSnowflake(t, guild.SystemChannel)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	t.Run("member cannot create role without MANAGE_ROLES", func(t *testing.T) {
		payload := map[string]any{
			"name":        "Member Role",
			"permissions": "0",
		}
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), payload, member.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusForbidden)
		resp.Body.Close()
	})

	payload := map[string]any{
		"name":        "Test Role",
		"permissions": "0",
		"color":       16711680,
	}
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), payload, owner.Token)
	if err != nil {
		t.Fatalf("failed to create role: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var role struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &role)

	t.Run("member cannot update role without MANAGE_ROLES", func(t *testing.T) {
		updatePayload := map[string]any{"name": "Hacked Role"}
		resp, err := client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/roles/%s", guildID, role.ID), updatePayload, member.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusForbidden)
		resp.Body.Close()
	})

	t.Run("member cannot delete role without MANAGE_ROLES", func(t *testing.T) {
		resp, err := client.delete(fmt.Sprintf("/guilds/%d/roles/%s", guildID, role.ID), member.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusForbidden)
		resp.Body.Close()
	})

	t.Run("owner can update role", func(t *testing.T) {
		updatePayload := map[string]any{"name": "Updated Role"}
		resp, err := client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/roles/%s", guildID, role.ID), updatePayload, owner.Token)
		if err != nil {
			t.Fatalf("failed to update role: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("owner can delete role", func(t *testing.T) {
		resp, err := client.delete(fmt.Sprintf("/guilds/%d/roles/%s", guildID, role.ID), owner.Token)
		if err != nil {
			t.Fatalf("failed to delete role: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})
}
