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

func TestGuildMemberRoleManagement(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	ensureSessionStarted(t, client, owner.Token)
	ensureSessionStarted(t, client, member.Token)

	guild := createGuild(t, client, owner.Token, "Role Management Guild")
	guildID := parseSnowflake(t, guild.ID)
	channelID := parseSnowflake(t, guild.SystemChannel)

	rolePayload := map[string]any{"name": "Test Role"}
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), rolePayload, owner.Token)
	if err != nil {
		t.Fatalf("failed to create role: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var role struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &role)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	t.Run("member cannot add role to self without MANAGE_ROLES", func(t *testing.T) {
		resp, err := client.putWithAuth(fmt.Sprintf("/guilds/%d/members/%s/roles/%s", guildID, member.UserID, role.ID), member.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusForbidden)
		resp.Body.Close()
	})

	t.Run("owner can add role to member", func(t *testing.T) {
		resp, err := client.putWithAuth(fmt.Sprintf("/guilds/%d/members/%s/roles/%s", guildID, member.UserID, role.ID), owner.Token)
		if err != nil {
			t.Fatalf("failed to add role: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})

	t.Run("member cannot remove role from self without MANAGE_ROLES", func(t *testing.T) {
		resp, err := client.delete(fmt.Sprintf("/guilds/%d/members/%s/roles/%s", guildID, member.UserID, role.ID), member.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusForbidden)
		resp.Body.Close()
	})

	t.Run("owner can remove role from member", func(t *testing.T) {
		resp, err := client.delete(fmt.Sprintf("/guilds/%d/members/%s/roles/%s", guildID, member.UserID, role.ID), owner.Token)
		if err != nil {
			t.Fatalf("failed to remove role: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})

	t.Run("reject adding nonexistent role", func(t *testing.T) {
		resp, err := client.putWithAuth(fmt.Sprintf("/guilds/%d/members/%s/roles/999999999999999999", guildID, member.UserID), owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusNotFound)
		resp.Body.Close()
	})
}
