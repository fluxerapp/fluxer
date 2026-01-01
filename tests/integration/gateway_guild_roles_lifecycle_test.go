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
	"time"
)

func TestGatewayGuildRolesLifecycle(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	ownerSocket := newGatewayClient(t, client, owner.Token)
	t.Cleanup(ownerSocket.Close)
	memberSocket := newGatewayClient(t, client, member.Token)
	t.Cleanup(memberSocket.Close)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Roles Guild %d", time.Now().UnixNano()))
	guildID := parseSnowflake(t, guild.ID)
	channelID := parseSnowflake(t, guild.SystemChannel)
	invite := createChannelInvite(t, client, owner.Token, channelID)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	memberSocket.WaitForEvent(t, "GUILD_CREATE", 60*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			ID string `json:"id"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			return false
		}
		return payload.ID == guild.ID
	})

	rolePayload := map[string]any{
		"name":        "Test Role",
		"color":       16711680,
		"permissions": "0",
		"hoist":       true,
	}
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), rolePayload, owner.Token)
	if err != nil {
		t.Fatalf("failed to create role: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var role struct {
		ID          string `json:"id"`
		Name        string `json:"name"`
		Color       int    `json:"color"`
		Permissions string `json:"permissions"`
	}
	decodeJSONResponse(t, resp, &role)

	waitForRoleCreate := func(socket *gatewayClient) {
		socket.WaitForEvent(t, "GUILD_ROLE_CREATE", 30*time.Second, func(raw json.RawMessage) bool {
			var payload struct {
				GuildID string `json:"guild_id"`
				Role    struct {
					ID   string `json:"id"`
					Name string `json:"name"`
				} `json:"role"`
			}
			if err := json.Unmarshal(raw, &payload); err != nil {
				t.Fatalf("failed to decode role create: %v", err)
			}
			return payload.GuildID == guild.ID && payload.Role.ID == role.ID && payload.Role.Name == "Test Role"
		})
	}

	waitForRoleCreate(ownerSocket)
	waitForRoleCreate(memberSocket)

	updatePayload := map[string]any{
		"name":  "Updated Role",
		"color": 65280,
	}
	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/roles/%s", guildID, role.ID), updatePayload, owner.Token)
	if err != nil {
		t.Fatalf("failed to update role: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	waitForRoleUpdate := func(socket *gatewayClient) {
		socket.WaitForEvent(t, "GUILD_ROLE_UPDATE", 30*time.Second, func(raw json.RawMessage) bool {
			var payload struct {
				GuildID string `json:"guild_id"`
				Role    struct {
					ID    string `json:"id"`
					Name  string `json:"name"`
					Color int    `json:"color"`
				} `json:"role"`
			}
			if err := json.Unmarshal(raw, &payload); err != nil {
				t.Fatalf("failed to decode role update: %v", err)
			}
			return payload.GuildID == guild.ID && payload.Role.ID == role.ID && payload.Role.Name == "Updated Role" && payload.Role.Color == 65280
		})
	}

	waitForRoleUpdate(ownerSocket)
	waitForRoleUpdate(memberSocket)

	memberResp, err := client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/members/%s", guildID, member.UserID), map[string]any{
		"roles": []string{role.ID},
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to assign role to member: %v", err)
	}
	assertStatus(t, memberResp, http.StatusOK)
	memberResp.Body.Close()

	ownerSocket.WaitForEvent(t, "GUILD_MEMBER_UPDATE", 60*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			GuildID string `json:"guild_id"`
			User    struct {
				ID string `json:"id"`
			} `json:"user"`
			Roles []string `json:"roles"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode member update: %v", err)
		}
		if payload.GuildID != guild.ID || payload.User.ID != member.UserID {
			return false
		}
		for _, r := range payload.Roles {
			if r == role.ID {
				return true
			}
		}
		return false
	})

	memberSocket.WaitForEvent(t, "GUILD_MEMBER_UPDATE", 60*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			GuildID string   `json:"guild_id"`
			Roles   []string `json:"roles"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode member update: %v", err)
		}
		if payload.GuildID != guild.ID {
			return false
		}
		for _, r := range payload.Roles {
			if r == role.ID {
				return true
			}
		}
		return false
	})

	resp, err = client.delete(fmt.Sprintf("/guilds/%d/roles/%s", guildID, role.ID), owner.Token)
	if err != nil {
		t.Fatalf("failed to delete role: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	waitForRoleDelete := func(socket *gatewayClient) {
		socket.WaitForEvent(t, "GUILD_ROLE_DELETE", 30*time.Second, func(raw json.RawMessage) bool {
			var payload struct {
				GuildID string `json:"guild_id"`
				RoleID  string `json:"role_id"`
			}
			if err := json.Unmarshal(raw, &payload); err != nil {
				t.Fatalf("failed to decode role delete: %v", err)
			}
			return payload.GuildID == guild.ID && payload.RoleID == role.ID
		})
	}

	waitForRoleDelete(ownerSocket)
	waitForRoleDelete(memberSocket)
}
