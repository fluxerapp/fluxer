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

// TestRoleHierarchyEnforcement tests role hierarchy permissions
func TestRoleHierarchyEnforcement(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	moderator := createTestAccount(t, client)
	member := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Role Hierarchy %d", time.Now().UnixNano()))
	guildID := parseSnowflake(t, guild.ID)
	channelID := parseSnowflake(t, guild.SystemChannel)

	modRolePayload := map[string]any{
		"name":        "Moderator",
		"color":       65280,
		"permissions": "268435456",
		"hoist":       true,
	}
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), modRolePayload, owner.Token)
	if err != nil {
		t.Fatalf("failed to create mod role: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var modRole struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &modRole)

	memberRolePayload := map[string]any{
		"name":        "Member",
		"color":       16711680,
		"permissions": "0",
		"hoist":       false,
	}
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), memberRolePayload, owner.Token)
	if err != nil {
		t.Fatalf("failed to create member role: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var memberRole struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &memberRole)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, moderator.Token)
	if err != nil {
		t.Fatalf("failed to accept moderator invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept member invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/members/%s", guildID, moderator.UserID), map[string]any{
		"roles": []string{modRole.ID},
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to assign mod role: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/members/%s", guildID, member.UserID), map[string]any{
		"roles": []string{memberRole.ID},
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to assign member role: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/roles/%s", guildID, memberRole.ID), map[string]any{
		"color": 255,
	}, moderator.Token)
	if err != nil {
		t.Fatalf("failed to modify lower role: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/roles/%s", guildID, modRole.ID), map[string]any{
		"permissions": "8",
	}, moderator.Token)
	if err != nil {
		t.Fatalf("failed to attempt self-role modify: %v", err)
	}
	assertStatus(t, resp, http.StatusForbidden)
	resp.Body.Close()

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/members/@me", guildID), map[string]any{
		"roles": []string{modRole.ID},
	}, member.Token)
	if err != nil {
		t.Fatalf("failed to attempt self role assignment: %v", err)
	}
	if resp.StatusCode == http.StatusOK {
		resp, err = client.getWithAuth(fmt.Sprintf("/guilds/%d/members/%s", guildID, member.UserID), owner.Token)
		if err != nil {
			t.Fatalf("failed to fetch member: %v", err)
		}
		var fetchedMember struct {
			Roles []string `json:"roles"`
		}
		decodeJSONResponse(t, resp, &fetchedMember)
		resp.Body.Close()

		for _, r := range fetchedMember.Roles {
			if r == modRole.ID {
				t.Fatalf("CRITICAL: member was able to assign role to themselves")
			}
		}
	}
	resp.Body.Close()
}
