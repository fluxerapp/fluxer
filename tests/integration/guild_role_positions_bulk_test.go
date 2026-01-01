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
	"sort"
	"testing"
)

// TestGuildRolePositionsBulk covers bulk role reordering and permission enforcement
func TestGuildRolePositionsBulk(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)
	ensureSessionStarted(t, client, owner.Token)
	ensureSessionStarted(t, client, member.Token)

	guild := createGuild(t, client, owner.Token, "Role Position Bulk")
	guildID := parseSnowflake(t, guild.ID)

	invite := createChannelInvite(t, client, owner.Token, parseSnowflake(t, guild.SystemChannel))
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	createRole := func(name string) string {
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), map[string]any{"name": name}, owner.Token)
		if err != nil {
			t.Fatalf("failed to create role %s: %v", name, err)
		}
		assertStatus(t, resp, http.StatusOK)
		var role struct {
			ID string `json:"id"`
		}
		decodeJSONResponse(t, resp, &role)
		return role.ID
	}

	roleA := createRole("Role A")
	roleB := createRole("Role B")
	roleC := createRole("Role C")

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), []map[string]any{{
		"id":       roleA,
		"position": 1,
	}}, member.Token)
	if err != nil {
		t.Fatalf("member reorder request failed: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 for member reorder, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), []map[string]any{
		{"id": roleC, "position": 3},
		{"id": roleA, "position": 2},
		{"id": roleB, "position": 1},
	}, owner.Token)
	if err != nil {
		t.Fatalf("owner reorder request failed: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.getWithAuth(fmt.Sprintf("/guilds/%d", guildID), owner.Token)
	if err != nil {
		t.Fatalf("failed to fetch guild: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var guildResp struct {
		Roles []struct {
			ID       string `json:"id"`
			Position int    `json:"position"`
		} `json:"roles"`
	}
	decodeJSONResponse(t, resp, &guildResp)
	resp.Body.Close()

	roles := guildResp.Roles
	sort.SliceStable(roles, func(i, j int) bool {
		return roles[i].Position > roles[j].Position
	})
	expected := []string{roleC, roleA, roleB}
	for idx, role := range roles {
		if idx >= len(expected) {
			break
		}
		if role.ID == guild.ID {
			continue
		}
		if role.ID != expected[idx] {
			t.Fatalf("expected role order %v, got role %s at index %d", expected, role.ID, idx)
		}
	}
}
