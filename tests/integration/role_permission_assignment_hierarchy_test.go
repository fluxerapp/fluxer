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

// TestRolePermissionAssignmentHierarchy ensures users cannot grant permissions they don't possess or edit higher roles
func TestRolePermissionAssignmentHierarchy(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	manager := createTestAccount(t, client)
	ensureSessionStarted(t, client, owner.Token)
	ensureSessionStarted(t, client, manager.Token)

	guild := createGuild(t, client, owner.Token, "Role Hierarchy Guild")
	guildID := parseSnowflake(t, guild.ID)

	createRole := func(name string, perms int64) string {
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), map[string]any{
			"name":        name,
			"permissions": fmt.Sprintf("%d", perms),
		}, owner.Token)
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

	roleHigh := createRole("High", 1<<28)
	roleMid := createRole("Mid", 1<<11)

	invite := createChannelInvite(t, client, owner.Token, parseSnowflake(t, guild.SystemChannel))
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, manager.Token)
	if err != nil {
		t.Fatalf("failed to invite manager: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.putWithAuth(fmt.Sprintf("/guilds/%d/members/%s/roles/%s", guildID, manager.UserID, roleMid), owner.Token)
	if err != nil {
		t.Fatalf("failed to assign mid role: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/roles/%s", guildID, roleHigh), map[string]any{
		"permissions": fmt.Sprintf("%d", 1<<28),
	}, manager.Token)
	if err != nil {
		t.Fatalf("manager patch high role request failed: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 editing higher role, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/roles/%s", guildID, roleMid), map[string]any{
		"permissions": fmt.Sprintf("%d", (1 << 28)),
	}, manager.Token)
	if err != nil {
		t.Fatalf("manager elevate perms request failed: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 elevating permissions, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/roles/%s", guildID, roleMid), map[string]any{
		"permissions": fmt.Sprintf("%d", (1<<11)|(1<<13)),
	}, owner.Token)
	if err != nil {
		t.Fatalf("owner update perms failed: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()
}
