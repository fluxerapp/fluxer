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

// TestChannelPermissionOverwriteValidation ensures overwrite assignment respects caller permissions
func TestChannelPermissionOverwriteValidation(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	manager := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, "Overwrite Validation Guild")
	guildID := parseSnowflake(t, guild.ID)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), map[string]any{
		"name":        "manager",
		"permissions": fmt.Sprintf("%d", 1<<4),
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create role: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var role struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &role)

	invite := createChannelInvite(t, client, owner.Token, parseSnowflake(t, guild.SystemChannel))
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, manager.Token)
	if err != nil {
		t.Fatalf("failed to invite manager: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.putWithAuth(fmt.Sprintf("/guilds/%d/members/%s/roles/%s", guildID, manager.UserID, role.ID), owner.Token)
	if err != nil {
		t.Fatalf("failed to assign role: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	allowSend := 1 << 11
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), map[string]any{
		"name": "text-a",
		"type": 0,
		"permission_overwrites": []map[string]any{
			{"id": manager.UserID, "type": 1, "allow": fmt.Sprintf("%d", allowSend)},
		},
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create channel with overwrites: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var chanResp minimalChannelResponse
	decodeJSONResponse(t, resp, &chanResp)

	manageRoles := 1 << 28
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), map[string]any{
		"name": "text-bad",
		"type": 0,
		"permission_overwrites": []map[string]any{
			{"id": owner.UserID, "type": 1, "allow": fmt.Sprintf("%d", manageRoles)},
		},
	}, manager.Token)
	if err != nil {
		t.Fatalf("manager create request failed: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected MissingPermissions on create, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/channels/%d", parseSnowflake(t, chanResp.ID)), map[string]any{
		"type": 0,
		"permission_overwrites": []map[string]any{
			{"id": manager.UserID, "type": 1, "allow": fmt.Sprintf("%d", manageRoles)},
		},
	}, manager.Token)
	if err != nil {
		t.Fatalf("manager patch request failed: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected MissingPermissions on patch, got %d", resp.StatusCode)
	}
	resp.Body.Close()
}
