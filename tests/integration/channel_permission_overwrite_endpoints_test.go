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

// TestChannelPermissionOverwriteEndpoints exercises the PUT/DELETE overwrite endpoints and permission checks
func TestChannelPermissionOverwriteEndpoints(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, "Overwrite Endpoint Guild")
	guildID := parseSnowflake(t, guild.ID)

	channel := createGuildChannel(t, client, owner.Token, guildID, "perm-channel")
	channelID := parseSnowflake(t, channel.ID)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to invite member: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	viewChannel := int64(1 << 10)
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), map[string]any{
		"name":        "viewer",
		"permissions": fmt.Sprintf("%d", viewChannel),
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create role: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var role struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &role)
	resp.Body.Close()

	resp, err = client.putWithAuth(fmt.Sprintf("/guilds/%d/members/%s/roles/%s", guildID, member.UserID, role.ID), owner.Token)
	if err != nil {
		t.Fatalf("failed to assign role: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	sendMessages := int64(1 << 11)
	resp, err = client.requestJSON(http.MethodPut, fmt.Sprintf("/channels/%d/permissions/%s", channelID, role.ID), map[string]any{
		"type":  0,
		"allow": fmt.Sprintf("%d", sendMessages),
		"deny":  "0",
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to PUT overwrite: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.getWithAuth(fmt.Sprintf("/channels/%d", channelID), owner.Token)
	if err != nil {
		t.Fatalf("failed to fetch channel: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var channelResp struct {
		PermissionOverwrites []struct {
			ID    string `json:"id"`
			Allow string `json:"allow"`
			Deny  string `json:"deny"`
		} `json:"permission_overwrites"`
	}
	decodeJSONResponse(t, resp, &channelResp)
	resp.Body.Close()

	found := false
	for _, ow := range channelResp.PermissionOverwrites {
		if ow.ID == role.ID {
			found = true
			if ow.Allow != fmt.Sprintf("%d", sendMessages) {
				t.Fatalf("expected allow %d, got %s", sendMessages, ow.Allow)
			}
		}
	}
	if !found {
		t.Fatalf("expected overwrite for role to exist")
	}

	manageRoles := int64(1 << 28)
	resp, err = client.requestJSON(http.MethodPut, fmt.Sprintf("/channels/%d/permissions/%s", channelID, member.UserID), map[string]any{
		"type":  1,
		"allow": fmt.Sprintf("%d", manageRoles),
	}, member.Token)
	if err != nil {
		t.Fatalf("member PUT overwrite failed: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 for member overwrite, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.requestJSON(http.MethodDelete, fmt.Sprintf("/channels/%d/permissions/%s", channelID, role.ID), nil, owner.Token)
	if err != nil {
		t.Fatalf("failed to DELETE overwrite: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()
}
