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

// TestChannelTypePermissions tests permissions across different channel types
func TestChannelTypePermissions(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Channel Type Guild %d", time.Now().UnixNano()))
	guildID := parseSnowflake(t, guild.ID)
	channelID := parseSnowflake(t, guild.SystemChannel)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), map[string]any{
		"type": 0,
		"name": "member-channel",
	}, member.Token)
	if err != nil {
		t.Fatalf("failed to attempt channel create: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 for channel create without permission, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), map[string]any{
		"type": 0,
		"name": "text-channel",
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create text channel: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var textChannel struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &textChannel)

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/channels/%s", textChannel.ID), map[string]string{
		"name": "hacked-channel",
	}, member.Token)
	if err != nil {
		t.Fatalf("failed to attempt channel modify: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 for channel modify without permission, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.delete(fmt.Sprintf("/channels/%s", textChannel.ID), member.Token)
	if err != nil {
		t.Fatalf("failed to attempt channel delete: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 for channel delete without permission, got %d", resp.StatusCode)
	}
	resp.Body.Close()
}
