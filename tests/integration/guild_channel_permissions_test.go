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

func TestGuildChannelPermissions(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	ensureSessionStarted(t, client, owner.Token)
	ensureSessionStarted(t, client, member.Token)

	guild := createGuild(t, client, owner.Token, "Channel Perms Guild")
	guildID := parseSnowflake(t, guild.ID)
	channelID := parseSnowflake(t, guild.SystemChannel)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	t.Run("member can get guild channels", func(t *testing.T) {
		resp, err := client.getWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), member.Token)
		if err != nil {
			t.Fatalf("failed to get channels: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("member cannot create channel without MANAGE_CHANNELS", func(t *testing.T) {
		payload := map[string]any{"name": "member-channel", "type": 0}
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), payload, member.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusForbidden)
		resp.Body.Close()
	})

	channelPayload := map[string]any{
		"name": fmt.Sprintf("test-%d", time.Now().UnixNano()),
		"type": 0,
	}
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), channelPayload, owner.Token)
	if err != nil {
		t.Fatalf("failed to create channel: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var newChannel struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &newChannel)
	newChannelID := parseSnowflake(t, newChannel.ID)

	t.Run("member cannot delete channel without MANAGE_CHANNELS", func(t *testing.T) {
		resp, err := client.delete(fmt.Sprintf("/channels/%d", newChannelID), member.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusForbidden)
		resp.Body.Close()
	})

	t.Run("owner can delete channel", func(t *testing.T) {
		resp, err := client.delete(fmt.Sprintf("/channels/%d", newChannelID), owner.Token)
		if err != nil {
			t.Fatalf("failed to delete channel: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})
}
