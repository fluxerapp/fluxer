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

type gatewayChannelPayload struct {
	ID      string `json:"id"`
	GuildID string `json:"guild_id"`
}

func TestGatewayGuildAndChannelEvents(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	guest := createTestAccount(t, client)
	stranger := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Gateway Guild %d", time.Now().UnixNano()))
	channelID := parseSnowflake(t, guild.SystemChannel)
	invite := createChannelInvite(t, client, owner.Token, channelID)

	guestSocket := newGatewayClient(t, client, guest.Token)
	t.Cleanup(guestSocket.Close)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, guest.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	guestSocket.WaitForEvent(t, "GUILD_CREATE", 60*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			ID string `json:"id"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode guild create payload: %v", err)
		}
		return payload.ID == guild.ID
	})

	ownerSocket := newGatewayClient(t, client, owner.Token)
	t.Cleanup(ownerSocket.Close)

	strangerSocket := newGatewayClient(t, client, stranger.Token)
	t.Cleanup(strangerSocket.Close)

	newChannelPayload := map[string]any{
		"type": 0,
		"name": fmt.Sprintf("gateway-text-%d", time.Now().UnixNano()),
		"nsfw": false,
	}
	resp, err = client.postJSONWithAuth(
		fmt.Sprintf("/guilds/%d/channels", parseSnowflake(t, guild.ID)),
		newChannelPayload,
		owner.Token,
	)
	if err != nil {
		t.Fatalf("failed to create guild channel: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var channel gatewayChannelPayload
	decodeJSONResponse(t, resp, &channel)

	waitForChannelEventFunc := func(socket *gatewayClient, event string) {
		socket.WaitForEvent(t, event, 60*time.Second, func(raw json.RawMessage) bool {
			var payload gatewayChannelPayload
			if err := json.Unmarshal(raw, &payload); err != nil {
				t.Fatalf("failed to decode %s payload: %v", event, err)
			}
			return payload.ID == channel.ID
		})
	}

	waitForChannelEventFunc(ownerSocket, "CHANNEL_CREATE")
	waitForChannelEventFunc(guestSocket, "CHANNEL_CREATE")
	strangerSocket.AssertNoEvent(t, "CHANNEL_CREATE", 5*time.Second, func(raw json.RawMessage) bool {
		var payload gatewayChannelPayload
		if err := json.Unmarshal(raw, &payload); err != nil {
			return false
		}
		return payload.ID == channel.ID
	})

	resp, err = client.delete(fmt.Sprintf("/channels/%d", parseSnowflake(t, channel.ID)), owner.Token)
	if err != nil {
		t.Fatalf("failed to delete channel: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	waitForChannelEventFunc(ownerSocket, "CHANNEL_DELETE")
	waitForChannelEventFunc(guestSocket, "CHANNEL_DELETE")
	strangerSocket.AssertNoEvent(t, "CHANNEL_DELETE", 5*time.Second, func(raw json.RawMessage) bool {
		var payload gatewayChannelPayload
		if err := json.Unmarshal(raw, &payload); err != nil {
			return false
		}
		return payload.ID == channel.ID
	})
}
