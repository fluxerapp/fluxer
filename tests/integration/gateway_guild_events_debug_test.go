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

// TestGatewayGuildChannelCreateEvent verifies that CHANNEL_CREATE events are properly dispatched
func TestGatewayGuildChannelCreateEvent(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	ownerSocket := newGatewayClient(t, client, owner.Token)
	t.Cleanup(ownerSocket.Close)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Guild %d", time.Now().UnixNano()))

	guildCreateEvent := ownerSocket.WaitForEvent(t, "GUILD_CREATE", 15*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			ID string `json:"id"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			return false
		}
		return payload.ID == guild.ID
	})

	var guildPayload struct {
		ID string `json:"id"`
	}
	if err := json.Unmarshal(guildCreateEvent.Data, &guildPayload); err != nil {
		t.Fatalf("failed to unmarshal GUILD_CREATE payload: %v", err)
	}
	if guildPayload.ID != guild.ID {
		t.Fatalf("expected GUILD_CREATE for guild %s, got %s", guild.ID, guildPayload.ID)
	}

	channelPayload := map[string]any{
		"type": 0,
		"name": fmt.Sprintf("test-channel-%d", time.Now().UnixNano()),
	}
	resp, err := client.postJSONWithAuth(
		fmt.Sprintf("/guilds/%d/channels", parseSnowflake(t, guild.ID)),
		channelPayload,
		owner.Token,
	)
	if err != nil {
		t.Fatalf("failed to create channel: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var channel struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &channel)

	channelCreateEvent := ownerSocket.WaitForEvent(t, "CHANNEL_CREATE", 10*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			ID string `json:"id"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			return false
		}
		return payload.ID == channel.ID
	})

	var channelEventPayload struct {
		ID      string `json:"id"`
		GuildID string `json:"guild_id"`
		Type    int    `json:"type"`
		Name    string `json:"name"`
	}
	if err := json.Unmarshal(channelCreateEvent.Data, &channelEventPayload); err != nil {
		t.Fatalf("failed to unmarshal CHANNEL_CREATE payload: %v", err)
	}

	if channelEventPayload.ID != channel.ID {
		t.Errorf("expected channel ID %s, got %s", channel.ID, channelEventPayload.ID)
	}
	if channelEventPayload.GuildID != guild.ID {
		t.Errorf("expected guild ID %s, got %s", guild.ID, channelEventPayload.GuildID)
	}
	if channelEventPayload.Type != 0 {
		t.Errorf("expected channel type 0, got %d", channelEventPayload.Type)
	}
}
