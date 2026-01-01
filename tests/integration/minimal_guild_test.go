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

// TestSocketConnectBeforeJoinWithDelay - Test with delay after GUILD_CREATE
func TestSocketConnectBeforeJoinWithDelay(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	guest := createTestAccount(t, client)

	t.Logf("Step 1: Creating guild (owner joins)")
	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Test Guild %d", time.Now().UnixNano()))
	channelID := parseSnowflake(t, guild.SystemChannel)
	invite := createChannelInvite(t, client, owner.Token, channelID)
	t.Logf("Guild created: %s", guild.ID)

	t.Logf("Step 2: Guest connects socket BEFORE joining guild")
	guestSocket := newGatewayClient(t, client, guest.Token)
	t.Cleanup(guestSocket.Close)
	t.Logf("Guest socket connected")

	t.Logf("Step 3: Guest joins guild via invite")
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, guest.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()
	t.Logf("Guest joined guild")

	t.Logf("Step 4: Guest should receive GUILD_CREATE")
	dispatch := guestSocket.WaitForEvent(t, "GUILD_CREATE", 30*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			ID string `json:"id"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Logf("Failed to unmarshal payload: %v", err)
			return false
		}
		t.Logf("Received GUILD_CREATE for guild %s", payload.ID)
		return payload.ID == guild.ID
	})
	t.Logf("SUCCESS: Guest received GUILD_CREATE")

	t.Logf("Step 5: Waiting 1 second for gateway to fully connect to guild process...")
	time.Sleep(1 * time.Second)

	t.Logf("Step 6: Creating channel")
	channelPayload := map[string]any{
		"type": 0,
		"name": fmt.Sprintf("test-channel-%d", time.Now().UnixNano()),
	}
	resp, err = client.postJSONWithAuth(
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
	t.Logf("Channel created: %s", channel.ID)

	t.Logf("Step 7: Guest should receive CHANNEL_CREATE")
	dispatch = guestSocket.WaitForEvent(t, "CHANNEL_CREATE", 30*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			ID string `json:"id"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Logf("Failed to unmarshal payload: %v", err)
			return false
		}
		t.Logf("Received CHANNEL_CREATE for channel %s", payload.ID)
		return payload.ID == channel.ID
	})
	t.Logf("SUCCESS: Guest received CHANNEL_CREATE event: %s", string(dispatch.Data)[:100])
}
