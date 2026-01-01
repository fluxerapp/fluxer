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
	"testing"
	"time"
)

func TestVirtualAccessNotGrantedForNormallyAccessibleChannel(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, "Normal Access Test")
	guildID := parseSnowflake(t, guild.ID)

	// Create two PUBLIC voice channels
	var channelA, channelB minimalChannelResponse

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), map[string]any{
		"name": "channel-a",
		"type": 2,
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create channel A: %v", err)
	}
	decodeJSONResponse(t, resp, &channelA)

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), map[string]any{
		"name": "channel-b",
		"type": 2,
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create channel B: %v", err)
	}
	decodeJSONResponse(t, resp, &channelB)

	invite := createChannelInvite(t, client, owner.Token, parseSnowflake(t, channelA.ID))
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	resp.Body.Close()

	memberGateway := newGatewayClient(t, client, member.Token)
	defer memberGateway.Close()

	memberGateway.SendVoiceStateUpdate(&guild.ID, &channelA.ID, nil, false, false, false, false)
	serverUpdate := memberGateway.WaitForEvent(t, "VOICE_SERVER_UPDATE", 10*time.Second, nil)
	var vsu voiceServerUpdate
	if err := json.Unmarshal(serverUpdate.Data, &vsu); err != nil {
		t.Fatalf("failed to decode initial VOICE_SERVER_UPDATE: %v", err)
	}

	roomName := guild.ID
	lkConn := connectToLiveKit(t, vsu.Endpoint, vsu.Token, roomName, member.UserID)
	defer lkConn.disconnect()

	memberGateway.WaitForEvent(t, "VOICE_STATE_UPDATE", 10*time.Second, func(data json.RawMessage) bool {
		var vs voiceStateUpdate
		if err := json.Unmarshal(data, &vs); err != nil {
			t.Logf("failed to decode initial VOICE_STATE_UPDATE: %v", err)
			return false
		}
		return vs.ConnectionID == vsu.ConnectionID
	})

	t.Run("moderator move to accessible channel does not create virtual access", func(t *testing.T) {
		resp, err := client.patchJSONWithAuth(
			fmt.Sprintf("/guilds/%d/members/%s", guildID, member.UserID),
			map[string]any{"channel_id": channelB.ID},
			owner.Token,
		)
		if err != nil {
			t.Fatalf("failed to move member: %v", err)
		}
		resp.Body.Close()

		newServerUpdate := memberGateway.WaitForEvent(t, "VOICE_SERVER_UPDATE", 10*time.Second, nil)
		var newVsu voiceServerUpdate
		if err := json.Unmarshal(newServerUpdate.Data, &newVsu); err != nil {
			t.Fatalf("failed to decode VOICE_SERVER_UPDATE after move: %v", err)
		}

		newLkConn := connectToLiveKit(t, newVsu.Endpoint, newVsu.Token, roomName, member.UserID)
		defer newLkConn.disconnect()

		memberGateway.WaitForEvent(t, "VOICE_STATE_UPDATE", 10*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				t.Logf("failed to decode VOICE_STATE_UPDATE after move: %v", err)
				return false
			}
			return vs.ChannelID != nil && *vs.ChannelID == channelB.ID
		})

		memberGateway.SendVoiceStateUpdate(&guild.ID, nil, &newVsu.ConnectionID, false, false, false, false)

		memberGateway.WaitForEvent(t, "VOICE_STATE_UPDATE", 10*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				t.Logf("failed to decode VOICE_STATE_UPDATE on disconnect: %v", err)
				return false
			}
			return vs.ChannelID == nil
		})

		time.Sleep(500 * time.Millisecond)
		t.Log("No CHANNEL_CREATE/DELETE events for normally accessible channel")
	})
}
