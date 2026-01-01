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

	"github.com/stretchr/testify/require"
)

func TestVoiceSelfMoveToAccessibleChannels(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)

	guild := createGuild(t, client, user.Token, "Self Move Test Guild")
	guildID := parseSnowflake(t, guild.ID)

	// Create two voice channels
	var voiceChannelA, voiceChannelB minimalChannelResponse
	for i, name := range []string{"voice-channel-a", "voice-channel-b"} {
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), map[string]any{
			"name": name,
			"type": 2,
		}, user.Token)
		if err != nil {
			t.Fatalf("failed to create voice channel %s: %v", name, err)
		}
		var ch minimalChannelResponse
		decodeJSONResponse(t, resp, &ch)
		if i == 0 {
			voiceChannelA = ch
		} else {
			voiceChannelB = ch
		}
	}

	gatewayClient := newGatewayClient(t, client, user.Token)
	defer gatewayClient.Close()

	gatewayClient.SendVoiceStateUpdate(&guild.ID, &voiceChannelA.ID, nil, false, false, false, false)

	serverUpdate := gatewayClient.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, nil)
	var vsu voiceServerUpdate
	if err := json.Unmarshal(serverUpdate.Data, &vsu); err != nil {
		t.Fatalf("failed to decode VOICE_SERVER_UPDATE: %v", err)
	}
	connectionID := vsu.ConnectionID

	roomName := guild.ID
	lkConn := connectToLiveKit(t, vsu.Endpoint, vsu.Token, roomName, user.UserID)
	defer lkConn.disconnect()

	gatewayClient.WaitForEvent(t, "VOICE_STATE_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
		var vs voiceStateUpdate
		if err := json.Unmarshal(data, &vs); err != nil {
			return false
		}
		return vs.UserID == user.UserID && vs.ConnectionID == connectionID && vs.ChannelID != nil && *vs.ChannelID == voiceChannelA.ID
	})

	t.Run("user can move self to another accessible channel", func(t *testing.T) {
		gatewayClient.SendVoiceStateUpdate(&guild.ID, &voiceChannelB.ID, &connectionID, false, false, false, false)

		stateUpdate := gatewayClient.WaitForEvent(t, "VOICE_STATE_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				return false
			}
			return vs.UserID == user.UserID && vs.ConnectionID == connectionID && vs.ChannelID != nil && *vs.ChannelID == voiceChannelB.ID
		})

		var vs voiceStateUpdate
		if err := json.Unmarshal(stateUpdate.Data, &vs); err != nil {
			t.Fatalf("failed to decode VOICE_STATE_UPDATE: %v", err)
		}

		require.NotNil(t, vs.ChannelID, "channel_id should not be null")
		require.Equal(t, voiceChannelB.ID, *vs.ChannelID, "user should be in channel B")
	})

	t.Run("user can move back to original channel", func(t *testing.T) {
		gatewayClient.SendVoiceStateUpdate(&guild.ID, &voiceChannelA.ID, &connectionID, false, false, false, false)

		stateUpdate := gatewayClient.WaitForEvent(t, "VOICE_STATE_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				return false
			}
			return vs.UserID == user.UserID && vs.ConnectionID == connectionID && vs.ChannelID != nil && *vs.ChannelID == voiceChannelA.ID
		})

		var vs voiceStateUpdate
		if err := json.Unmarshal(stateUpdate.Data, &vs); err != nil {
			t.Fatalf("failed to decode VOICE_STATE_UPDATE: %v", err)
		}

		require.NotNil(t, vs.ChannelID, "channel_id should not be null")
		require.Equal(t, voiceChannelA.ID, *vs.ChannelID, "user should be in channel A")
	})

	gatewayClient.SendVoiceStateUpdate(&guild.ID, nil, &connectionID, false, false, false, false)
	gatewayClient.WaitForEvent(t, "VOICE_STATE_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
		var vs voiceStateUpdate
		if err := json.Unmarshal(data, &vs); err != nil {
			return false
		}
		return vs.UserID == user.UserID && vs.ChannelID == nil
	})
}
