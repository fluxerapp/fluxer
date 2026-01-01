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

func TestGatewayVoiceStateGuildChannel(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)

	guild := createGuild(t, client, user.Token, "Voice Test Guild")
	guildID := parseSnowflake(t, guild.ID)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), map[string]any{
		"name": "voice-channel",
		"type": 2,
	}, user.Token)
	if err != nil {
		t.Fatalf("failed to create voice channel: %v", err)
	}
	var voiceChannel minimalChannelResponse
	decodeJSONResponse(t, resp, &voiceChannel)

	gatewayClient := newGatewayClient(t, client, user.Token)
	defer gatewayClient.Close()

	var connectionID string
	var lkConn *livekitConnection

	t.Run("join guild voice channel", func(t *testing.T) {
		gatewayClient.SendVoiceStateUpdate(&guild.ID, &voiceChannel.ID, nil, false, false, false, false)

		serverUpdate := gatewayClient.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, nil)
		var vsu voiceServerUpdate
		if err := json.Unmarshal(serverUpdate.Data, &vsu); err != nil {
			t.Fatalf("failed to decode VOICE_SERVER_UPDATE: %v", err)
		}

		if vsu.Token == "" {
			t.Fatal("voice server update token is empty")
		}
		if vsu.Endpoint == "" {
			t.Fatal("voice server update endpoint is empty")
		}
		if vsu.GuildID == nil || *vsu.GuildID != guild.ID {
			t.Fatalf("expected guild_id %s, got %v", guild.ID, vsu.GuildID)
		}
		if vsu.ConnectionID == "" {
			t.Fatal("connection_id should not be empty")
		}

		connectionID = vsu.ConnectionID

		t.Logf("Received voice server update: endpoint=%s, guild_id=%s, connection_id=%s",
			vsu.Endpoint, *vsu.GuildID, vsu.ConnectionID)

		roomName := guild.ID
		if vsu.GuildID != nil {
			roomName = *vsu.GuildID
		}

		lkConnChan := make(chan *livekitConnection, 1)
		go func() {
			conn := connectToLiveKit(t, vsu.Endpoint, vsu.Token, roomName, user.UserID)
			lkConnChan <- conn
		}()

		stateUpdate := gatewayClient.WaitForEvent(t, "VOICE_STATE_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				return false
			}
			return vs.UserID == user.UserID &&
				vs.ConnectionID == connectionID &&
				vs.ChannelID != nil &&
				*vs.ChannelID == voiceChannel.ID
		})

		var vs voiceStateUpdate
		if err := json.Unmarshal(stateUpdate.Data, &vs); err != nil {
			t.Fatalf("failed to decode VOICE_STATE_UPDATE: %v", err)
		}

		if vs.ChannelID == nil || *vs.ChannelID != voiceChannel.ID {
			t.Fatalf("expected channel_id %s, got %v", voiceChannel.ID, vs.ChannelID)
		}
		if vs.GuildID == nil || *vs.GuildID != guild.ID {
			t.Fatalf("expected guild_id %s, got %v", guild.ID, vs.GuildID)
		}

		select {
		case lkConn = <-lkConnChan:
			defer lkConn.disconnect()
			t.Logf("Successfully connected to LiveKit room with %d participant(s)", lkConn.getParticipantCount())
		case <-time.After(5 * time.Second):
			t.Fatal("timeout waiting for LiveKit connection")
		}
	})

	t.Run("disconnect from voice channel", func(t *testing.T) {
		gatewayClient.SendVoiceStateUpdate(&guild.ID, nil, &connectionID, false, false, false, false)

		stateUpdate := gatewayClient.WaitForEvent(t, "VOICE_STATE_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				return false
			}
			return vs.UserID == user.UserID && vs.ChannelID == nil
		})

		var vs voiceStateUpdate
		if err := json.Unmarshal(stateUpdate.Data, &vs); err != nil {
			t.Fatalf("failed to decode VOICE_STATE_UPDATE: %v", err)
		}

		if vs.ChannelID != nil {
			t.Fatalf("expected channel_id to be null after disconnect, got %v", vs.ChannelID)
		}
	})
}
