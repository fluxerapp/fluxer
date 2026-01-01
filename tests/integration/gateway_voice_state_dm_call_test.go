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
	"testing"
	"time"
)

func TestGatewayVoiceStateDMCall(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)

	guild := createGuild(t, client, user1.Token, "Test Guild")
	invite := createChannelInvite(t, client, user1.Token, parseSnowflake(t, guild.SystemChannel))
	joinGuild(t, client, user2.Token, invite.Code)

	dm := createDmChannel(t, client, user1.Token, parseSnowflake(t, user2.UserID))

	gatewayClient := newGatewayClient(t, client, user1.Token)
	defer gatewayClient.Close()

	t.Run("join DM voice call", func(t *testing.T) {
		gatewayClient.SendVoiceStateUpdate(nil, &dm.ID, nil, false, false, false, false)

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
		if vsu.GuildID != nil {
			t.Fatalf("expected guild_id to be null for DM call, got %v", vsu.GuildID)
		}
		if vsu.ConnectionID == "" {
			t.Fatal("connection_id should not be empty")
		}

		connectionID := vsu.ConnectionID
		t.Logf("Received DM voice server update: endpoint=%s, connection_id=%s",
			vsu.Endpoint, vsu.ConnectionID)

		stateUpdate := gatewayClient.WaitForEvent(t, "VOICE_STATE_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				return false
			}
			return vs.UserID == user1.UserID && vs.ConnectionID == connectionID
		})

		roomName := dm.ID
		lkConn := connectToLiveKit(t, vsu.Endpoint, vsu.Token, roomName, user1.UserID)
		defer lkConn.disconnect()

		if lkConn.getParticipantCount() < 1 {
			t.Fatal("expected at least 1 participant in LiveKit room")
		}

		t.Logf("Successfully connected to LiveKit DM room with %d participant(s)", lkConn.getParticipantCount())

		var vs voiceStateUpdate
		if err := json.Unmarshal(stateUpdate.Data, &vs); err != nil {
			t.Fatalf("failed to decode VOICE_STATE_UPDATE: %v", err)
		}

		if vs.ChannelID == nil || *vs.ChannelID != dm.ID {
			t.Fatalf("expected channel_id %s, got %v", dm.ID, vs.ChannelID)
		}
		if vs.GuildID != nil {
			t.Fatalf("expected guild_id to be null for DM call, got %v", vs.GuildID)
		}
	})
}
