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

func TestGatewayVoiceStateMultiUser(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)

	guild := createGuild(t, client, user1.Token, "Multi-User Voice Guild")
	guildID := parseSnowflake(t, guild.ID)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), map[string]any{
		"name": "multi-voice",
		"type": 2,
	}, user1.Token)
	if err != nil {
		t.Fatalf("failed to create voice channel: %v", err)
	}
	var voiceChannel minimalChannelResponse
	decodeJSONResponse(t, resp, &voiceChannel)

	invite := createChannelInvite(t, client, user1.Token, parseSnowflake(t, voiceChannel.ID))

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, user2.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	resp.Body.Close()

	gateway1 := newGatewayClient(t, client, user1.Token)
	defer gateway1.Close()

	gateway2 := newGatewayClient(t, client, user2.Token)
	defer gateway2.Close()

	var lkConn1, lkConn2 *livekitConnection

	defer func() {
		if lkConn1 != nil {
			lkConn1.disconnect()
		}
		if lkConn2 != nil {
			lkConn2.disconnect()
		}
	}()

	var user1ConnectionID, user2ConnectionID string

	t.Run("user2 receives voice state when user1 joins", func(t *testing.T) {
		gateway1.SendVoiceStateUpdate(&guild.ID, &voiceChannel.ID, nil, false, false, false, false)

		serverUpdate1 := gateway1.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, nil)
		var vsu voiceServerUpdate
		if err := json.Unmarshal(serverUpdate1.Data, &vsu); err != nil {
			t.Fatalf("failed to decode VOICE_SERVER_UPDATE: %v", err)
		}
		user1ConnectionID = vsu.ConnectionID

		roomName := guild.ID
		lkConn1 = connectToLiveKit(t, vsu.Endpoint, vsu.Token, roomName, user1.UserID)

		stateUpdate := gateway2.WaitForEvent(t, "VOICE_STATE_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				return false
			}
			return vs.UserID == user1.UserID
		})

		var vs voiceStateUpdate
		if err := json.Unmarshal(stateUpdate.Data, &vs); err != nil {
			t.Fatalf("failed to decode VOICE_STATE_UPDATE: %v", err)
		}

		if vs.UserID != user1.UserID {
			t.Fatalf("expected user_id %s, got %s", user1.UserID, vs.UserID)
		}
		if vs.ChannelID == nil || *vs.ChannelID != voiceChannel.ID {
			t.Fatalf("expected channel_id %s, got %v", voiceChannel.ID, vs.ChannelID)
		}

		t.Logf("User2 received voice state update for user1 joining")
	})

	t.Run("user1 receives voice state when user2 joins", func(t *testing.T) {
		gateway2.SendVoiceStateUpdate(&guild.ID, &voiceChannel.ID, nil, false, false, false, false)

		serverUpdate2 := gateway2.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, nil)
		var vsu voiceServerUpdate
		if err := json.Unmarshal(serverUpdate2.Data, &vsu); err != nil {
			t.Fatalf("failed to decode VOICE_SERVER_UPDATE: %v", err)
		}
		user2ConnectionID = vsu.ConnectionID

		roomName := guild.ID
		lkConn2 = connectToLiveKit(t, vsu.Endpoint, vsu.Token, roomName, user2.UserID)

		time.Sleep(500 * time.Millisecond)

		user1Identity := getParticipantIdentity(user1.UserID, user1ConnectionID)
		user2Identity := getParticipantIdentity(user2.UserID, user2ConnectionID)

		if !lkConn1.waitForParticipant(user2Identity, 5*time.Second) {
			t.Fatalf("user1 did not see user2 join the LiveKit room (expected identity: %s)", user2Identity)
		}
		if !lkConn2.waitForParticipant(user1Identity, 5*time.Second) {
			t.Fatalf("user2 did not see user1 in the LiveKit room (expected identity: %s)", user1Identity)
		}

		t.Logf("Both users successfully connected to LiveKit and can see each other")

		stateUpdate := gateway1.WaitForEvent(t, "VOICE_STATE_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				return false
			}
			return vs.UserID == user2.UserID
		})

		var vs voiceStateUpdate
		if err := json.Unmarshal(stateUpdate.Data, &vs); err != nil {
			t.Fatalf("failed to decode VOICE_STATE_UPDATE: %v", err)
		}

		if vs.UserID != user2.UserID {
			t.Fatalf("expected user_id %s, got %s", user2.UserID, vs.UserID)
		}

		t.Logf("User1 received voice state update for user2 joining")
	})
}
