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

	"github.com/stretchr/testify/require"
)

func TestMultipleConnectionsModeratorMoveAll(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, "Multi Conn Move Test")
	guildID := parseSnowflake(t, guild.ID)

	// Create two voice channels
	var voiceChannelA, voiceChannelB minimalChannelResponse
	for i, name := range []string{"multi-conn-a", "multi-conn-b"} {
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), map[string]any{
			"name": name,
			"type": 2,
		}, owner.Token)
		if err != nil {
			t.Fatalf("failed to create voice channel: %v", err)
		}
		var ch minimalChannelResponse
		decodeJSONResponse(t, resp, &ch)
		if i == 0 {
			voiceChannelA = ch
		} else {
			voiceChannelB = ch
		}
	}

	invite := createChannelInvite(t, client, owner.Token, parseSnowflake(t, voiceChannelA.ID))
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to invite member: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	memberGateway1 := newGatewayClient(t, client, member.Token)
	defer memberGateway1.Close()

	memberGateway2 := newGatewayClient(t, client, member.Token)
	defer memberGateway2.Close()

	memberGateway1.SendVoiceStateUpdate(&guild.ID, &voiceChannelA.ID, nil, false, false, false, false)
	serverUpdate1 := memberGateway1.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, nil)
	var vsu1 voiceServerUpdate
	if err := json.Unmarshal(serverUpdate1.Data, &vsu1); err != nil {
		t.Fatalf("failed to decode VOICE_SERVER_UPDATE: %v", err)
	}
	connectionID1 := vsu1.ConnectionID

	roomName := guild.ID
	lkConn1 := connectToLiveKit(t, vsu1.Endpoint, vsu1.Token, roomName, member.UserID)
	defer lkConn1.disconnect()

	memberGateway1.WaitForEvent(t, "VOICE_STATE_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
		var vs voiceStateUpdate
		if err := json.Unmarshal(data, &vs); err != nil {
			return false
		}
		return vs.UserID == member.UserID && vs.ConnectionID == connectionID1
	})

	memberGateway2.SendVoiceStateUpdate(&guild.ID, &voiceChannelA.ID, nil, false, false, false, false)
	serverUpdate2 := memberGateway2.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, nil)
	var vsu2 voiceServerUpdate
	if err := json.Unmarshal(serverUpdate2.Data, &vsu2); err != nil {
		t.Fatalf("failed to decode VOICE_SERVER_UPDATE: %v", err)
	}
	connectionID2 := vsu2.ConnectionID

	lkConn2 := connectToLiveKit(t, vsu2.Endpoint, vsu2.Token, roomName, member.UserID)
	defer lkConn2.disconnect()

	memberGateway2.WaitForEvent(t, "VOICE_STATE_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
		var vs voiceStateUpdate
		if err := json.Unmarshal(data, &vs); err != nil {
			return false
		}
		return vs.UserID == member.UserID && vs.ConnectionID == connectionID2
	})

	t.Logf("Member has two connections: %s and %s", connectionID1, connectionID2)

	t.Run("moderator moves all connections to channel B", func(t *testing.T) {
		resp, err := client.patchJSONWithAuth(
			fmt.Sprintf("/guilds/%d/members/%s", guildID, member.UserID),
			map[string]any{"channel_id": voiceChannelB.ID},
			owner.Token,
		)
		if err != nil {
			t.Fatalf("failed to move member: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()

		newServerUpdate1 := memberGateway1.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var vsu voiceServerUpdate
			if err := json.Unmarshal(data, &vsu); err != nil {
				return false
			}
			return vsu.Token != ""
		})
		var newVsu1 voiceServerUpdate
		if err := json.Unmarshal(newServerUpdate1.Data, &newVsu1); err != nil {
			t.Fatalf("failed to decode VOICE_SERVER_UPDATE for connection 1: %v", err)
		}

		newServerUpdate2 := memberGateway2.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var vsu voiceServerUpdate
			if err := json.Unmarshal(data, &vsu); err != nil {
				return false
			}
			return vsu.Token != ""
		})
		var newVsu2 voiceServerUpdate
		if err := json.Unmarshal(newServerUpdate2.Data, &newVsu2); err != nil {
			t.Fatalf("failed to decode VOICE_SERVER_UPDATE for connection 2: %v", err)
		}

		require.NotEmpty(t, newVsu1.Token, "connection 1 should receive new token")
		require.NotEmpty(t, newVsu2.Token, "connection 2 should receive new token")
		t.Logf("Both sessions received new voice tokens for channel B")

		newLkConn1 := connectToLiveKit(t, newVsu1.Endpoint, newVsu1.Token, roomName, member.UserID)
		defer newLkConn1.disconnect()

		newLkConn2 := connectToLiveKit(t, newVsu2.Endpoint, newVsu2.Token, roomName, member.UserID)
		defer newLkConn2.disconnect()

		memberGateway1.WaitForEvent(t, "VOICE_STATE_UPDATE", 10*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				return false
			}
			return vs.UserID == member.UserID && vs.ChannelID != nil && *vs.ChannelID == voiceChannelB.ID
		})

		stateUpdate2 := memberGateway2.WaitForEvent(t, "VOICE_STATE_UPDATE", 10*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				return false
			}
			return vs.UserID == member.UserID && vs.ChannelID != nil && *vs.ChannelID == voiceChannelB.ID
		})

		var vs1, vs2 voiceStateUpdate
		require.NoError(t, json.Unmarshal(stateUpdate2.Data, &vs2), "should decode VOICE_STATE_UPDATE for connection 2")
		require.NotNil(t, vs2.ChannelID, "connection 2 should have channel_id")
		require.Equal(t, voiceChannelB.ID, *vs2.ChannelID, "connection 2 should be in channel B")

		stateData1 := memberGateway1.WaitForEvent(t, "VOICE_STATE_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				return false
			}
			return vs.UserID == member.UserID && vs.ChannelID != nil && *vs.ChannelID == voiceChannelB.ID
		})
		require.NoError(t, json.Unmarshal(stateData1.Data, &vs1), "should decode VOICE_STATE_UPDATE for connection 1")
		require.NotNil(t, vs1.ChannelID, "connection 1 should have channel_id")
		require.Equal(t, voiceChannelB.ID, *vs1.ChannelID, "connection 1 should be in channel B")
	})
}
