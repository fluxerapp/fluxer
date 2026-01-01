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

func TestModeratorMoveUserToChannel(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, "Mod Move Test Guild")
	guildID := parseSnowflake(t, guild.ID)

	// Create two voice channels
	var voiceChannelA, voiceChannelB minimalChannelResponse
	for i, name := range []string{"mod-move-a", "mod-move-b"} {
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), map[string]any{
			"name": name,
			"type": 2,
		}, owner.Token)
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

	invite := createChannelInvite(t, client, owner.Token, parseSnowflake(t, voiceChannelA.ID))
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to invite member: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	memberGateway := newGatewayClient(t, client, member.Token)
	defer memberGateway.Close()

	memberGateway.SendVoiceStateUpdate(&guild.ID, &voiceChannelA.ID, nil, false, false, false, false)

	serverUpdate := memberGateway.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, nil)
	var vsu voiceServerUpdate
	if err := json.Unmarshal(serverUpdate.Data, &vsu); err != nil {
		t.Fatalf("failed to decode VOICE_SERVER_UPDATE: %v", err)
	}
	connectionID := vsu.ConnectionID

	roomName := guild.ID
	lkConn := connectToLiveKit(t, vsu.Endpoint, vsu.Token, roomName, member.UserID)
	defer lkConn.disconnect()

	memberGateway.WaitForEvent(t, "VOICE_STATE_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
		var vs voiceStateUpdate
		if err := json.Unmarshal(data, &vs); err != nil {
			return false
		}
		return vs.UserID == member.UserID && vs.ConnectionID == connectionID
	})

	t.Run("moderator moves member to another channel", func(t *testing.T) {
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

		newServerUpdate := memberGateway.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var vsu voiceServerUpdate
			if err := json.Unmarshal(data, &vsu); err != nil {
				return false
			}
			return vsu.ConnectionID != connectionID && vsu.Token != ""
		})

		var newVsu voiceServerUpdate
		if err := json.Unmarshal(newServerUpdate.Data, &newVsu); err != nil {
			t.Fatalf("failed to decode new VOICE_SERVER_UPDATE: %v", err)
		}

		require.NotEmpty(t, newVsu.ConnectionID, "new connection_id should not be empty")
		require.NotEqual(t, connectionID, newVsu.ConnectionID, "connection_id should be different for new channel")
		t.Logf("Member received new voice server update with connection_id=%s for channel B", newVsu.ConnectionID)

		newLkConn := connectToLiveKit(t, newVsu.Endpoint, newVsu.Token, roomName, member.UserID)
		defer newLkConn.disconnect()

		stateUpdate := memberGateway.WaitForEvent(t, "VOICE_STATE_UPDATE", 10*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				return false
			}
			return vs.UserID == member.UserID && vs.ChannelID != nil && *vs.ChannelID == voiceChannelB.ID
		})

		var vs voiceStateUpdate
		if err := json.Unmarshal(stateUpdate.Data, &vs); err != nil {
			t.Fatalf("failed to decode VOICE_STATE_UPDATE: %v", err)
		}

		require.NotNil(t, vs.ChannelID, "channel_id should not be null")
		require.Equal(t, voiceChannelB.ID, *vs.ChannelID, "member should be in channel B")

		memberGateway.SendVoiceStateUpdate(&guild.ID, nil, &newVsu.ConnectionID, false, false, false, false)
	})
}
