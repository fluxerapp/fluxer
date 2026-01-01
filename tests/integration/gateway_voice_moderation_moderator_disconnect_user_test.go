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

func TestModeratorDisconnectUser(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, "Mod Disconnect Test Guild")
	guildID := parseSnowflake(t, guild.ID)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), map[string]any{
		"name": "disconnect-test",
		"type": 2,
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create voice channel: %v", err)
	}
	var voiceChannel minimalChannelResponse
	decodeJSONResponse(t, resp, &voiceChannel)

	invite := createChannelInvite(t, client, owner.Token, parseSnowflake(t, voiceChannel.ID))
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to invite member: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	memberGateway := newGatewayClient(t, client, member.Token)
	defer memberGateway.Close()

	memberGateway.SendVoiceStateUpdate(&guild.ID, &voiceChannel.ID, nil, false, false, false, false)

	serverUpdate := memberGateway.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, nil)
	var vsu voiceServerUpdate
	if err := json.Unmarshal(serverUpdate.Data, &vsu); err != nil {
		t.Fatalf("failed to decode VOICE_SERVER_UPDATE: %v", err)
	}

	roomName := guild.ID
	lkConn := connectToLiveKit(t, vsu.Endpoint, vsu.Token, roomName, member.UserID)
	defer lkConn.disconnect()

	memberGateway.WaitForEvent(t, "VOICE_STATE_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
		var vs voiceStateUpdate
		if err := json.Unmarshal(data, &vs); err != nil {
			return false
		}
		return vs.UserID == member.UserID && vs.ConnectionID == vsu.ConnectionID
	})

	t.Run("moderator disconnects member by setting channel_id to null", func(t *testing.T) {
		resp, err := client.patchJSONWithAuth(
			fmt.Sprintf("/guilds/%d/members/%s", guildID, member.UserID),
			map[string]any{"channel_id": nil},
			owner.Token,
		)
		if err != nil {
			t.Fatalf("failed to disconnect member: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()

		stateUpdate := memberGateway.WaitForEvent(t, "VOICE_STATE_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				return false
			}
			return vs.UserID == member.UserID && vs.ChannelID == nil
		})

		var vs voiceStateUpdate
		if err := json.Unmarshal(stateUpdate.Data, &vs); err != nil {
			t.Fatalf("failed to decode VOICE_STATE_UPDATE: %v", err)
		}

		require.Nil(t, vs.ChannelID, "channel_id should be null after moderator disconnect")
	})
}
