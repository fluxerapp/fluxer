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

func TestVirtualAccessOnModeratorDisconnect(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, "Mod Disconnect Virtual Access")
	guildID := parseSnowflake(t, guild.ID)

	// Create public and private channels
	var publicChannel, privateChannel minimalChannelResponse

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), map[string]any{
		"name": "public-voice",
		"type": 2,
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create public channel: %v", err)
	}
	decodeJSONResponse(t, resp, &publicChannel)

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), map[string]any{
		"name": "private-voice",
		"type": 2,
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create private channel: %v", err)
	}
	decodeJSONResponse(t, resp, &privateChannel)

	privateChannelID := parseSnowflake(t, privateChannel.ID)

	invite := createChannelInvite(t, client, owner.Token, parseSnowflake(t, publicChannel.ID))
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	resp.Body.Close()

	resp, err = client.putJSONWithAuth(
		fmt.Sprintf("/channels/%d/permissions/%s", privateChannelID, guild.ID),
		map[string]any{"type": 0, "deny": fmt.Sprintf("%d", PermissionViewChannel|PermissionConnect), "allow": "0"},
		owner.Token,
	)
	if err != nil {
		t.Fatalf("failed to set channel permissions: %v", err)
	}
	resp.Body.Close()

	memberGateway := newGatewayClient(t, client, member.Token)
	defer memberGateway.Close()

	memberGateway.SendVoiceStateUpdate(&guild.ID, &publicChannel.ID, nil, false, false, false, false)
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

	resp, err = client.patchJSONWithAuth(
		fmt.Sprintf("/guilds/%d/members/%s", guildID, member.UserID),
		map[string]any{"channel_id": privateChannel.ID},
		owner.Token,
	)
	if err != nil {
		t.Fatalf("failed to move member to private channel: %v", err)
	}
	resp.Body.Close()

	memberGateway.WaitForEvent(t, "CHANNEL_CREATE", 10*time.Second, func(data json.RawMessage) bool {
		var cc channelCreateEvent
		if err := json.Unmarshal(data, &cc); err != nil {
			t.Logf("failed to decode CHANNEL_CREATE: %v", err)
			return false
		}
		return cc.ID == privateChannel.ID
	})

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
		return vs.ChannelID != nil && *vs.ChannelID == privateChannel.ID
	})

	t.Log("Member is now in private channel with virtual access")

	t.Run("moderator disconnect removes virtual access", func(t *testing.T) {
		resp, err := client.patchJSONWithAuth(
			fmt.Sprintf("/guilds/%d/members/%s", guildID, member.UserID),
			map[string]any{"channel_id": nil},
			owner.Token,
		)
		if err != nil {
			t.Fatalf("failed to disconnect member: %v", err)
		}
		resp.Body.Close()

		channelDeleteEvt := memberGateway.WaitForEvent(t, "CHANNEL_DELETE", 10*time.Second, func(data json.RawMessage) bool {
			var cd channelDeleteEvent
			if err := json.Unmarshal(data, &cd); err != nil {
				t.Logf("failed to decode CHANNEL_DELETE: %v", err)
				return false
			}
			return cd.ID == privateChannel.ID
		})

		var cd channelDeleteEvent
		if err := json.Unmarshal(channelDeleteEvt.Data, &cd); err != nil {
			t.Fatalf("failed to decode CHANNEL_DELETE payload: %v", err)
		}
		if cd.ID != privateChannel.ID {
			t.Fatalf("expected CHANNEL_DELETE for %s, got %s", privateChannel.ID, cd.ID)
		}
		t.Log("Moderator disconnect successfully revoked virtual access")
	})
}
