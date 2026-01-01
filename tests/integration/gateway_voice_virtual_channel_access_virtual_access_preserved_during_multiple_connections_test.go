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

func TestVirtualAccessPreservedDuringMultipleConnections(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, "Multi Conn Virtual Access")
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
		t.Fatalf("failed to invite member: %v", err)
	}
	resp.Body.Close()

	resp, err = client.putJSONWithAuth(
		fmt.Sprintf("/channels/%d/permissions/%s", privateChannelID, guild.ID),
		map[string]any{
			"type":  0,
			"deny":  fmt.Sprintf("%d", PermissionViewChannel|PermissionConnect),
			"allow": "0",
		},
		owner.Token,
	)
	if err != nil {
		t.Fatalf("failed to set permission overwrite: %v", err)
	}
	resp.Body.Close()

	memberGateway1 := newGatewayClient(t, client, member.Token)
	defer memberGateway1.Close()

	memberGateway2 := newGatewayClient(t, client, member.Token)
	defer memberGateway2.Close()

	memberGateway1.SendVoiceStateUpdate(&guild.ID, &publicChannel.ID, nil, false, false, false, false)
	serverUpdate1 := memberGateway1.WaitForEvent(t, "VOICE_SERVER_UPDATE", 10*time.Second, nil)
	var vsu1 voiceServerUpdate
	if err := json.Unmarshal(serverUpdate1.Data, &vsu1); err != nil {
		t.Fatalf("failed to decode initial VOICE_SERVER_UPDATE session1: %v", err)
	}
	connectionID1 := vsu1.ConnectionID

	roomName := guild.ID
	lkConn1 := connectToLiveKit(t, vsu1.Endpoint, vsu1.Token, roomName, member.UserID)
	defer lkConn1.disconnect()

	memberGateway1.WaitForEvent(t, "VOICE_STATE_UPDATE", 10*time.Second, func(data json.RawMessage) bool {
		var vs voiceStateUpdate
		if err := json.Unmarshal(data, &vs); err != nil {
			t.Logf("failed to decode session1 VOICE_STATE_UPDATE: %v", err)
			return false
		}
		return vs.ConnectionID == connectionID1
	})

	resp, err = client.patchJSONWithAuth(
		fmt.Sprintf("/guilds/%d/members/%s", guildID, member.UserID),
		map[string]any{"channel_id": privateChannel.ID, "connection_id": connectionID1},
		owner.Token,
	)
	if err != nil {
		t.Fatalf("failed to move member: %v", err)
	}
	resp.Body.Close()

	memberGateway1.WaitForEvent(t, "CHANNEL_CREATE", 10*time.Second, func(data json.RawMessage) bool {
		var cc channelCreateEvent
		if err := json.Unmarshal(data, &cc); err != nil {
			t.Logf("failed to decode CHANNEL_CREATE: %v", err)
			return false
		}
		return cc.ID == privateChannel.ID
	})

	newServerUpdate1 := memberGateway1.WaitForEvent(t, "VOICE_SERVER_UPDATE", 10*time.Second, nil)
	var newVsu1 voiceServerUpdate
	if err := json.Unmarshal(newServerUpdate1.Data, &newVsu1); err != nil {
		t.Fatalf("failed to decode VOICE_SERVER_UPDATE after move session1: %v", err)
	}
	connectionID1 = newVsu1.ConnectionID

	newLkConn1 := connectToLiveKit(t, newVsu1.Endpoint, newVsu1.Token, roomName, member.UserID)
	defer newLkConn1.disconnect()

	memberGateway1.WaitForEvent(t, "VOICE_STATE_UPDATE", 10*time.Second, func(data json.RawMessage) bool {
		var vs voiceStateUpdate
		if err := json.Unmarshal(data, &vs); err != nil {
			t.Logf("failed to decode VOICE_STATE_UPDATE after move session1: %v", err)
			return false
		}
		return vs.ConnectionID == connectionID1 && vs.ChannelID != nil && *vs.ChannelID == privateChannel.ID
	})

	memberGateway2.SendVoiceStateUpdate(&guild.ID, &privateChannel.ID, nil, false, false, false, false)
	serverUpdate2 := memberGateway2.WaitForEvent(t, "VOICE_SERVER_UPDATE", 10*time.Second, nil)
	var vsu2 voiceServerUpdate
	if err := json.Unmarshal(serverUpdate2.Data, &vsu2); err != nil {
		t.Fatalf("failed to decode VOICE_SERVER_UPDATE session2: %v", err)
	}
	connectionID2 := vsu2.ConnectionID

	lkConn2 := connectToLiveKit(t, vsu2.Endpoint, vsu2.Token, roomName, member.UserID)
	defer lkConn2.disconnect()

	memberGateway2.WaitForEvent(t, "VOICE_STATE_UPDATE", 10*time.Second, func(data json.RawMessage) bool {
		var vs voiceStateUpdate
		if err := json.Unmarshal(data, &vs); err != nil {
			t.Logf("failed to decode session2 VOICE_STATE_UPDATE: %v", err)
			return false
		}
		return vs.ConnectionID == connectionID2
	})

	t.Logf("Both sessions connected to private channel: conn1=%s, conn2=%s", connectionID1, connectionID2)

	t.Run("disconnecting one connection preserves virtual access", func(t *testing.T) {
		memberGateway1.SendVoiceStateUpdate(&guild.ID, nil, &connectionID1, false, false, false, false)

		memberGateway1.WaitForEvent(t, "VOICE_STATE_UPDATE", 10*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				t.Logf("failed to decode VOICE_STATE_UPDATE disconnect session1: %v", err)
				return false
			}
			return vs.ConnectionID == connectionID1 && vs.ChannelID == nil
		})

		time.Sleep(500 * time.Millisecond)
		t.Log("First connection disconnected, virtual access preserved (second connection still active)")
	})

	t.Run("disconnecting last connection removes virtual access", func(t *testing.T) {
		memberGateway2.SendVoiceStateUpdate(&guild.ID, nil, &connectionID2, false, false, false, false)

		channelDeleteEvt := memberGateway2.WaitForEvent(t, "CHANNEL_DELETE", 10*time.Second, func(data json.RawMessage) bool {
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
		t.Log("Last connection disconnected, virtual access revoked")
	})
}
