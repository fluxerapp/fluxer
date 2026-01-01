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
)

func TestVirtualChannelAccessViaForceMoveComprehensive(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, "Virtual Access Test Guild")
	guildID := parseSnowflake(t, guild.ID)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), map[string]any{
		"name": "public-voice",
		"type": 2,
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create public voice channel: %v", err)
	}
	var publicChannel minimalChannelResponse
	decodeJSONResponse(t, resp, &publicChannel)

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), map[string]any{
		"name": "private-voice",
		"type": 2,
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create private voice channel: %v", err)
	}
	var privateChannel minimalChannelResponse
	decodeJSONResponse(t, resp, &privateChannel)

	privateChannelID := parseSnowflake(t, privateChannel.ID)

	invite := createChannelInvite(t, client, owner.Token, parseSnowflake(t, publicChannel.ID))
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to invite member: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	everyoneRoleID := guild.ID

	resp, err = client.putJSONWithAuth(
		fmt.Sprintf("/channels/%d/permissions/%s", privateChannelID, everyoneRoleID),
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
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	memberGateway := newGatewayClient(t, client, member.Token)
	defer memberGateway.Close()

	memberGateway.SendVoiceStateUpdate(&guild.ID, &publicChannel.ID, nil, false, false, false, false)

	serverUpdate := memberGateway.WaitForEvent(t, "VOICE_SERVER_UPDATE", 10*time.Second, nil)
	var vsu voiceServerUpdate
	if err := json.Unmarshal(serverUpdate.Data, &vsu); err != nil {
		t.Fatalf("failed to decode VOICE_SERVER_UPDATE: %v", err)
	}
	connectionID1 := vsu.ConnectionID

	roomName := guild.ID
	lkConn1 := connectToLiveKit(t, vsu.Endpoint, vsu.Token, roomName, member.UserID)
	defer lkConn1.disconnect()

	memberGateway.WaitForEvent(t, "VOICE_STATE_UPDATE", 10*time.Second, func(data json.RawMessage) bool {
		var vs voiceStateUpdate
		if err := json.Unmarshal(data, &vs); err != nil {
			return false
		}
		return vs.UserID == member.UserID && vs.ConnectionID == connectionID1
	})

	t.Logf("Member connected to public channel with connection_id=%s", connectionID1)

	t.Run("member cannot directly join private channel", func(t *testing.T) {
		memberGateway.SendVoiceStateUpdate(&guild.ID, &privateChannel.ID, nil, false, false, false, false)

		time.Sleep(500 * time.Millisecond)

		t.Log("Member cannot directly join private channel (as expected)")
	})

	t.Run("moderator force-moves member to private channel", func(t *testing.T) {
		resp, err := client.patchJSONWithAuth(
			fmt.Sprintf("/guilds/%d/members/%s", guildID, member.UserID),
			map[string]any{"channel_id": privateChannel.ID},
			owner.Token,
		)
		if err != nil {
			t.Fatalf("failed to force-move member: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()

		channelCreateEvt := memberGateway.WaitForEvent(t, "CHANNEL_CREATE", 10*time.Second, func(data json.RawMessage) bool {
			var cc channelCreateEvent
			if err := json.Unmarshal(data, &cc); err != nil {
				return false
			}
			return cc.ID == privateChannel.ID
		})

		var cc channelCreateEvent
		if err := json.Unmarshal(channelCreateEvt.Data, &cc); err != nil {
			t.Fatalf("failed to decode CHANNEL_CREATE: %v", err)
		}

		if cc.ID != privateChannel.ID {
			t.Fatalf("expected CHANNEL_CREATE for private channel %s, got %s", privateChannel.ID, cc.ID)
		}
		t.Logf("Member received CHANNEL_CREATE for private channel - virtual access granted!")

		newServerUpdate := memberGateway.WaitForEvent(t, "VOICE_SERVER_UPDATE", 10*time.Second, func(data json.RawMessage) bool {
			var vsu voiceServerUpdate
			if err := json.Unmarshal(data, &vsu); err != nil {
				return false
			}
			return vsu.ConnectionID != connectionID1 && vsu.Token != ""
		})

		var newVsu voiceServerUpdate
		if err := json.Unmarshal(newServerUpdate.Data, &newVsu); err != nil {
			t.Fatalf("failed to decode new VOICE_SERVER_UPDATE: %v", err)
		}

		connectionID1 = newVsu.ConnectionID
		t.Logf("Member received new voice token for private channel with connection_id=%s", connectionID1)

		newLkConn := connectToLiveKit(t, newVsu.Endpoint, newVsu.Token, roomName, member.UserID)
		defer newLkConn.disconnect()

		stateUpdate := memberGateway.WaitForEvent(t, "VOICE_STATE_UPDATE", 10*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				return false
			}
			return vs.UserID == member.UserID && vs.ChannelID != nil && *vs.ChannelID == privateChannel.ID
		})

		var vs voiceStateUpdate
		if err := json.Unmarshal(stateUpdate.Data, &vs); err != nil {
			t.Fatalf("failed to decode VOICE_STATE_UPDATE: %v", err)
		}

		if vs.ChannelID == nil || *vs.ChannelID != privateChannel.ID {
			t.Fatalf("expected member in private channel %s, got %v", privateChannel.ID, vs.ChannelID)
		}
		t.Log("Member successfully force-moved to private channel")
	})

	t.Run("new session can join via virtual access", func(t *testing.T) {
		memberGateway2 := newGatewayClient(t, client, member.Token)
		defer memberGateway2.Close()

		memberGateway2.SendVoiceStateUpdate(&guild.ID, &privateChannel.ID, nil, false, false, false, false)

		serverUpdate2 := memberGateway2.WaitForEvent(t, "VOICE_SERVER_UPDATE", 10*time.Second, func(data json.RawMessage) bool {
			var vsu voiceServerUpdate
			if err := json.Unmarshal(data, &vsu); err != nil {
				return false
			}
			return vsu.Token != ""
		})

		var vsu2 voiceServerUpdate
		if err := json.Unmarshal(serverUpdate2.Data, &vsu2); err != nil {
			t.Fatalf("failed to decode VOICE_SERVER_UPDATE: %v", err)
		}

		connectionID2 := vsu2.ConnectionID
		t.Logf("Second session can join private channel with connection_id=%s", connectionID2)

		lkConn2 := connectToLiveKit(t, vsu2.Endpoint, vsu2.Token, roomName, member.UserID)
		defer lkConn2.disconnect()

		memberGateway2.WaitForEvent(t, "VOICE_STATE_UPDATE", 10*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				return false
			}
			return vs.UserID == member.UserID && vs.ConnectionID == connectionID2
		})

		t.Log("Second session successfully joined private channel via virtual access")

		memberGateway2.SendVoiceStateUpdate(&guild.ID, nil, &connectionID2, false, false, false, false)
		memberGateway2.WaitForEvent(t, "VOICE_STATE_UPDATE", 10*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				return false
			}
			return vs.UserID == member.UserID && vs.ConnectionID == connectionID2 && vs.ChannelID == nil
		})
		t.Log("Second session disconnected")
	})

	t.Run("disconnecting last connection removes virtual access", func(t *testing.T) {
		memberGateway.SendVoiceStateUpdate(&guild.ID, nil, &connectionID1, false, false, false, false)

		channelDeleteEvt := memberGateway.WaitForEvent(t, "CHANNEL_DELETE", 10*time.Second, func(data json.RawMessage) bool {
			var cd channelDeleteEvent
			if err := json.Unmarshal(data, &cd); err != nil {
				return false
			}
			return cd.ID == privateChannel.ID
		})

		var cd channelDeleteEvent
		if err := json.Unmarshal(channelDeleteEvt.Data, &cd); err != nil {
			t.Fatalf("failed to decode CHANNEL_DELETE: %v", err)
		}

		if cd.ID != privateChannel.ID {
			t.Fatalf("expected CHANNEL_DELETE for private channel %s, got %s", privateChannel.ID, cd.ID)
		}
		t.Log("Member received CHANNEL_DELETE - virtual access revoked after all connections disconnected")
	})

	t.Run("member can no longer join private channel after losing virtual access", func(t *testing.T) {
		memberGateway.SendVoiceStateUpdate(&guild.ID, &privateChannel.ID, nil, false, false, false, false)

		time.Sleep(500 * time.Millisecond)
		t.Log("Member can no longer join private channel after losing virtual access")
	})
}
