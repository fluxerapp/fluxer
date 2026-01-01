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

func TestVoiceStateChannelMoveAndRtcRegionChange(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, "Voice Region Move Test")
	guildID := parseSnowflake(t, guild.ID)

	createVoiceChannel := func(name string) minimalChannelResponse {
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), map[string]any{
			"name": name,
			"type": 2,
		}, owner.Token)
		if err != nil {
			t.Fatalf("failed to create voice channel %s: %v", name, err)
		}
		var channel minimalChannelResponse
		decodeJSONResponse(t, resp, &channel)
		return channel
	}

	voiceChannelA := createVoiceChannel("alpha-voice")
	voiceChannelB := createVoiceChannel("beta-voice")

	gatewayClient := newGatewayClient(t, client, owner.Token)
	defer gatewayClient.Close()

	waitForVoiceState := func(expectedConnID, expectedChannelID string, expectVideo, expectStream bool) voiceStateUpdate {
		t.Helper()
		event := gatewayClient.WaitForEvent(t, "VOICE_STATE_UPDATE", 10*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				return false
			}
			if vs.UserID != owner.UserID {
				return false
			}
			if vs.ConnectionID != expectedConnID {
				return false
			}
			if vs.ChannelID == nil || *vs.ChannelID != expectedChannelID {
				return false
			}
			return true
		})

		var vs voiceStateUpdate
		if err := json.Unmarshal(event.Data, &vs); err != nil {
			t.Fatalf("failed to decode VOICE_STATE_UPDATE: %v", err)
		}
		if vs.SelfVideo != expectVideo {
			t.Fatalf("expected self_video=%t, got %t", expectVideo, vs.SelfVideo)
		}
		if vs.SelfStream != expectStream {
			t.Fatalf("expected self_stream=%t, got %t", expectStream, vs.SelfStream)
		}
		return vs
	}

	gatewayClient.SendVoiceStateUpdate(&guild.ID, &voiceChannelA.ID, nil, false, false, true, true)

	serverUpdate := gatewayClient.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, nil)
	var vsu voiceServerUpdate
	if err := json.Unmarshal(serverUpdate.Data, &vsu); err != nil {
		t.Fatalf("failed to decode VOICE_SERVER_UPDATE: %v", err)
	}
	if vsu.ConnectionID == "" {
		t.Fatal("expected non-empty connection id when joining initial channel")
	}
	connectionID := vsu.ConnectionID

	waitForVoiceState(connectionID, voiceChannelA.ID, true, true)

	gatewayClient.SendVoiceStateUpdate(&guild.ID, &voiceChannelB.ID, &connectionID, false, false, true, true)
	waitForVoiceState(connectionID, voiceChannelB.ID, true, true)

	gatewayClient.SendVoiceStateUpdate(&guild.ID, &voiceChannelB.ID, &connectionID, false, false, true, false)
	waitForVoiceState(connectionID, voiceChannelB.ID, true, false)

	channelBID := parseSnowflake(t, voiceChannelB.ID)
	regionsResp, err := client.getWithAuth(fmt.Sprintf("/channels/%d/rtc-regions", channelBID), owner.Token)
	if err != nil {
		t.Fatalf("failed to fetch rtc regions: %v", err)
	}
	assertStatus(t, regionsResp, http.StatusOK)
	var regions []struct {
		ID   string `json:"id"`
		Name string `json:"name"`
	}
	decodeJSONResponse(t, regionsResp, &regions)
	regionsResp.Body.Close()
	if len(regions) == 0 {
		t.Fatal("expected at least one rtc region availability")
	}

	targetRegion := regions[0].ID
	updatePayload := map[string]any{"rtc_region": targetRegion}
	regionResp, err := client.patchJSONWithAuth(fmt.Sprintf("/channels/%d", channelBID), updatePayload, owner.Token)
	if err != nil {
		t.Fatalf("failed to update rtc region: %v", err)
	}
	assertStatus(t, regionResp, http.StatusOK)
	regionResp.Body.Close()

	regionUpdate := gatewayClient.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
		var evt voiceServerUpdate
		if err := json.Unmarshal(data, &evt); err != nil {
			return false
		}
		if evt.GuildID == nil || *evt.GuildID != guild.ID {
			return false
		}
		return evt.Token != ""
	})

	var regionVSU voiceServerUpdate
	if err := json.Unmarshal(regionUpdate.Data, &regionVSU); err != nil {
		t.Fatalf("failed to decode region VOICE_SERVER_UPDATE: %v", err)
	}
	if regionVSU.ConnectionID == "" {
		t.Fatal("expected region update to include connection id")
	}
	newConnectionID := regionVSU.ConnectionID

	roomName := guild.ID
	lkConn := connectToLiveKit(t, regionVSU.Endpoint, regionVSU.Token, roomName, owner.UserID)
	defer lkConn.disconnect()

	waitForVoiceState(newConnectionID, voiceChannelB.ID, true, false)

	gatewayClient.SendVoiceStateUpdate(&guild.ID, &voiceChannelB.ID, &newConnectionID, false, false, false, false)
	waitForVoiceState(newConnectionID, voiceChannelB.ID, false, false)

	gatewayClient.SendVoiceStateUpdate(&guild.ID, nil, &newConnectionID, false, false, false, false)
	disconnectEvent := gatewayClient.WaitForEvent(t, "VOICE_STATE_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
		var vs voiceStateUpdate
		if err := json.Unmarshal(data, &vs); err != nil {
			return false
		}
		return vs.UserID == owner.UserID && vs.ConnectionID == newConnectionID && vs.ChannelID == nil
	})
	if disconnectEvent.Type != "VOICE_STATE_UPDATE" {
		t.Fatalf("expected VOICE_STATE_UPDATE event for disconnect, got %s", disconnectEvent.Type)
	}
}
