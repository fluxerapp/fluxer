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

func TestMultiUserVoicePermissions(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	speaker := createTestAccount(t, client)
	listener := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, "Multi User Voice Permission Test")
	guildID := parseSnowflake(t, guild.ID)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), map[string]any{
		"name": "multi-user-voice",
		"type": 2,
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create voice channel: %v", err)
	}
	var voiceChannel minimalChannelResponse
	decodeJSONResponse(t, resp, &voiceChannel)
	channelID := parseSnowflake(t, voiceChannel.ID)

	speakerPerms := PermissionViewChannel | PermissionConnect | PermissionSpeak
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), map[string]any{
		"name":        "speaker",
		"permissions": fmt.Sprintf("%d", speakerPerms),
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create speaker role: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var speakerRole struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &speakerRole)

	listenerPerms := PermissionViewChannel | PermissionConnect
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), map[string]any{
		"name":        "listener",
		"permissions": fmt.Sprintf("%d", listenerPerms),
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create listener role: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var listenerRole struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &listenerRole)

	invite := createChannelInvite(t, client, owner.Token, channelID)

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, speaker.Token)
	if err != nil {
		t.Fatalf("failed to invite speaker: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, listener.Token)
	if err != nil {
		t.Fatalf("failed to invite listener: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.putWithAuth(fmt.Sprintf("/guilds/%d/members/%s/roles/%s", guildID, speaker.UserID, speakerRole.ID), owner.Token)
	if err != nil {
		t.Fatalf("failed to assign speaker role: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.putWithAuth(fmt.Sprintf("/guilds/%d/members/%s/roles/%s", guildID, listener.UserID, listenerRole.ID), owner.Token)
	if err != nil {
		t.Fatalf("failed to assign listener role: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	t.Run("speaker can connect with speak permission", func(t *testing.T) {
		gatewayClient := newGatewayClient(t, client, speaker.Token)
		defer gatewayClient.Close()

		gatewayClient.SendVoiceStateUpdate(&guild.ID, &voiceChannel.ID, nil, false, false, false, false)

		serverUpdate := gatewayClient.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, nil)
		var vsu voiceServerUpdate
		if err := json.Unmarshal(serverUpdate.Data, &vsu); err != nil {
			t.Fatalf("failed to decode VOICE_SERVER_UPDATE: %v", err)
		}

		if vsu.Token == "" {
			t.Fatal("expected speaker to receive voice token")
		}
		if vsu.ConnectionID == "" {
			t.Fatal("expected non-empty connection_id for speaker")
		}

		t.Logf("Speaker connected successfully with token and connection_id=%s", vsu.ConnectionID)

		gatewayClient.SendVoiceStateUpdate(&guild.ID, nil, &vsu.ConnectionID, false, false, false, false)
	})

	t.Run("listener can connect without speak permission", func(t *testing.T) {
		gatewayClient := newGatewayClient(t, client, listener.Token)
		defer gatewayClient.Close()

		gatewayClient.SendVoiceStateUpdate(&guild.ID, &voiceChannel.ID, nil, false, false, false, false)

		serverUpdate := gatewayClient.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, nil)
		var vsu voiceServerUpdate
		if err := json.Unmarshal(serverUpdate.Data, &vsu); err != nil {
			t.Fatalf("failed to decode VOICE_SERVER_UPDATE: %v", err)
		}

		if vsu.Token == "" {
			t.Fatal("expected listener to receive voice token (even without speak)")
		}
		if vsu.ConnectionID == "" {
			t.Fatal("expected non-empty connection_id for listener")
		}

		t.Logf("Listener connected successfully (with limited permissions) with connection_id=%s", vsu.ConnectionID)

		gatewayClient.SendVoiceStateUpdate(&guild.ID, nil, &vsu.ConnectionID, false, false, false, false)
	})
}
