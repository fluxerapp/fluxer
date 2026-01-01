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

func TestVoicePermissionChannelOverwrite(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, "Voice Overwrite Test Guild")
	guildID := parseSnowflake(t, guild.ID)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), map[string]any{
		"name": "overwrite-test-channel",
		"type": 2,
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create voice channel: %v", err)
	}
	var voiceChannel minimalChannelResponse
	decodeJSONResponse(t, resp, &voiceChannel)
	channelID := parseSnowflake(t, voiceChannel.ID)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to invite member: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	fullVoicePerms := PermissionViewChannel | PermissionConnect | PermissionSpeak | PermissionStream
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), map[string]any{
		"name":        "voice-role",
		"permissions": fmt.Sprintf("%d", fullVoicePerms),
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create role: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var voiceRole struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &voiceRole)

	resp, err = client.putWithAuth(fmt.Sprintf("/guilds/%d/members/%s/roles/%s", guildID, member.UserID, voiceRole.ID), owner.Token)
	if err != nil {
		t.Fatalf("failed to assign role: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	t.Run("channel overwrite denies stream permission", func(t *testing.T) {
		resp, err = client.requestJSON(http.MethodPut, fmt.Sprintf("/channels/%d/permissions/%s", channelID, voiceRole.ID), map[string]any{
			"type":  0,
			"allow": "0",
			"deny":  fmt.Sprintf("%d", PermissionStream),
		}, owner.Token)
		if err != nil {
			t.Fatalf("failed to add channel overwrite: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()

		gatewayClient := newGatewayClient(t, client, member.Token)
		defer gatewayClient.Close()

		gatewayClient.SendVoiceStateUpdate(&guild.ID, &voiceChannel.ID, nil, false, false, false, false)

		serverUpdate := gatewayClient.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, nil)
		var vsu voiceServerUpdate
		if err := json.Unmarshal(serverUpdate.Data, &vsu); err != nil {
			t.Fatalf("failed to decode VOICE_SERVER_UPDATE: %v", err)
		}

		if vsu.Token == "" {
			t.Fatal("expected non-empty token")
		}
		if vsu.ConnectionID == "" {
			t.Fatal("expected non-empty connection_id")
		}

		t.Logf("Member with channel-denied STREAM received voice token with connection_id=%s", vsu.ConnectionID)

		gatewayClient.SendVoiceStateUpdate(&guild.ID, nil, &vsu.ConnectionID, false, false, false, false)
	})

	t.Run("remove overwrite restores permissions", func(t *testing.T) {
		resp, err = client.delete(fmt.Sprintf("/channels/%d/permissions/%s", channelID, voiceRole.ID), owner.Token)
		if err != nil {
			t.Fatalf("failed to delete channel overwrite: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()

		gatewayClient := newGatewayClient(t, client, member.Token)
		defer gatewayClient.Close()

		gatewayClient.SendVoiceStateUpdate(&guild.ID, &voiceChannel.ID, nil, false, false, false, false)

		serverUpdate := gatewayClient.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, nil)
		var vsu voiceServerUpdate
		if err := json.Unmarshal(serverUpdate.Data, &vsu); err != nil {
			t.Fatalf("failed to decode VOICE_SERVER_UPDATE: %v", err)
		}

		if vsu.Token == "" {
			t.Fatal("expected non-empty token after overwrite removal")
		}
		if vsu.ConnectionID == "" {
			t.Fatal("expected non-empty connection_id after overwrite removal")
		}

		t.Logf("Member with restored permissions received voice token with connection_id=%s", vsu.ConnectionID)

		gatewayClient.SendVoiceStateUpdate(&guild.ID, nil, &vsu.ConnectionID, false, false, false, false)
	})
}
