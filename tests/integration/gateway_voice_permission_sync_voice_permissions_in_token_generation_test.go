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

func TestVoicePermissionsInTokenGeneration(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, "Voice Permission Test Guild")
	guildID := parseSnowflake(t, guild.ID)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), map[string]any{
		"name": "voice-perms-channel",
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

	noVoicePerms := PermissionViewChannel | PermissionConnect
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), map[string]any{
		"name":        "no-voice-perms",
		"permissions": fmt.Sprintf("%d", noVoicePerms),
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create role: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var restrictedRole struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &restrictedRole)

	fullVoicePerms := PermissionViewChannel | PermissionConnect | PermissionSpeak | PermissionStream
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), map[string]any{
		"name":        "full-voice-perms",
		"permissions": fmt.Sprintf("%d", fullVoicePerms),
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create full voice role: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var fullRole struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &fullRole)

	t.Run("owner has full voice permissions", func(t *testing.T) {
		gatewayClient := newGatewayClient(t, client, owner.Token)
		defer gatewayClient.Close()

		gatewayClient.SendVoiceStateUpdate(&guild.ID, &voiceChannel.ID, nil, false, false, false, false)

		serverUpdate := gatewayClient.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, nil)
		var vsu voiceServerUpdate
		if err := json.Unmarshal(serverUpdate.Data, &vsu); err != nil {
			t.Fatalf("failed to decode VOICE_SERVER_UPDATE: %v", err)
		}

		if vsu.Token == "" {
			t.Fatal("expected non-empty token for owner")
		}
		if vsu.ConnectionID == "" {
			t.Fatal("expected non-empty connection_id")
		}

		t.Logf("Owner received voice token successfully with connection_id=%s", vsu.ConnectionID)

		gatewayClient.SendVoiceStateUpdate(&guild.ID, nil, &vsu.ConnectionID, false, false, false, false)
	})

	t.Run("member with no speak permission can still connect", func(t *testing.T) {
		resp, err = client.putWithAuth(fmt.Sprintf("/guilds/%d/members/%s/roles/%s", guildID, member.UserID, restrictedRole.ID), owner.Token)
		if err != nil {
			t.Fatalf("failed to assign role: %v", err)
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
			t.Fatal("expected non-empty token for member with connect permission")
		}
		if vsu.ConnectionID == "" {
			t.Fatal("expected non-empty connection_id for member")
		}

		t.Logf("Member with no SPEAK permission received voice token with connection_id=%s", vsu.ConnectionID)

		gatewayClient.SendVoiceStateUpdate(&guild.ID, nil, &vsu.ConnectionID, false, false, false, false)
	})
}
