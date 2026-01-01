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

func TestVoiceWithVideoAndStreamPermissions(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, "Video Stream Permission Test")
	guildID := parseSnowflake(t, guild.ID)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), map[string]any{
		"name": "video-stream-channel",
		"type": 2,
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create voice channel: %v", err)
	}
	var voiceChannel minimalChannelResponse
	decodeJSONResponse(t, resp, &voiceChannel)

	gatewayClient := newGatewayClient(t, client, owner.Token)
	defer gatewayClient.Close()

	var connectionID string

	t.Run("join with video and stream enabled", func(t *testing.T) {
		gatewayClient.SendVoiceStateUpdate(&guild.ID, &voiceChannel.ID, nil, false, false, true, true)

		serverUpdate := gatewayClient.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, nil)
		var vsu voiceServerUpdate
		if err := json.Unmarshal(serverUpdate.Data, &vsu); err != nil {
			t.Fatalf("failed to decode VOICE_SERVER_UPDATE: %v", err)
		}
		if vsu.Token == "" {
			t.Fatal("expected non-empty token for owner")
		}
		if vsu.ConnectionID == "" {
			t.Fatal("expected non-empty connection_id for owner")
		}
		connectionID = vsu.ConnectionID

		stateUpdate := gatewayClient.WaitForEvent(t, "VOICE_STATE_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				return false
			}
			return vs.UserID == owner.UserID && vs.ConnectionID == connectionID && vs.SelfVideo && vs.SelfStream
		})

		var vs voiceStateUpdate
		if err := json.Unmarshal(stateUpdate.Data, &vs); err != nil {
			t.Fatalf("failed to decode VOICE_STATE_UPDATE: %v", err)
		}

		if !vs.SelfVideo {
			t.Fatal("expected self_video to be true")
		}
		if !vs.SelfStream {
			t.Fatal("expected self_stream to be true")
		}

		t.Logf("Owner connected with video=%t and stream=%t", vs.SelfVideo, vs.SelfStream)
	})

	t.Run("toggle stream off while maintaining video", func(t *testing.T) {
		gatewayClient.SendVoiceStateUpdate(&guild.ID, &voiceChannel.ID, &connectionID, false, false, true, false)

		stateUpdate := gatewayClient.WaitForEvent(t, "VOICE_STATE_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				return false
			}
			return vs.UserID == owner.UserID && vs.ConnectionID == connectionID && !vs.SelfStream
		})

		var vs voiceStateUpdate
		if err := json.Unmarshal(stateUpdate.Data, &vs); err != nil {
			t.Fatalf("failed to decode VOICE_STATE_UPDATE: %v", err)
		}

		if !vs.SelfVideo {
			t.Fatal("expected self_video to remain true")
		}
		if vs.SelfStream {
			t.Fatal("expected self_stream to be false")
		}
	})

	t.Run("disconnect", func(t *testing.T) {
		gatewayClient.SendVoiceStateUpdate(&guild.ID, nil, &connectionID, false, false, false, false)

		stateUpdate := gatewayClient.WaitForEvent(t, "VOICE_STATE_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				return false
			}
			return vs.UserID == owner.UserID && vs.ChannelID == nil
		})

		var vs voiceStateUpdate
		if err := json.Unmarshal(stateUpdate.Data, &vs); err != nil {
			t.Fatalf("failed to decode VOICE_STATE_UPDATE: %v", err)
		}

		if vs.ChannelID != nil {
			t.Fatalf("expected channel_id to be null after disconnect")
		}
	})
}
