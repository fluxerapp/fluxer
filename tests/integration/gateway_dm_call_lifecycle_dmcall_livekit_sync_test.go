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
	"testing"
	"time"
)

func TestDMCallLiveKitSync(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)

	guild := createGuild(t, client, user1.Token, "Test Guild")
	invite := createChannelInvite(t, client, user1.Token, parseSnowflake(t, guild.SystemChannel))
	joinGuild(t, client, user2.Token, invite.Code)

	dm := createDmChannel(t, client, user1.Token, parseSnowflake(t, user2.UserID))

	gateway1 := newGatewayClient(t, client, user1.Token)
	defer gateway1.Close()

	t.Run("voice state reflects LiveKit connection state", func(t *testing.T) {
		ringCall(t, client, user1.Token, parseSnowflake(t, dm.ID), nil)

		callCreateEvt := gateway1.WaitForEvent(t, "CALL_CREATE", 5*time.Second, nil)
		var callCreate callCreateEvent
		if err := json.Unmarshal(callCreateEvt.Data, &callCreate); err != nil {
			t.Fatalf("failed to decode CALL_CREATE: %v", err)
		}

		if len(callCreate.VoiceStates) != 0 {
			t.Fatalf("expected no voice_states at call creation, got %d", len(callCreate.VoiceStates))
		}

		t.Log("Call created with empty voice_states")

		gateway1.SendVoiceStateUpdate(nil, &dm.ID, nil, false, false, false, false)

		serverUpdate := gateway1.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, nil)
		var vsu voiceServerUpdate
		if err := json.Unmarshal(serverUpdate.Data, &vsu); err != nil {
			t.Fatalf("failed to decode VOICE_SERVER_UPDATE: %v", err)
		}

		if vsu.Endpoint == "" {
			t.Fatal("expected LiveKit endpoint in VOICE_SERVER_UPDATE")
		}
		if vsu.Token == "" {
			t.Fatal("expected LiveKit token in VOICE_SERVER_UPDATE")
		}
		if vsu.ConnectionID == "" {
			t.Fatal("expected connection_id in VOICE_SERVER_UPDATE")
		}

		t.Logf("VOICE_SERVER_UPDATE received: endpoint=%s, connection_id=%s", vsu.Endpoint, vsu.ConnectionID)
		connectionID := vsu.ConnectionID

		lkConn := connectToLiveKit(t, vsu.Endpoint, vsu.Token, dm.ID, user1.UserID)
		defer lkConn.disconnect()

		vsEvt := gateway1.WaitForEvent(t, "VOICE_STATE_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				return false
			}
			return vs.UserID == user1.UserID && vs.ConnectionID == connectionID
		})

		var voiceState voiceStateUpdate
		if err := json.Unmarshal(vsEvt.Data, &voiceState); err != nil {
			t.Fatalf("failed to decode VOICE_STATE_UPDATE: %v", err)
		}

		if voiceState.ChannelID == nil || *voiceState.ChannelID != dm.ID {
			t.Fatalf("expected channel_id %s, got %v", dm.ID, voiceState.ChannelID)
		}
		if voiceState.UserID != user1.UserID {
			t.Fatalf("expected user_id %s, got %s", user1.UserID, voiceState.UserID)
		}
		if voiceState.ConnectionID != connectionID {
			t.Fatalf("expected connection_id %s, got %s", connectionID, voiceState.ConnectionID)
		}
		if voiceState.GuildID != nil {
			t.Fatalf("expected guild_id to be nil for DM call, got %s", *voiceState.GuildID)
		}

		t.Logf("Voice state synced correctly: channel_id=%s, user_id=%s, guild_id=%v",
			*voiceState.ChannelID, voiceState.UserID, voiceState.GuildID)

		callUpdateEvt := gateway1.WaitForEvent(t, "CALL_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var call callUpdateEvent
			if err := json.Unmarshal(data, &call); err != nil {
				return false
			}
			return call.ChannelID == dm.ID && len(call.VoiceStates) == 1
		})

		var callUpdate callUpdateEvent
		if err := json.Unmarshal(callUpdateEvt.Data, &callUpdate); err != nil {
			t.Fatalf("failed to decode CALL_UPDATE: %v", err)
		}

		if len(callUpdate.VoiceStates) != 1 {
			t.Fatalf("expected 1 voice state in CALL_UPDATE, got %d", len(callUpdate.VoiceStates))
		}

		vs := callUpdate.VoiceStates[0]
		if vs.UserID != user1.UserID {
			t.Fatalf("expected user_id %s in CALL_UPDATE voice_states, got %s", user1.UserID, vs.UserID)
		}

		t.Log("CALL_UPDATE contains synced voice state from LiveKit")

		gateway1.SendVoiceStateUpdate(nil, nil, &connectionID, false, false, false, false)
		gateway1.WaitForEvent(t, "CALL_DELETE", 5*time.Second, nil)

		t.Log("Call properly deleted after LiveKit disconnect")
	})
}
