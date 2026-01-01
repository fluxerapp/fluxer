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

func TestDMCallEventsBroadcastToAllRecipients(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)

	guild := createGuild(t, client, user1.Token, "Test Guild")
	invite := createChannelInvite(t, client, user1.Token, parseSnowflake(t, guild.SystemChannel))
	joinGuild(t, client, user2.Token, invite.Code)

	dm := createDmChannel(t, client, user1.Token, parseSnowflake(t, user2.UserID))

	gateway1 := newGatewayClient(t, client, user1.Token)
	defer gateway1.Close()

	gateway2 := newGatewayClient(t, client, user2.Token)
	defer gateway2.Close()

	t.Run("both users receive all call events", func(t *testing.T) {
		ringCall(t, client, user1.Token, parseSnowflake(t, dm.ID), nil)

		// Both should receive CALL_CREATE
		var call1, call2 callCreateEvent

		callEvent1 := gateway1.WaitForEvent(t, "CALL_CREATE", 5*time.Second, func(data json.RawMessage) bool {
			var call callCreateEvent
			if err := json.Unmarshal(data, &call); err != nil {
				t.Logf("failed to decode CALL_CREATE for user1: %v", err)
				return false
			}
			return call.ChannelID == dm.ID
		})
		if err := json.Unmarshal(callEvent1.Data, &call1); err != nil {
			t.Fatalf("failed to decode CALL_CREATE event for user1: %v", err)
		}

		callEvent2 := gateway2.WaitForEvent(t, "CALL_CREATE", 5*time.Second, func(data json.RawMessage) bool {
			var call callCreateEvent
			if err := json.Unmarshal(data, &call); err != nil {
				t.Logf("failed to decode CALL_CREATE for user2: %v", err)
				return false
			}
			return call.ChannelID == dm.ID
		})
		if err := json.Unmarshal(callEvent2.Data, &call2); err != nil {
			t.Fatalf("failed to decode CALL_CREATE event for user2: %v", err)
		}

		if call1.ChannelID != call2.ChannelID {
			t.Fatalf("channel_id mismatch: user1 got %s, user2 got %s", call1.ChannelID, call2.ChannelID)
		}
		if call1.MessageID != call2.MessageID {
			t.Fatalf("message_id mismatch: user1 got %s, user2 got %s", call1.MessageID, call2.MessageID)
		}

		t.Log("Both users received matching CALL_CREATE events")

		gateway1.SendVoiceStateUpdate(nil, &dm.ID, nil, false, false, false, false)
		serverUpdate := gateway1.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, nil)
		var vsu voiceServerUpdate
		if err := json.Unmarshal(serverUpdate.Data, &vsu); err != nil {
			t.Fatalf("failed to decode VOICE_SERVER_UPDATE: %v", err)
		}
		connectionID := vsu.ConnectionID

		lkConn := connectToLiveKit(t, vsu.Endpoint, vsu.Token, dm.ID, user1.UserID)
		defer lkConn.disconnect()

		// Both should receive CALL_UPDATE
		var update1, update2 callUpdateEvent

		updateEvent1 := gateway1.WaitForEvent(t, "CALL_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var call callUpdateEvent
			if err := json.Unmarshal(data, &call); err != nil {
				t.Logf("failed to decode CALL_UPDATE for user1: %v", err)
				return false
			}
			for _, vs := range call.VoiceStates {
				if vs.UserID == user1.UserID {
					return true
				}
			}
			return false
		})
		if err := json.Unmarshal(updateEvent1.Data, &update1); err != nil {
			t.Fatalf("failed to decode CALL_UPDATE event for user1: %v", err)
		}

		updateEvent2 := gateway2.WaitForEvent(t, "CALL_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var call callUpdateEvent
			if err := json.Unmarshal(data, &call); err != nil {
				t.Logf("failed to decode CALL_UPDATE for user2: %v", err)
				return false
			}
			for _, vs := range call.VoiceStates {
				if vs.UserID == user1.UserID {
					return true
				}
			}
			return false
		})
		if err := json.Unmarshal(updateEvent2.Data, &update2); err != nil {
			t.Fatalf("failed to decode CALL_UPDATE event for user2: %v", err)
		}

		if len(update1.VoiceStates) != len(update2.VoiceStates) {
			t.Fatalf("voice_states count mismatch: user1 got %d, user2 got %d",
				len(update1.VoiceStates), len(update2.VoiceStates))
		}

		t.Log("Both users received matching CALL_UPDATE events")

		gateway1.SendVoiceStateUpdate(nil, nil, &connectionID, false, false, false, false)

		deleteEvent1 := gateway1.WaitForEvent(t, "CALL_DELETE", 5*time.Second, func(data json.RawMessage) bool {
			var call callDeleteEvent
			if err := json.Unmarshal(data, &call); err != nil {
				t.Logf("failed to decode CALL_DELETE for user1: %v", err)
				return false
			}
			return call.ChannelID == dm.ID
		})

		deleteEvent2 := gateway2.WaitForEvent(t, "CALL_DELETE", 5*time.Second, func(data json.RawMessage) bool {
			var call callDeleteEvent
			if err := json.Unmarshal(data, &call); err != nil {
				t.Logf("failed to decode CALL_DELETE for user2: %v", err)
				return false
			}
			return call.ChannelID == dm.ID
		})

		var delete1, delete2 callDeleteEvent
		if err := json.Unmarshal(deleteEvent1.Data, &delete1); err != nil {
			t.Fatalf("failed to decode CALL_DELETE event for user1: %v", err)
		}
		if err := json.Unmarshal(deleteEvent2.Data, &delete2); err != nil {
			t.Fatalf("failed to decode CALL_DELETE event for user2: %v", err)
		}

		if delete1.ChannelID != delete2.ChannelID {
			t.Fatalf("CALL_DELETE channel_id mismatch: user1 got %s, user2 got %s",
				delete1.ChannelID, delete2.ChannelID)
		}

		t.Log("Both users received matching CALL_DELETE events")
	})
}
