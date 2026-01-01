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

func TestDMCallCreation(t *testing.T) {
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

	t.Run("user1 initiates call and both receive CALL_CREATE", func(t *testing.T) {
		ringCall(t, client, user1.Token, parseSnowflake(t, dm.ID), nil)

		msgEvent1 := gateway1.WaitForEvent(t, "MESSAGE_CREATE", 5*time.Second, func(data json.RawMessage) bool {
			var msg callMessageResponse
			if err := json.Unmarshal(data, &msg); err != nil {
				return false
			}
			return msg.Type == 3 && msg.ChannelID == dm.ID
		})

		var msg1 callMessageResponse
		if err := json.Unmarshal(msgEvent1.Data, &msg1); err != nil {
			t.Fatalf("failed to decode MESSAGE_CREATE: %v", err)
		}

		if msg1.Type != 3 {
			t.Fatalf("expected message type 3 (CALL), got %d", msg1.Type)
		}
		if msg1.ChannelID != dm.ID {
			t.Fatalf("expected channel_id %s, got %s", dm.ID, msg1.ChannelID)
		}

		callEvent1 := gateway1.WaitForEvent(t, "CALL_CREATE", 5*time.Second, func(data json.RawMessage) bool {
			var call callCreateEvent
			if err := json.Unmarshal(data, &call); err != nil {
				return false
			}
			return call.ChannelID == dm.ID
		})

		var call1 callCreateEvent
		if err := json.Unmarshal(callEvent1.Data, &call1); err != nil {
			t.Fatalf("failed to decode CALL_CREATE: %v", err)
		}

		if call1.ChannelID != dm.ID {
			t.Fatalf("expected channel_id %s, got %s", dm.ID, call1.ChannelID)
		}
		if call1.MessageID == "" {
			t.Fatal("expected message_id to be set")
		}
		if call1.Region == "" {
			t.Fatal("expected region to be set")
		}
		if len(call1.VoiceStates) != 0 {
			t.Fatalf("expected voice_states to be empty, got %d states", len(call1.VoiceStates))
		}

		t.Logf("User1 received CALL_CREATE: channel_id=%s, message_id=%s, region=%s",
			call1.ChannelID, call1.MessageID, call1.Region)

		callEvent2 := gateway2.WaitForEvent(t, "CALL_CREATE", 5*time.Second, func(data json.RawMessage) bool {
			var call callCreateEvent
			if err := json.Unmarshal(data, &call); err != nil {
				return false
			}
			return call.ChannelID == dm.ID
		})

		var call2 callCreateEvent
		if err := json.Unmarshal(callEvent2.Data, &call2); err != nil {
			t.Fatalf("failed to decode user2 CALL_CREATE: %v", err)
		}

		if call2.ChannelID != dm.ID {
			t.Fatalf("user2: expected channel_id %s, got %s", dm.ID, call2.ChannelID)
		}

		callUpdateEventMsg := gateway1.WaitForEvent(t, "CALL_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var call callUpdateEvent
			if err := json.Unmarshal(data, &call); err != nil {
				return false
			}
			return call.ChannelID == dm.ID && containsString(call.Ringing, user2.UserID)
		})

		var update callUpdateEvent
		if err := json.Unmarshal(callUpdateEventMsg.Data, &update); err != nil {
			t.Fatalf("failed to decode CALL_UPDATE: %v", err)
		}

		if len(update.VoiceStates) != 0 {
			t.Fatalf("expected voice_states to remain empty, got %d states", len(update.VoiceStates))
		}

		t.Logf("CALL_UPDATE after confirm has ringing=%v, voice_states=%d", update.Ringing, len(update.VoiceStates))
	})
}
