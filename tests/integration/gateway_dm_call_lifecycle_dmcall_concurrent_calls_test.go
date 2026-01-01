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

func TestDMCallConcurrentCalls(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)
	user3 := createTestAccount(t, client)

	guild := createGuild(t, client, user1.Token, "Test Guild")
	invite := createChannelInvite(t, client, user1.Token, parseSnowflake(t, guild.SystemChannel))
	joinGuild(t, client, user2.Token, invite.Code)
	joinGuild(t, client, user3.Token, invite.Code)

	dm12 := createDmChannel(t, client, user1.Token, parseSnowflake(t, user2.UserID))
	dm13 := createDmChannel(t, client, user1.Token, parseSnowflake(t, user3.UserID))

	gateway1 := newGatewayClient(t, client, user1.Token)
	defer gateway1.Close()

	gateway2 := newGatewayClient(t, client, user2.Token)
	defer gateway2.Close()

	gateway3 := newGatewayClient(t, client, user3.Token)
	defer gateway3.Close()

	t.Run("concurrent calls in different DM channels operate independently", func(t *testing.T) {
		ringCall(t, client, user1.Token, parseSnowflake(t, dm12.ID), nil)

		callCreate12 := gateway1.WaitForEvent(t, "CALL_CREATE", 5*time.Second, func(data json.RawMessage) bool {
			var call callCreateEvent
			if err := json.Unmarshal(data, &call); err != nil {
				return false
			}
			return call.ChannelID == dm12.ID
		})

		var call12 callCreateEvent
		if err := json.Unmarshal(callCreate12.Data, &call12); err != nil {
			t.Fatalf("failed to decode CALL_CREATE for dm12: %v", err)
		}

		t.Logf("Call created in dm12 (channel_id=%s)", call12.ChannelID)

		gateway2.WaitForEvent(t, "CALL_CREATE", 5*time.Second, func(data json.RawMessage) bool {
			var call callCreateEvent
			if err := json.Unmarshal(data, &call); err != nil {
				return false
			}
			return call.ChannelID == dm12.ID
		})

		ringCall(t, client, user3.Token, parseSnowflake(t, dm13.ID), nil)

		callCreate13 := gateway3.WaitForEvent(t, "CALL_CREATE", 5*time.Second, func(data json.RawMessage) bool {
			var call callCreateEvent
			if err := json.Unmarshal(data, &call); err != nil {
				return false
			}
			return call.ChannelID == dm13.ID
		})

		var call13 callCreateEvent
		if err := json.Unmarshal(callCreate13.Data, &call13); err != nil {
			t.Fatalf("failed to decode CALL_CREATE for dm13: %v", err)
		}

		t.Logf("Call created in dm13 (channel_id=%s)", call13.ChannelID)

		gateway1.WaitForEvent(t, "CALL_CREATE", 5*time.Second, func(data json.RawMessage) bool {
			var call callCreateEvent
			if err := json.Unmarshal(data, &call); err != nil {
				return false
			}
			return call.ChannelID == dm13.ID
		})

		if call12.ChannelID == call13.ChannelID {
			t.Fatal("expected different channel_ids for the two calls")
		}
		if call12.MessageID == call13.MessageID {
			t.Fatal("expected different message_ids for the two calls")
		}

		t.Log("Both calls have independent channel_id and message_id")

		gateway2.SendVoiceStateUpdate(nil, &dm12.ID, nil, false, false, false, false)
		serverUpdate2 := gateway2.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, nil)
		var vsu2 voiceServerUpdate
		if err := json.Unmarshal(serverUpdate2.Data, &vsu2); err != nil {
			t.Fatalf("failed to decode VOICE_SERVER_UPDATE for user2: %v", err)
		}
		connectionID2 := vsu2.ConnectionID

		lkConn2 := connectToLiveKit(t, vsu2.Endpoint, vsu2.Token, dm12.ID, user2.UserID)
		defer lkConn2.disconnect()

		time.Sleep(2 * time.Second)

		gateway3.SendVoiceStateUpdate(nil, &dm13.ID, nil, false, false, false, false)
		serverUpdate3 := gateway3.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, nil)
		var vsu3 voiceServerUpdate
		if err := json.Unmarshal(serverUpdate3.Data, &vsu3); err != nil {
			t.Fatalf("failed to decode VOICE_SERVER_UPDATE for user3: %v", err)
		}
		connectionID3 := vsu3.ConnectionID

		lkConn3 := connectToLiveKit(t, vsu3.Endpoint, vsu3.Token, dm13.ID, user3.UserID)
		defer lkConn3.disconnect()

		time.Sleep(500 * time.Millisecond)

		callUpdate12 := gateway2.WaitForEvent(t, "CALL_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var call callUpdateEvent
			if err := json.Unmarshal(data, &call); err != nil {
				return false
			}
			return call.ChannelID == dm12.ID && len(call.VoiceStates) >= 1
		})

		var update12 callUpdateEvent
		if err := json.Unmarshal(callUpdate12.Data, &update12); err != nil {
			t.Fatalf("failed to decode CALL_UPDATE for dm12: %v", err)
		}

		user2InCall12 := false
		for _, vs := range update12.VoiceStates {
			if vs.UserID == user2.UserID {
				user2InCall12 = true
				break
			}
		}
		if !user2InCall12 {
			t.Fatal("expected user2 in dm12 call voice_states")
		}

		t.Log("User2 is in dm12 call")

		callUpdate13 := gateway3.WaitForEvent(t, "CALL_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var call callUpdateEvent
			if err := json.Unmarshal(data, &call); err != nil {
				return false
			}
			return call.ChannelID == dm13.ID && len(call.VoiceStates) >= 1
		})

		var update13 callUpdateEvent
		if err := json.Unmarshal(callUpdate13.Data, &update13); err != nil {
			t.Fatalf("failed to decode CALL_UPDATE for dm13: %v", err)
		}

		user3InCall13 := false
		for _, vs := range update13.VoiceStates {
			if vs.UserID == user3.UserID {
				user3InCall13 = true
				break
			}
		}
		if !user3InCall13 {
			t.Fatal("expected user3 in dm13 call voice_states")
		}

		t.Log("User3 is in dm13 call")

		gateway2.SendVoiceStateUpdate(nil, nil, &connectionID2, false, false, false, false)

		gateway2.WaitForEvent(t, "CALL_DELETE", 5*time.Second, func(data json.RawMessage) bool {
			var call callDeleteEvent
			if err := json.Unmarshal(data, &call); err != nil {
				return false
			}
			return call.ChannelID == dm12.ID
		})

		t.Log("dm12 call ended")

		time.Sleep(300 * time.Millisecond)

		gateway3.SendVoiceStateUpdate(nil, nil, &connectionID3, false, false, false, false)

		gateway3.WaitForEvent(t, "CALL_DELETE", 5*time.Second, func(data json.RawMessage) bool {
			var call callDeleteEvent
			if err := json.Unmarshal(data, &call); err != nil {
				return false
			}
			return call.ChannelID == dm13.ID
		})

		t.Log("dm13 call ended")
		t.Log("Both concurrent calls operated independently and ended correctly")
	})
}
