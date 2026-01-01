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

func TestGroupDMCallMultipleParticipants(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)
	user3 := createTestAccount(t, client)

	guild := createGuild(t, client, user1.Token, "Test Guild")
	invite := createChannelInvite(t, client, user1.Token, parseSnowflake(t, guild.SystemChannel))
	joinGuild(t, client, user2.Token, invite.Code)
	joinGuild(t, client, user3.Token, invite.Code)

	createFriendship(t, client, user1, user2)
	createFriendship(t, client, user1, user3)

	groupDM := createGroupDmChannel(t, client, user1.Token, user2.UserID, user3.UserID)

	gateway1 := newGatewayClient(t, client, user1.Token)
	defer gateway1.Close()

	gateway2 := newGatewayClient(t, client, user2.Token)
	defer gateway2.Close()

	gateway3 := newGatewayClient(t, client, user3.Token)
	defer gateway3.Close()

	t.Run("group DM call with multiple participants", func(t *testing.T) {
		ringCall(t, client, user1.Token, parseSnowflake(t, groupDM.ID), nil)

		callEvent1 := gateway1.WaitForEvent(t, "CALL_CREATE", 5*time.Second, func(data json.RawMessage) bool {
			var call callCreateEvent
			if err := json.Unmarshal(data, &call); err != nil {
				return false
			}
			return call.ChannelID == groupDM.ID
		})

		var call1 callCreateEvent
		if err := json.Unmarshal(callEvent1.Data, &call1); err != nil {
			t.Fatalf("failed to decode CALL_CREATE event: %v", err)
		}

		gateway2.WaitForEvent(t, "CALL_CREATE", 5*time.Second, nil)
		gateway3.WaitForEvent(t, "CALL_CREATE", 5*time.Second, nil)

		initialUpdateMsg := gateway1.WaitForEvent(t, "CALL_UPDATE", 10*time.Second, func(data json.RawMessage) bool {
			var call callUpdateEvent
			if err := json.Unmarshal(data, &call); err != nil {
				return false
			}
			return call.ChannelID == groupDM.ID &&
				containsString(call.Ringing, user2.UserID) &&
				containsString(call.Ringing, user3.UserID)
		})

		var initialUpdate callUpdateEvent
		if err := json.Unmarshal(initialUpdateMsg.Data, &initialUpdate); err != nil {
			t.Fatalf("failed to decode CALL_UPDATE event: %v", err)
		}

		t.Logf("Initial ringing after call start: %v", initialUpdate.Ringing)

		gateway2.SendVoiceStateUpdate(nil, &groupDM.ID, nil, false, false, false, false)
		serverUpdate2 := gateway2.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, nil)
		var vsu2 voiceServerUpdate
		if err := json.Unmarshal(serverUpdate2.Data, &vsu2); err != nil {
			t.Fatalf("failed to decode VOICE_SERVER_UPDATE for user2: %v", err)
		}

		lkConn2 := connectToLiveKit(t, vsu2.Endpoint, vsu2.Token, groupDM.ID, user2.UserID)
		defer lkConn2.disconnect()

		callUpdateEventMsg := gateway1.WaitForEvent(t, "CALL_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var call callUpdateEvent
			if err := json.Unmarshal(data, &call); err != nil {
				return false
			}
			hasUser2InVoice := false
			for _, vs := range call.VoiceStates {
				if vs.UserID == user2.UserID {
					hasUser2InVoice = true
					break
				}
			}
			return hasUser2InVoice && !containsString(call.Ringing, user2.UserID)
		})

		var callUpdate callUpdateEvent
		if err := json.Unmarshal(callUpdateEventMsg.Data, &callUpdate); err != nil {
			t.Fatalf("failed to decode CALL_UPDATE event: %v", err)
		}

		t.Logf("User2 joined, ringing now: %v, voice_states: %d users", callUpdate.Ringing, len(callUpdate.VoiceStates))

		if !containsString(callUpdate.Ringing, user3.UserID) {
			t.Fatalf("expected user3 still in ringing, got %v", callUpdate.Ringing)
		}

		stopRinging(t, client, user3.Token, parseSnowflake(t, groupDM.ID), nil)

		callUpdateEventMsg2 := gateway1.WaitForEvent(t, "CALL_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var call callUpdateEvent
			if err := json.Unmarshal(data, &call); err != nil {
				return false
			}
			return !containsString(call.Ringing, user3.UserID)
		})

		if err := json.Unmarshal(callUpdateEventMsg2.Data, &callUpdate); err != nil {
			t.Fatalf("failed to decode CALL_UPDATE event after rejection: %v", err)
		}
		t.Logf("User3 rejected, ringing now: %v", callUpdate.Ringing)

		if len(callUpdate.Ringing) != 0 {
			t.Fatalf("expected ringing to be empty after user3 rejected, got %v", callUpdate.Ringing)
		}
	})
}
