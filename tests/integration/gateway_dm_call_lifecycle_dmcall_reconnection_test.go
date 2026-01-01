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

func TestDMCallReconnection(t *testing.T) {
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

	t.Run("user can rejoin call after leaving while another user is still connected", func(t *testing.T) {
		ringCall(t, client, user1.Token, parseSnowflake(t, dm.ID), nil)

		gateway1.WaitForEvent(t, "CALL_CREATE", 5*time.Second, nil)
		gateway2.WaitForEvent(t, "CALL_CREATE", 5*time.Second, nil)

		gateway1.SendVoiceStateUpdate(nil, &dm.ID, nil, false, false, false, false)
		serverUpdate1 := gateway1.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, nil)
		var vsu1 voiceServerUpdate
		if err := json.Unmarshal(serverUpdate1.Data, &vsu1); err != nil {
			t.Fatalf("failed to decode VOICE_SERVER_UPDATE for user1: %v", err)
		}
		connectionID1 := vsu1.ConnectionID

		lkConn1 := connectToLiveKit(t, vsu1.Endpoint, vsu1.Token, dm.ID, user1.UserID)
		defer lkConn1.disconnect()

		gateway2.SendVoiceStateUpdate(nil, &dm.ID, nil, false, false, false, false)
		serverUpdate2 := gateway2.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, nil)
		var vsu2 voiceServerUpdate
		if err := json.Unmarshal(serverUpdate2.Data, &vsu2); err != nil {
			t.Fatalf("failed to decode VOICE_SERVER_UPDATE for user2: %v", err)
		}
		connectionID2 := vsu2.ConnectionID

		lkConn2 := connectToLiveKit(t, vsu2.Endpoint, vsu2.Token, dm.ID, user2.UserID)
		defer lkConn2.disconnect()

		gateway1.WaitForEvent(t, "CALL_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var call callUpdateEvent
			if err := json.Unmarshal(data, &call); err != nil {
				return false
			}
			return len(call.VoiceStates) == 2
		})

		t.Log("Both users in call")

		gateway1.SendVoiceStateUpdate(nil, nil, &connectionID1, false, false, false, false)
		lkConn1.disconnect()

		gateway2.WaitForEvent(t, "CALL_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var call callUpdateEvent
			if err := json.Unmarshal(data, &call); err != nil {
				return false
			}
			return len(call.VoiceStates) == 1
		})

		t.Log("User1 left, user2 still in call")

		time.Sleep(500 * time.Millisecond)

		gateway1.SendVoiceStateUpdate(nil, &dm.ID, nil, false, false, false, false)
		serverUpdate1Rejoin := gateway1.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, nil)
		var vsu1Rejoin voiceServerUpdate
		if err := json.Unmarshal(serverUpdate1Rejoin.Data, &vsu1Rejoin); err != nil {
			t.Fatalf("failed to decode VOICE_SERVER_UPDATE for user1 rejoin: %v", err)
		}
		connectionID1New := vsu1Rejoin.ConnectionID

		if connectionID1New == connectionID1 {
			t.Log("Note: connection_id is the same after rejoin (this is acceptable)")
		} else {
			t.Logf("New connection_id after rejoin: old=%s, new=%s", connectionID1, connectionID1New)
		}

		lkConn1Rejoin := connectToLiveKit(t, vsu1Rejoin.Endpoint, vsu1Rejoin.Token, dm.ID, user1.UserID)
		defer lkConn1Rejoin.disconnect()

		callUpdateEvt := gateway1.WaitForEvent(t, "CALL_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var call callUpdateEvent
			if err := json.Unmarshal(data, &call); err != nil {
				return false
			}
			return len(call.VoiceStates) == 2
		})

		var callUpdate callUpdateEvent
		if err := json.Unmarshal(callUpdateEvt.Data, &callUpdate); err != nil {
			t.Fatalf("failed to decode CALL_UPDATE: %v", err)
		}

		if len(callUpdate.VoiceStates) != 2 {
			t.Fatalf("expected 2 voice_states after user1 rejoin, got %d", len(callUpdate.VoiceStates))
		}

		user1Found := false
		user2Found := false
		for _, vs := range callUpdate.VoiceStates {
			if vs.UserID == user1.UserID {
				user1Found = true
			}
			if vs.UserID == user2.UserID {
				user2Found = true
			}
		}

		if !user1Found {
			t.Fatal("expected user1 in voice_states after rejoin")
		}
		if !user2Found {
			t.Fatal("expected user2 in voice_states after rejoin")
		}

		t.Log("User1 successfully rejoined the call")

		gateway1.SendVoiceStateUpdate(nil, nil, &connectionID1New, false, false, false, false)
		gateway2.SendVoiceStateUpdate(nil, nil, &connectionID2, false, false, false, false)

		gateway1.WaitForEvent(t, "CALL_DELETE", 5*time.Second, nil)

		t.Log("Call ended after both users left")
	})
}
