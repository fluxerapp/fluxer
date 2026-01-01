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

	"github.com/stretchr/testify/require"
)

func TestDMCallParticipants(t *testing.T) {
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

	t.Run("call message participants contains all users who joined the call", func(t *testing.T) {
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

		gateway1.WaitForEvent(t, "VOICE_STATE_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				return false
			}
			return vs.UserID == user1.UserID && vs.ConnectionID == connectionID1
		})

		t.Log("User1 joined the call")

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

		gateway2.WaitForEvent(t, "CALL_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var call callUpdateEvent
			if err := json.Unmarshal(data, &call); err != nil {
				return false
			}
			return len(call.VoiceStates) == 1
		})

		t.Log("User1 left, user2 still in call")

		gateway2.SendVoiceStateUpdate(nil, nil, &connectionID2, false, false, false, false)

		gateway2.WaitForEvent(t, "CALL_DELETE", 5*time.Second, nil)

		t.Log("Call ended, verifying participants list...")

		time.Sleep(1 * time.Second)

		callMsg := findCallMessage(t, client, user1.Token, parseSnowflake(t, dm.ID))
		if callMsg == nil {
			t.Fatal("expected CALL message to exist after call ended")
			return
		}

		if callMsg.Call == nil {
			t.Fatal("expected call object to be present in message")
			return
		}

		callPayload := callMsg.Call
		participants := callPayload.Participants
		if len(participants) < 2 {
			t.Fatalf("expected at least 2 participants, got %d: %v", len(participants), participants)
		}

		if !containsString(participants, user1.UserID) {
			t.Fatalf("expected user1 (%s) to be in participants, got %v", user1.UserID, participants)
		}

		if !containsString(participants, user2.UserID) {
			t.Fatalf("expected user2 (%s) to be in participants, got %v", user2.UserID, participants)
		}

		t.Logf("Call participants correctly includes both users: %v", participants)

		require.NotNil(t, callPayload.EndedTimestamp, "ended_timestamp should be set after call ended")
		t.Logf("ended_timestamp: %s", *callPayload.EndedTimestamp)
	})
}
