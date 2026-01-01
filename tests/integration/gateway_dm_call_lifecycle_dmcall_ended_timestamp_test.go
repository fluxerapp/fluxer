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

func TestDMCallEndedTimestamp(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)

	guild := createGuild(t, client, user1.Token, "Test Guild")
	invite := createChannelInvite(t, client, user1.Token, parseSnowflake(t, guild.SystemChannel))
	joinGuild(t, client, user2.Token, invite.Code)

	dm := createDmChannel(t, client, user1.Token, parseSnowflake(t, user2.UserID))

	gateway1 := newGatewayClient(t, client, user1.Token)
	defer gateway1.Close()

	t.Run("call message ended_timestamp is set after call ends", func(t *testing.T) {
		beforeCallStart := time.Now()

		ringCall(t, client, user1.Token, parseSnowflake(t, dm.ID), nil)

		gateway1.WaitForEvent(t, "CALL_CREATE", 5*time.Second, nil)

		gateway1.SendVoiceStateUpdate(nil, &dm.ID, nil, false, false, false, false)
		serverUpdate := gateway1.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, nil)
		var vsu voiceServerUpdate
		if err := json.Unmarshal(serverUpdate.Data, &vsu); err != nil {
			t.Fatalf("failed to decode VOICE_SERVER_UPDATE: %v", err)
		}
		connectionID := vsu.ConnectionID

		lkConn := connectToLiveKit(t, vsu.Endpoint, vsu.Token, dm.ID, user1.UserID)
		defer lkConn.disconnect()

		gateway1.WaitForEvent(t, "VOICE_STATE_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				return false
			}
			return vs.UserID == user1.UserID && vs.ConnectionID == connectionID
		})

		time.Sleep(300 * time.Millisecond)
		callMsgDuring := findCallMessage(t, client, user1.Token, parseSnowflake(t, dm.ID))
		if callMsgDuring == nil {
			t.Fatal("expected to find CALL message during active call")
			return
		}

		if callMsgDuring.Type != 3 {
			t.Fatalf("expected message type 3 (CALL), got %d", callMsgDuring.Type)
		}

		if callMsgDuring.Call == nil {
			t.Fatal("expected call object to be present during active call")
		}
		if callMsgDuring.Call.EndedTimestamp != nil {
			t.Fatalf("expected ended_timestamp to be nil during active call, got: %s", *callMsgDuring.Call.EndedTimestamp)
		}

		beforeCallEnd := time.Now()
		gateway1.SendVoiceStateUpdate(nil, nil, &connectionID, false, false, false, false)

		gateway1.WaitForEvent(t, "CALL_DELETE", 5*time.Second, nil)
		afterCallEnd := time.Now()

		time.Sleep(1 * time.Second)

		callMsgAfter := findCallMessage(t, client, user1.Token, parseSnowflake(t, dm.ID))
		if callMsgAfter == nil {
			t.Fatal("expected CALL message to still exist after call ended")
			return
		}

		if callMsgAfter.Call == nil {
			t.Fatal("expected call object to be present in message after call ended")
		}

		if callMsgAfter.Call.EndedTimestamp == nil {
			t.Fatal("expected ended_timestamp to be set after call ended, but it is null")
		}

		endedTs := *callMsgAfter.Call.EndedTimestamp
		endedTime, err := time.Parse(time.RFC3339Nano, endedTs)
		if err != nil {
			endedTime, err = time.Parse("2006-01-02T15:04:05.999999", endedTs)
			if err != nil {
				t.Fatalf("failed to parse ended_timestamp %q: %v", endedTs, err)
			}
		}

		if endedTime.Before(beforeCallStart) {
			t.Fatalf("ended_timestamp %v is before call started %v", endedTime, beforeCallStart)
		}

		if endedTime.Before(beforeCallEnd.Add(-5 * time.Second)) {
			t.Fatalf("ended_timestamp %v is too early compared to call end time %v", endedTime, beforeCallEnd)
		}

		if endedTime.After(afterCallEnd.Add(5 * time.Second)) {
			t.Fatalf("ended_timestamp %v is too late compared to call end time %v", endedTime, afterCallEnd)
		}

		t.Logf("Call ended, ended_timestamp correctly set to: %s", endedTs)
	})
}
