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

func TestDMCallMessageStructure(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)

	guild := createGuild(t, client, user1.Token, "Test Guild")
	invite := createChannelInvite(t, client, user1.Token, parseSnowflake(t, guild.SystemChannel))
	joinGuild(t, client, user2.Token, invite.Code)

	dm := createDmChannel(t, client, user1.Token, parseSnowflake(t, user2.UserID))

	gateway1 := newGatewayClient(t, client, user1.Token)
	defer gateway1.Close()

	t.Run("call message has correct structure during active call", func(t *testing.T) {
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
				t.Logf("failed to decode VOICE_STATE_UPDATE: %v", err)
				return false
			}
			return vs.UserID == user1.UserID && vs.ConnectionID == connectionID
		})

		time.Sleep(500 * time.Millisecond)

		callMsg := findCallMessage(t, client, user1.Token, parseSnowflake(t, dm.ID))
		if callMsg == nil {
			t.Fatal("expected to find CALL message in channel")
			return
		}

		if callMsg.Type != 3 {
			t.Fatalf("expected message type 3 (CALL), got %d", callMsg.Type)
		}

		require.NotNil(t, callMsg.Call, "call object should not be nil")
		require.Nil(t, callMsg.Call.EndedTimestamp, "ended_timestamp should be nil during active call")

		gateway1.SendVoiceStateUpdate(nil, nil, &connectionID, false, false, false, false)

		gateway1.WaitForEvent(t, "CALL_DELETE", 5*time.Second, nil)

		time.Sleep(500 * time.Millisecond)

		callMsgAfter := findCallMessage(t, client, user1.Token, parseSnowflake(t, dm.ID))
		if callMsgAfter == nil {
			t.Fatal("expected CALL message to still exist after call ended")
			return
		}

		callAfter := callMsgAfter.Call
		require.NotNil(t, callAfter, "call object should not be nil after call ended")
		require.NotNil(t, callAfter.EndedTimestamp, "ended_timestamp should be set after call ended")
		require.NotEmpty(t, callAfter.Participants, "participants array should not be empty")
		require.Contains(t, callAfter.Participants, user1.UserID, "user1 should be in participants")
	})
}
