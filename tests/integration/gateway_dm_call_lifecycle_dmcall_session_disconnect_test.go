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

func TestDMCallSessionDisconnect(t *testing.T) {
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

	t.Run("call ends when user's gateway session disconnects while in call", func(t *testing.T) {
		ringCall(t, client, user1.Token, parseSnowflake(t, dm.ID), nil)

		gateway1.WaitForEvent(t, "CALL_CREATE", 5*time.Second, nil)
		gateway2.WaitForEvent(t, "CALL_CREATE", 5*time.Second, nil)

		gateway2.SendVoiceStateUpdate(nil, &dm.ID, nil, false, false, false, false)
		serverUpdate2 := gateway2.WaitForEvent(t, "VOICE_SERVER_UPDATE", 5*time.Second, nil)
		var vsu2 voiceServerUpdate
		if err := json.Unmarshal(serverUpdate2.Data, &vsu2); err != nil {
			t.Fatalf("failed to decode VOICE_SERVER_UPDATE for user2: %v", err)
		}

		lkConn2 := connectToLiveKit(t, vsu2.Endpoint, vsu2.Token, dm.ID, user2.UserID)
		defer lkConn2.disconnect()

		gateway2.WaitForEvent(t, "VOICE_STATE_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var vs voiceStateUpdate
			if err := json.Unmarshal(data, &vs); err != nil {
				return false
			}
			return vs.UserID == user2.UserID
		})

		gateway1.WaitForEvent(t, "CALL_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
			var call callUpdateEvent
			if err := json.Unmarshal(data, &call); err != nil {
				return false
			}
			return len(call.VoiceStates) == 1
		})

		lkConn2.disconnect()
		gateway2.Close()

		callDeleteEvt := gateway1.WaitForEvent(t, "CALL_DELETE", 30*time.Second, func(data json.RawMessage) bool {
			var call callDeleteEvent
			if err := json.Unmarshal(data, &call); err != nil {
				return false
			}
			return call.ChannelID == dm.ID
		})

		var callDelete callDeleteEvent
		if err := json.Unmarshal(callDeleteEvt.Data, &callDelete); err != nil {
			t.Fatalf("failed to decode CALL_DELETE: %v", err)
		}

		if callDelete.ChannelID != dm.ID {
			t.Fatalf("expected CALL_DELETE for channel %s, got %s", dm.ID, callDelete.ChannelID)
		}

		time.Sleep(1 * time.Second)

		callMsg := findCallMessage(t, client, user1.Token, parseSnowflake(t, dm.ID))
		if callMsg == nil {
			t.Fatal("expected to find CALL message")
			return
		}

		callPayload := callMsg.Call
		require.NotNil(t, callPayload, "call message should have Call field")
		require.NotNil(t, callPayload.EndedTimestamp, "call message should have ended_timestamp set")
		t.Logf("Call message ended_timestamp correctly set: %s", *callPayload.EndedTimestamp)

		require.NotEmpty(t, callPayload.Participants, "call message should have participants")
		require.True(t, containsString(callPayload.Participants, user2.UserID), "user2 should be in participants list")
		t.Logf("User2 correctly recorded in participants: %v", callPayload.Participants)
	})
}
