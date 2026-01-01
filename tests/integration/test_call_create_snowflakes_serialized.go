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
	"strings"
	"testing"
	"time"
)

// TestCallCreateSnowflakesSerialized verifies that all snowflake IDs in CALL_CREATE
// events are serialized as JSON strings, not numbers.
// This is critical because JavaScript cannot handle 64-bit integers precisely.
func TestCallCreateSnowflakesSerialized(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)

	dm := createDmChannel(t, client, user1.Token, parseSnowflake(t, user2.UserID))

	gateway1 := newGatewayClient(t, client, user1.Token)
	defer gateway1.Close()

	ringCall(t, client, user1.Token, parseSnowflake(t, dm.ID), nil)

	callEvent := gateway1.WaitForEvent(t, "CALL_CREATE", 5*time.Second, func(data json.RawMessage) bool {
		var call callCreateEvent
		if err := json.Unmarshal(data, &call); err != nil {
			return false
		}
		return call.ChannelID == dm.ID
	})

	rawJSON := string(callEvent.Data)

	if !strings.Contains(rawJSON, `"channel_id":"`+dm.ID+`"`) {
		if strings.Contains(rawJSON, `"channel_id":`+dm.ID) {
			t.Errorf("channel_id is serialized as a number, should be a string. Raw: %s", rawJSON)
		} else {
			t.Errorf("channel_id not found or has unexpected value. Raw: %s", rawJSON)
		}
	}

	if !strings.Contains(rawJSON, `"message_id":"`) {
		if strings.Contains(rawJSON, `"message_id":`) && !strings.Contains(rawJSON, `"message_id":"`) {
			t.Errorf("message_id is serialized as a number, should be a string. Raw: %s", rawJSON)
		}
	}

	if strings.Contains(rawJSON, `"ringing":[`) {
		ringingStart := strings.Index(rawJSON, `"ringing":[`)
		if ringingStart != -1 {
			ringingEnd := strings.Index(rawJSON[ringingStart:], `]`)
			if ringingEnd != -1 {
				ringingSection := rawJSON[ringingStart : ringingStart+ringingEnd+1]
				if !strings.Contains(ringingSection, `[]`) {
					if !strings.Contains(ringingSection, `"`) {
						t.Errorf("ringing array contains bare numbers, should contain strings. Section: %s", ringingSection)
					}
				}
			}
		}
	}

	// Decode and verify the struct fields are populated correctly
	var call callCreateEvent
	if err := json.Unmarshal(callEvent.Data, &call); err != nil {
		t.Fatalf("failed to decode CALL_CREATE: %v", err)
	}

	if call.ChannelID == "" {
		t.Error("channel_id is empty after decoding - likely serialized as wrong type")
	}
	if call.MessageID == "" {
		t.Error("message_id is empty after decoding - likely serialized as wrong type")
	}

	t.Logf("Snowflake serialization test passed. channel_id=%s, message_id=%s, ringing=%v",
		call.ChannelID, call.MessageID, call.Ringing)
}
