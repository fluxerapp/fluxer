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

// TestVoiceStateSnowflakesSerialized verifies that snowflakes in voice state updates are strings
func TestVoiceStateSnowflakesSerialized(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)

	dm := createDmChannel(t, client, user1.Token, parseSnowflake(t, user2.UserID))

	gateway1 := newGatewayClient(t, client, user1.Token)
	defer gateway1.Close()

	gateway1.SendVoiceStateUpdate(nil, &dm.ID, nil, false, false, false, false)

	vsuEvent := gateway1.WaitForEvent(t, "VOICE_STATE_UPDATE", 5*time.Second, func(data json.RawMessage) bool {
		var vsu voiceStateUpdate
		if err := json.Unmarshal(data, &vsu); err != nil {
			return false
		}
		return vsu.ChannelID != nil && *vsu.ChannelID == dm.ID
	})

	rawJSON := string(vsuEvent.Data)

	if !strings.Contains(rawJSON, `"user_id":"`) {
		if strings.Contains(rawJSON, `"user_id":`) && !strings.Contains(rawJSON, `"user_id":"`) {
			t.Errorf("user_id is serialized as a number, should be a string. Raw: %s", rawJSON)
		}
	}

	if strings.Contains(rawJSON, `"channel_id":`) && !strings.Contains(rawJSON, `"channel_id":null`) {
		if !strings.Contains(rawJSON, `"channel_id":"`) {
			t.Errorf("channel_id is serialized as a number, should be a string. Raw: %s", rawJSON)
		}
	}

	// Decode and verify
	var vsu voiceStateUpdate
	if err := json.Unmarshal(vsuEvent.Data, &vsu); err != nil {
		t.Fatalf("failed to decode VOICE_STATE_UPDATE: %v", err)
	}

	if vsu.UserID == "" {
		t.Error("user_id is empty after decoding - likely serialized as wrong type")
	}

	t.Logf("Voice state snowflake serialization test passed. user_id=%s, channel_id=%v",
		vsu.UserID, vsu.ChannelID)
}
