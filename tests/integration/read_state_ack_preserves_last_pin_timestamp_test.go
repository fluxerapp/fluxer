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
	"fmt"
	"net/http"
	"testing"
	"time"
)

func TestReadStateAckPreservesLastPinTimestamp(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	ownerSocket := newGatewayClient(t, client, owner.Token)
	t.Cleanup(ownerSocket.Close)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Pin Preserve Guild %d", time.Now().UnixNano()))
	channelID := parseSnowflake(t, guild.SystemChannel)

	message1 := sendChannelMessage(t, client, owner.Token, channelID, "message to pin")

	resp, err := client.putWithAuth(fmt.Sprintf("/channels/%d/pins/%d", channelID, parseSnowflake(t, message1.ID)), owner.Token)
	if err != nil {
		t.Fatalf("failed to pin message: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	ownerSocket.WaitForEvent(t, "CHANNEL_PINS_UPDATE", 30*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			ChannelID string `json:"channel_id"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			return false
		}
		return payload.ChannelID == guild.SystemChannel
	})

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/channels/%d/pins/ack", channelID), nil, owner.Token)
	if err != nil {
		t.Fatalf("failed to ack pins: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	ownerSocket.WaitForEvent(t, "CHANNEL_PINS_ACK", 30*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			ChannelID string `json:"channel_id"`
			Timestamp string `json:"timestamp"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			return false
		}
		return payload.ChannelID == guild.SystemChannel && payload.Timestamp != ""
	})

	message2 := sendChannelMessage(t, client, owner.Token, channelID, "another message")

	resp, err = client.postJSONWithAuth(
		fmt.Sprintf("/channels/%d/messages/%d/ack", channelID, parseSnowflake(t, message2.ID)),
		map[string]any{"mention_count": 0},
		owner.Token,
	)
	if err != nil {
		t.Fatalf("failed to ack message: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	waitForAckEvent(t, ownerSocket, guild.SystemChannel, message2.ID)

	ownerSocket.Close()
	newSocket := newGatewayClient(t, client, owner.Token)
	t.Cleanup(newSocket.Close)

	var readStates []struct {
		ID               string  `json:"id"`
		LastMessageID    *string `json:"last_message_id"`
		LastPinTimestamp *string `json:"last_pin_timestamp"`
		MentionCount     int     `json:"mention_count"`
	}

	newSocket.WaitForEvent(t, "READY", 30*time.Second, func(raw json.RawMessage) bool {
		var ready struct {
			ReadStates []struct {
				ID               string  `json:"id"`
				LastMessageID    *string `json:"last_message_id"`
				LastPinTimestamp *string `json:"last_pin_timestamp"`
				MentionCount     int     `json:"mention_count"`
			} `json:"read_states"`
		}
		if err := json.Unmarshal(raw, &ready); err != nil {
			t.Logf("failed to unmarshal READY: %v", err)
			return false
		}
		readStates = ready.ReadStates
		return true
	})

	var foundReadState *struct {
		ID               string  `json:"id"`
		LastMessageID    *string `json:"last_message_id"`
		LastPinTimestamp *string `json:"last_pin_timestamp"`
		MentionCount     int     `json:"mention_count"`
	}

	for i := range readStates {
		if readStates[i].ID == guild.SystemChannel {
			foundReadState = &readStates[i]
			break
		}
	}

	if foundReadState == nil {
		t.Fatalf("read state for channel %s not found in READY payload", guild.SystemChannel)
	}

	if foundReadState.LastPinTimestamp == nil || *foundReadState.LastPinTimestamp == "" {
		t.Errorf("last_pin_timestamp was lost after message ack; expected non-empty timestamp, got nil or empty")
	}

	if foundReadState.LastMessageID == nil || *foundReadState.LastMessageID != message2.ID {
		t.Errorf("last_message_id mismatch; expected %s, got %v", message2.ID, foundReadState.LastMessageID)
	}
}
