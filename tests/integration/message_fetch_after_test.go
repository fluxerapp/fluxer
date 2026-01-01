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
	"fmt"
	"net/http"
	"testing"
	"time"
)

func TestMessageFetchAfter(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	guild := createGuild(t, client, account.Token, "Fetch After Test Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	clearChannelMessages(t, client, channelID, account.Token)

	// Seed 20 messages
	const totalMessages = 20
	timestamps := make([]time.Time, totalMessages)
	baseTime := time.Now().Add(-time.Hour * 1)
	for i := 0; i < totalMessages; i++ {
		timestamps[i] = baseTime.Add(time.Minute * time.Duration(i))
	}

	seedResult := seedMessagesAtTimestamps(t, client, channelID, timestamps, account.UserID)

	if len(seedResult.Messages) != totalMessages {
		t.Fatalf("expected %d messages to be seeded, got %d", totalMessages, len(seedResult.Messages))
	}

	// Get the 5th message (index 4) to use as the "after" anchor
	const anchorIndex = 4
	anchorMessageID := seedResult.Messages[anchorIndex].MessageID

	// Fetch messages after the 5th message
	// The "after" parameter returns messages with IDs greater than the anchor
	// With DESC ordering and a limit, we get the LATEST N messages after the anchor
	// So with limit=10 and 15 messages after the anchor (indices 5-19),
	// we expect the 10 newest: indices 10-19
	const limit = 10
	resp, err := client.getWithAuth(fmt.Sprintf("/channels/%d/messages?limit=%d&after=%s", channelID, limit, anchorMessageID), account.Token)
	if err != nil {
		t.Fatalf("failed to fetch messages: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var fetchedMessages []struct {
		ID        string `json:"id"`
		ChannelID string `json:"channel_id"`
		Content   string `json:"content"`
	}
	decodeJSONResponse(t, resp, &fetchedMessages)

	if len(fetchedMessages) != limit {
		t.Fatalf("expected %d messages, got %d", limit, len(fetchedMessages))
	}

	for i := 0; i < len(fetchedMessages)-1; i++ {
		current := parseSnowflake(t, fetchedMessages[i].ID)
		next := parseSnowflake(t, fetchedMessages[i+1].ID)
		if current <= next {
			t.Fatalf("messages not in descending order: message[%d] (%d) <= message[%d] (%d)", i, current, i+1, next)
		}
	}

	// With 15 messages after anchor (indices 5-19), limit=10 returns the newest 10 (indices 10-19)
	// Calculate expected range: total messages after anchor = totalMessages - anchorIndex - 1 = 15
	// Expected indices: from (totalMessages - limit) to (totalMessages - 1) = indices 10-19
	const expectedFirstIndex = totalMessages - 1 // 19 (newest)
	const expectedLastIndex = totalMessages - limit

	expectedIDs := make(map[string]bool)
	for i := expectedLastIndex; i <= expectedFirstIndex; i++ {
		expectedIDs[seedResult.Messages[i].MessageID] = true
	}

	for i, msg := range fetchedMessages {
		if !expectedIDs[msg.ID] {
			t.Fatalf("message[%d] ID %s was not in the expected range (indices %d-%d)", i, msg.ID, expectedLastIndex, expectedFirstIndex)
		}
	}

	expectedFirstMessageID := seedResult.Messages[expectedFirstIndex].MessageID
	if fetchedMessages[0].ID != expectedFirstMessageID {
		t.Fatalf("first fetched message should be from index %d: expected %s, got %s", expectedFirstIndex, expectedFirstMessageID, fetchedMessages[0].ID)
	}

	expectedLastMessageID := seedResult.Messages[expectedLastIndex].MessageID
	if fetchedMessages[limit-1].ID != expectedLastMessageID {
		t.Fatalf("last fetched message should be from index %d: expected %s, got %s", expectedLastIndex, expectedLastMessageID, fetchedMessages[limit-1].ID)
	}

	anchorSnowflake := parseSnowflake(t, anchorMessageID)
	for i, msg := range fetchedMessages {
		msgSnowflake := parseSnowflake(t, msg.ID)
		if msgSnowflake <= anchorSnowflake {
			t.Fatalf("message[%d] ID %s (%d) should be after anchor %s (%d)", i, msg.ID, msgSnowflake, anchorMessageID, anchorSnowflake)
		}
	}

	t.Logf("Fetch after test passed: retrieved %d messages after message at index %d", len(fetchedMessages), anchorIndex)
}
