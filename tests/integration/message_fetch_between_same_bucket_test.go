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

func TestMessageFetchBetweenSameBucket(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	guild := createGuild(t, client, account.Token, "Fetch Between Same Bucket Test Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	clearChannelMessages(t, client, channelID, account.Token)

	// Seed 20 messages in the same bucket
	const totalMessages = 20
	timestamps := make([]time.Time, totalMessages)
	baseTime := time.Now().Add(-time.Hour * 1)
	for i := 0; i < totalMessages; i++ {
		timestamps[i] = baseTime.Add(time.Second * time.Duration(i*10))
	}

	seedResult := seedMessagesAtTimestamps(t, client, channelID, timestamps, account.UserID)

	if len(seedResult.Messages) != totalMessages {
		t.Fatalf("expected %d messages to be seeded, got %d", totalMessages, len(seedResult.Messages))
	}

	// Get message 5 (index 4) as the "after" anchor
	// Get message 15 (index 14) as the "before" anchor
	// Expected: messages at indices 5-13 (9 messages, exclusive of bounds)
	const afterIndex = 4
	const beforeIndex = 14
	afterMessageID := seedResult.Messages[afterIndex].MessageID
	beforeMessageID := seedResult.Messages[beforeIndex].MessageID

	resp, err := client.getWithAuth(fmt.Sprintf("/channels/%d/messages?after=%s&before=%s", channelID, afterMessageID, beforeMessageID), account.Token)
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

	expectedCount := beforeIndex - afterIndex - 1
	if len(fetchedMessages) != expectedCount {
		t.Fatalf("expected %d messages, got %d", expectedCount, len(fetchedMessages))
	}

	for i := 0; i < len(fetchedMessages)-1; i++ {
		current := parseSnowflake(t, fetchedMessages[i].ID)
		next := parseSnowflake(t, fetchedMessages[i+1].ID)
		if current <= next {
			t.Fatalf("messages not in descending order: message[%d] (%d) <= message[%d] (%d)", i, current, i+1, next)
		}
	}

	expectedIDs := make(map[string]bool)
	for i := afterIndex + 1; i < beforeIndex; i++ {
		expectedIDs[seedResult.Messages[i].MessageID] = true
	}

	for i, msg := range fetchedMessages {
		if !expectedIDs[msg.ID] {
			t.Fatalf("message[%d] ID %s was not in the expected range (indices %d-%d)", i, msg.ID, afterIndex+1, beforeIndex-1)
		}
	}

	expectedFirstMessageID := seedResult.Messages[beforeIndex-1].MessageID
	if fetchedMessages[0].ID != expectedFirstMessageID {
		t.Fatalf("first fetched message should be from index %d: expected %s, got %s", beforeIndex-1, expectedFirstMessageID, fetchedMessages[0].ID)
	}

	expectedLastMessageID := seedResult.Messages[afterIndex+1].MessageID
	if fetchedMessages[expectedCount-1].ID != expectedLastMessageID {
		t.Fatalf("last fetched message should be from index %d: expected %s, got %s", afterIndex+1, expectedLastMessageID, fetchedMessages[expectedCount-1].ID)
	}

	afterSnowflake := parseSnowflake(t, afterMessageID)
	beforeSnowflake := parseSnowflake(t, beforeMessageID)
	for i, msg := range fetchedMessages {
		msgSnowflake := parseSnowflake(t, msg.ID)
		if msgSnowflake <= afterSnowflake {
			t.Fatalf("message[%d] ID %s (%d) should be after 'after' anchor %s (%d)", i, msg.ID, msgSnowflake, afterMessageID, afterSnowflake)
		}
		if msgSnowflake >= beforeSnowflake {
			t.Fatalf("message[%d] ID %s (%d) should be before 'before' anchor %s (%d)", i, msg.ID, msgSnowflake, beforeMessageID, beforeSnowflake)
		}
	}

	t.Logf("Fetch between same bucket test passed: retrieved %d messages between indices %d and %d (exclusive)", len(fetchedMessages), afterIndex, beforeIndex)
}
