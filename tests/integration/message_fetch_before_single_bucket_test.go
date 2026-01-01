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

func TestMessageFetchBeforeSingleBucket(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	guild := createGuild(t, client, account.Token, "Fetch Before Single Bucket Test Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	clearChannelMessages(t, client, channelID, account.Token)

	// Seed 20 messages in the same bucket (all within a short time span)
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

	// Get the 15th message (index 14) to use as the "before" anchor
	const anchorIndex = 14
	anchorMessageID := seedResult.Messages[anchorIndex].MessageID

	// Fetch 10 messages before the 15th message
	// Expected: messages at indices 4-13 (10 messages)
	const limit = 10
	resp, err := client.getWithAuth(fmt.Sprintf("/channels/%d/messages?limit=%d&before=%s", channelID, limit, anchorMessageID), account.Token)
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

	expectedIDs := make(map[string]bool)
	for i := anchorIndex - limit; i < anchorIndex; i++ {
		expectedIDs[seedResult.Messages[i].MessageID] = true
	}

	for i, msg := range fetchedMessages {
		if !expectedIDs[msg.ID] {
			t.Fatalf("message[%d] ID %s was not in the expected range (indices %d-%d)", i, msg.ID, anchorIndex-limit, anchorIndex-1)
		}
	}

	expectedFirstMessageID := seedResult.Messages[anchorIndex-1].MessageID
	if fetchedMessages[0].ID != expectedFirstMessageID {
		t.Fatalf("first fetched message should be from index %d: expected %s, got %s", anchorIndex-1, expectedFirstMessageID, fetchedMessages[0].ID)
	}

	expectedLastMessageID := seedResult.Messages[anchorIndex-limit].MessageID
	if fetchedMessages[limit-1].ID != expectedLastMessageID {
		t.Fatalf("last fetched message should be from index %d: expected %s, got %s", anchorIndex-limit, expectedLastMessageID, fetchedMessages[limit-1].ID)
	}

	anchorSnowflake := parseSnowflake(t, anchorMessageID)
	for i, msg := range fetchedMessages {
		msgSnowflake := parseSnowflake(t, msg.ID)
		if msgSnowflake >= anchorSnowflake {
			t.Fatalf("message[%d] ID %s (%d) should be before anchor %s (%d)", i, msg.ID, msgSnowflake, anchorMessageID, anchorSnowflake)
		}
	}

	t.Logf("Fetch before single bucket test passed: retrieved %d messages before message at index %d", len(fetchedMessages), anchorIndex)
}
