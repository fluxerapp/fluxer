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

func TestMessageFetchBeforeCrossBucket(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	guild := createGuild(t, client, account.Token, "Fetch Before Cross Bucket Test Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	clearChannelMessages(t, client, channelID, account.Token)

	// Seed messages spanning 3 buckets (30 days apart for each bucket)
	// Bucket 1: 60 days ago (5 messages)
	// Bucket 2: 30 days ago (5 messages)
	// Bucket 3: now (5 messages)
	const messagesPerBucket = 5
	const totalMessages = messagesPerBucket * 3
	timestamps := make([]time.Time, totalMessages)

	bucket1Base := time.Now().Add(-60 * 24 * time.Hour)
	for i := 0; i < messagesPerBucket; i++ {
		timestamps[i] = bucket1Base.Add(time.Minute * time.Duration(i))
	}

	bucket2Base := time.Now().Add(-30 * 24 * time.Hour)
	for i := 0; i < messagesPerBucket; i++ {
		timestamps[messagesPerBucket+i] = bucket2Base.Add(time.Minute * time.Duration(i))
	}

	bucket3Base := time.Now()
	for i := 0; i < messagesPerBucket; i++ {
		timestamps[2*messagesPerBucket+i] = bucket3Base.Add(time.Minute * time.Duration(i))
	}

	seedResult := seedMessagesAtTimestamps(t, client, channelID, timestamps, account.UserID)

	if len(seedResult.Messages) != totalMessages {
		t.Fatalf("expected %d messages to be seeded, got %d", totalMessages, len(seedResult.Messages))
	}

	if len(seedResult.BucketsPopulated) < 2 {
		t.Fatalf("expected at least 2 buckets to be populated, got %d", len(seedResult.BucketsPopulated))
	}

	// Get a message from the newest bucket (index 12, which is in bucket 3)
	// to use as the "before" anchor
	const anchorIndex = 12
	anchorMessageID := seedResult.Messages[anchorIndex].MessageID

	// Fetch 10 messages before this anchor
	// This should span across buckets (bucket 2 and bucket 1)
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

	bucketsFound := make(map[int]bool)
	for _, msg := range seedResult.Messages {
		for i, fetchedMsg := range fetchedMessages {
			if msg.MessageID == fetchedMsg.ID {
				bucketsFound[msg.Bucket] = true
				t.Logf("Fetched message[%d] is from bucket %d", i, msg.Bucket)
			}
		}
	}

	if len(bucketsFound) < 2 {
		t.Fatalf("expected messages to span at least 2 buckets, but only found messages in %d bucket(s)", len(bucketsFound))
	}

	t.Logf("Fetch before cross-bucket test passed: retrieved %d messages spanning %d buckets", len(fetchedMessages), len(bucketsFound))
}
