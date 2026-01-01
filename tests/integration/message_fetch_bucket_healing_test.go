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

func TestMessageFetchBucketHealing(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	guild := createGuild(t, client, account.Token, "Bucket Healing Test Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	clearChannelMessages(t, client, channelID, account.Token)

	// Insert messages directly WITHOUT bucket index (use seedMessagesRaw)
	const totalMessages = 30
	timestamps := make([]time.Time, totalMessages)
	baseTime := time.Now().Add(-time.Hour * 48)
	for i := 0; i < totalMessages; i++ {
		timestamps[i] = baseTime.Add(time.Minute * time.Duration(i*5))
	}

	seedResult := seedMessagesRaw(t, client, channelID, timestamps, account.UserID)

	if len(seedResult.Messages) != totalMessages {
		t.Fatalf("expected %d messages to be seeded, got %d", totalMessages, len(seedResult.Messages))
	}

	bucketsBefore := getChannelBuckets(t, client, channelID, account.Token)
	if bucketsBefore.Count != 0 {
		t.Fatalf("expected bucket index to be empty before fetch, got %d buckets", bucketsBefore.Count)
	}

	stateBefore := getChannelState(t, client, channelID, account.Token)
	if !stateBefore.HasMessages {
		t.Fatalf("expected channel state to show has_messages = true")
	}
	if stateBefore.LastMessageBucket != nil {
		t.Fatalf("expected channel state to have last_message_bucket = nil for legacy scenario, got %d", *stateBefore.LastMessageBucket)
	}

	resp, err := client.getWithAuth(fmt.Sprintf("/channels/%d/messages?limit=50", channelID), account.Token)
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

	if len(fetchedMessages) != totalMessages {
		t.Fatalf("expected %d messages to be fetched, got %d", totalMessages, len(fetchedMessages))
	}

	for i := 0; i < len(fetchedMessages)-1; i++ {
		current := parseSnowflake(t, fetchedMessages[i].ID)
		next := parseSnowflake(t, fetchedMessages[i+1].ID)
		if current <= next {
			t.Fatalf("messages not in descending order: message[%d] (%d) <= message[%d] (%d)", i, current, i+1, next)
		}
	}

	bucketsAfter := getChannelBuckets(t, client, channelID, account.Token)
	if bucketsAfter.Count == 0 {
		t.Fatalf("expected bucket index to be populated after fetch, got 0 buckets")
	}

	expectedBuckets := make(map[int]bool)
	for _, msg := range seedResult.Messages {
		expectedBuckets[msg.Bucket] = true
	}

	if bucketsAfter.Count != len(expectedBuckets) {
		t.Fatalf("expected %d buckets in index, got %d", len(expectedBuckets), bucketsAfter.Count)
	}

	for _, bucket := range bucketsAfter.Buckets {
		if !expectedBuckets[bucket.Bucket] {
			t.Fatalf("unexpected bucket %d in index", bucket.Bucket)
		}
	}

	stateAfter := getChannelState(t, client, channelID, account.Token)
	if !stateAfter.HasMessages {
		t.Fatalf("expected channel state to show has_messages = true after healing")
	}
	if stateAfter.LastMessageID == nil {
		t.Fatalf("expected channel state to have last_message_id after healing")
	}
}
