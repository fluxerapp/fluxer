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

func TestMessageFetchLatestLegacyScan(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	guild := createGuild(t, client, account.Token, "Legacy Scan Test Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	clearChannelMessages(t, client, channelID, account.Token)

	// Seed messages without populating bucket index (legacy mode)
	const totalMessages = 30
	timestamps := make([]time.Time, totalMessages)
	baseTime := time.Now().Add(-time.Hour * 24)
	for i := 0; i < totalMessages; i++ {
		timestamps[i] = baseTime.Add(time.Minute * time.Duration(i))
	}

	seedResult := seedMessagesRaw(t, client, channelID, timestamps, account.UserID)

	if len(seedResult.Messages) != totalMessages {
		t.Fatalf("expected %d messages to be seeded, got %d", totalMessages, len(seedResult.Messages))
	}

	if len(seedResult.BucketsPopulated) != 0 {
		t.Fatalf("expected no buckets to be populated initially, got %d", len(seedResult.BucketsPopulated))
	}

	bucketsBefore := getChannelBuckets(t, client, channelID, account.Token)
	if bucketsBefore.Count != 0 {
		t.Fatalf("expected bucket index to be empty before fetch, got %d buckets", bucketsBefore.Count)
	}

	// Fetch latest messages via the API - should trigger legacy scan
	const limit = 25
	resp, err := client.getWithAuth(fmt.Sprintf("/channels/%d/messages?limit=%d", channelID, limit), account.Token)
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

	expectedLatestIDs := make(map[string]bool)
	for i := totalMessages - limit; i < totalMessages; i++ {
		expectedLatestIDs[seedResult.Messages[i].MessageID] = true
	}

	for i, msg := range fetchedMessages {
		if !expectedLatestIDs[msg.ID] {
			t.Fatalf("message[%d] ID %s was not in the expected latest %d messages", i, msg.ID, limit)
		}
	}

	latestSeededMessageID := seedResult.Messages[totalMessages-1].MessageID
	if fetchedMessages[0].ID != latestSeededMessageID {
		t.Fatalf("first fetched message should be the latest seeded message: expected %s, got %s", latestSeededMessageID, fetchedMessages[0].ID)
	}

	bucketsAfter := getChannelBuckets(t, client, channelID, account.Token)
	if bucketsAfter.Count == 0 {
		t.Fatalf("expected bucket index to be healed after fetch, but got 0 buckets")
	}

	t.Logf("Legacy scan test passed: fetched %d messages and healed %d buckets", len(fetchedMessages), bucketsAfter.Count)
}
