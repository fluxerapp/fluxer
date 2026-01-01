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
)

func TestMessageFetchLargeDataset(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	guild := createGuild(t, client, account.Token, "Large Dataset Test Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	clearChannelMessages(t, client, channelID, account.Token)

	// Seed 200+ messages across multiple buckets
	const totalMessages = 250
	seedResult := seedMessagesWithContent(t, client, channelID, totalMessages, account.UserID)

	if len(seedResult.Messages) != totalMessages {
		t.Fatalf("expected %d messages to be seeded, got %d", totalMessages, len(seedResult.Messages))
	}

	if len(seedResult.BucketsPopulated) == 0 {
		t.Fatalf("expected buckets to be populated by seed, got %d", len(seedResult.BucketsPopulated))
	}

	fetchedIDs := make(map[string]bool)
	allFetchedMessages := []struct {
		ID        string `json:"id"`
		ChannelID string `json:"channel_id"`
		Content   string `json:"content"`
	}{}

	// Paginate through ALL messages
	const limit = 50
	var beforeID *int64

	for {
		url := fmt.Sprintf("/channels/%d/messages?limit=%d", channelID, limit)
		if beforeID != nil {
			url = fmt.Sprintf("%s&before=%d", url, *beforeID)
		}

		resp, err := client.getWithAuth(url, account.Token)
		if err != nil {
			t.Fatalf("failed to fetch messages: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)

		var messages []struct {
			ID        string `json:"id"`
			ChannelID string `json:"channel_id"`
			Content   string `json:"content"`
		}
		decodeJSONResponse(t, resp, &messages)

		if len(messages) == 0 {
			break
		}

		for i := 0; i < len(messages)-1; i++ {
			current := parseSnowflake(t, messages[i].ID)
			next := parseSnowflake(t, messages[i+1].ID)
			if current <= next {
				t.Fatalf("messages not in descending order: message[%d] (%d) <= message[%d] (%d)", i, current, i+1, next)
			}
		}

		allFetchedMessages = append(allFetchedMessages, messages...)

		for _, msg := range messages {
			if fetchedIDs[msg.ID] {
				t.Fatalf("duplicate message ID found during pagination: %s", msg.ID)
			}
			fetchedIDs[msg.ID] = true
		}

		lastMsgID := parseSnowflake(t, messages[len(messages)-1].ID)
		beforeID = &lastMsgID

		if len(allFetchedMessages) > totalMessages+10 {
			t.Fatalf("fetched more messages than expected, possible infinite loop")
		}
	}

	if len(allFetchedMessages) != totalMessages {
		t.Fatalf("expected to fetch all %d messages, got %d", totalMessages, len(allFetchedMessages))
	}

	seededIDs := make(map[string]bool)
	for _, msg := range seedResult.Messages {
		seededIDs[msg.MessageID] = true
	}

	for _, msg := range allFetchedMessages {
		if !seededIDs[msg.ID] {
			t.Fatalf("fetched message %s was not in seeded messages", msg.ID)
		}
	}

	for i := 0; i < len(allFetchedMessages)-1; i++ {
		current := parseSnowflake(t, allFetchedMessages[i].ID)
		next := parseSnowflake(t, allFetchedMessages[i+1].ID)
		if current <= next {
			t.Fatalf("messages not in descending order across entire dataset: message[%d] (%d) <= message[%d] (%d)", i, current, i+1, next)
		}
	}

	buckets := getChannelBuckets(t, client, channelID, account.Token)
	t.Logf("Bucket index contains %d buckets after fetching: %v", buckets.Count, func() []int {
		result := make([]int, len(buckets.Buckets))
		for i, b := range buckets.Buckets {
			result[i] = b.Bucket
		}
		return result
	}())
	if buckets.Count != len(seedResult.BucketsPopulated) {
		t.Logf("WARNING: bucket count mismatch - seed reported %d, endpoint reports %d", len(seedResult.BucketsPopulated), buckets.Count)
	}

	for i, msg := range allFetchedMessages {
		if parseSnowflake(t, msg.ChannelID) != channelID {
			t.Fatalf("message[%d] has wrong channel_id: expected %d, got %s", i, channelID, msg.ChannelID)
		}
	}
}
