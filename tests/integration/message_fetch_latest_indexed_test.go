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

func TestMessageFetchLatestWithBucketIndex(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	guild := createGuild(t, client, account.Token, "Message Fetch Test Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	clearChannelMessages(t, client, channelID, account.Token)

	// Seed 50 messages with bucket index populated
	const totalMessages = 50
	seedResult := seedMessagesWithContent(t, client, channelID, totalMessages, account.UserID)

	if len(seedResult.Messages) != totalMessages {
		t.Fatalf("expected %d messages to be seeded, got %d", totalMessages, len(seedResult.Messages))
	}

	if !seedResult.ChannelStateUpdated {
		t.Fatalf("expected channel state to be updated")
	}

	if len(seedResult.BucketsPopulated) == 0 {
		t.Fatalf("expected buckets to be populated, got none")
	}

	state := getChannelState(t, client, channelID, account.Token)
	if !state.Exists {
		t.Fatalf("expected channel state to exist")
	}
	if !state.HasMessages {
		t.Fatalf("expected channel state to show has_messages = true")
	}
	if state.LastMessageID == nil {
		t.Fatalf("expected channel state to have last_message_id")
	}

	buckets := getChannelBuckets(t, client, channelID, account.Token)
	if buckets.Count == 0 {
		t.Fatalf("expected bucket index to be populated")
	}

	// Fetch latest 25 messages via the API
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

	for i, msg := range fetchedMessages {
		if parseSnowflake(t, msg.ChannelID) != channelID {
			t.Fatalf("message[%d] has wrong channel_id: expected %d, got %s", i, channelID, msg.ChannelID)
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
}
