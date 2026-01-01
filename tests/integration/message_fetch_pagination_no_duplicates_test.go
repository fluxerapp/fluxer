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

func TestMessageFetchPaginationNoDuplicates(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	guild := createGuild(t, client, account.Token, "Pagination No Duplicates Test Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	clearChannelMessages(t, client, channelID, account.Token)

	// Seed 100 messages
	const totalMessages = 100
	seedResult := seedMessagesWithContent(t, client, channelID, totalMessages, account.UserID)

	if len(seedResult.Messages) != totalMessages {
		t.Fatalf("expected %d messages to be seeded, got %d", totalMessages, len(seedResult.Messages))
	}

	// Paginate through all messages with limit=25
	const limit = 25
	allMessageIDs := make(map[string]bool)
	var lastMessageID string
	pageCount := 0

	for {
		var url string
		if lastMessageID == "" {
			url = fmt.Sprintf("/channels/%d/messages?limit=%d", channelID, limit)
		} else {
			url = fmt.Sprintf("/channels/%d/messages?limit=%d&before=%s", channelID, limit, lastMessageID)
		}

		resp, err := client.getWithAuth(url, account.Token)
		if err != nil {
			t.Fatalf("failed to fetch messages on page %d: %v", pageCount+1, err)
		}
		assertStatus(t, resp, http.StatusOK)

		var fetchedMessages []struct {
			ID        string `json:"id"`
			ChannelID string `json:"channel_id"`
			Content   string `json:"content"`
		}
		decodeJSONResponse(t, resp, &fetchedMessages)

		if len(fetchedMessages) == 0 {
			break
		}

		pageCount++

		t.Logf("Page %d: fetched %d messages", pageCount, len(fetchedMessages))

		for i, msg := range fetchedMessages {
			if allMessageIDs[msg.ID] {
				t.Fatalf("duplicate message ID %s found on page %d at index %d", msg.ID, pageCount, i)
			}
			allMessageIDs[msg.ID] = true
		}

		for i := 0; i < len(fetchedMessages)-1; i++ {
			current := parseSnowflake(t, fetchedMessages[i].ID)
			next := parseSnowflake(t, fetchedMessages[i+1].ID)
			if current <= next {
				t.Fatalf("page %d: messages not in descending order: message[%d] (%d) <= message[%d] (%d)", pageCount, i, current, i+1, next)
			}
		}

		lastMessageID = fetchedMessages[len(fetchedMessages)-1].ID

		if pageCount > 10 {
			t.Fatalf("pagination exceeded expected number of pages (expected ~4 pages for 100 messages with limit 25)")
		}

		if len(fetchedMessages) < limit {
			break
		}
	}

	if len(allMessageIDs) != totalMessages {
		t.Fatalf("expected to collect %d unique messages, got %d", totalMessages, len(allMessageIDs))
	}

	for _, seededMsg := range seedResult.Messages {
		if !allMessageIDs[seededMsg.MessageID] {
			t.Fatalf("seeded message ID %s was not retrieved during pagination", seededMsg.MessageID)
		}
	}

	expectedPages := 4
	if pageCount != expectedPages {
		t.Fatalf("expected %d pages, got %d", expectedPages, pageCount)
	}

	t.Logf("Pagination test passed: retrieved %d unique messages across %d pages with no duplicates", len(allMessageIDs), pageCount)
}
