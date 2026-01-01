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

func TestMessageFetchEmptyChannel(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	guild := createGuild(t, client, account.Token, "Empty Channel Test Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	clearChannelMessages(t, client, channelID, account.Token)

	state := getChannelState(t, client, channelID, account.Token)
	if state.HasMessages {
		t.Fatalf("expected channel state to show has_messages = false, got true")
	}
	if state.LastMessageID != nil {
		t.Fatalf("expected channel state to have no last_message_id, got %s", *state.LastMessageID)
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

	if len(fetchedMessages) != 0 {
		t.Fatalf("expected empty array for channel with no messages, got %d messages", len(fetchedMessages))
	}

	buckets := getChannelBuckets(t, client, channelID, account.Token)
	if buckets.Count != 0 {
		t.Fatalf("expected no buckets for empty channel, got %d buckets", buckets.Count)
	}
}
