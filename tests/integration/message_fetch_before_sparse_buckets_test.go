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

func TestMessageFetchBeforeSparseBucketsBeyondIndexLimit(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	guild := createGuild(t, client, account.Token, "Before Sparse Buckets Test")
	channelID := parseSnowflake(t, guild.SystemChannel)
	clearChannelMessages(t, client, channelID, account.Token)

	const bucketCount = 60
	timestamps := make([]time.Time, bucketCount)

	base := time.Now().Add(-time.Hour * 24 * 10 * (bucketCount - 1))
	for i := 0; i < bucketCount; i++ {
		timestamps[i] = base.Add(time.Hour * 24 * 10 * time.Duration(i))
	}

	seed := seedMessagesAtTimestamps(t, client, channelID, timestamps, account.UserID)

	anchor := seed.Messages[bucketCount-1].MessageID // newest
	const limit = 50

	resp, err := client.getWithAuth(
		fmt.Sprintf("/channels/%d/messages?before=%s&limit=%d", channelID, anchor, limit),
		account.Token,
	)
	if err != nil {
		t.Fatalf("fetch failed: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var messages []struct{ ID string }
	decodeJSONResponse(t, resp, &messages)

	if len(messages) != limit {
		t.Fatalf("expected %d messages, got %d", limit, len(messages))
	}
}
