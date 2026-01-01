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

func TestMessageFetchBetweenCrossBucket(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	guild := createGuild(t, client, account.Token, "Between Cross Bucket Test")
	channelID := parseSnowflake(t, guild.SystemChannel)
	clearChannelMessages(t, client, channelID, account.Token)

	timestamps := []time.Time{
		time.Now().Add(-time.Hour * 24 * 30),
		time.Now().Add(-time.Hour * 24 * 20),
		time.Now().Add(-time.Hour * 24 * 10),
		time.Now(),
	}

	seed := seedMessagesAtTimestamps(t, client, channelID, timestamps, account.UserID)

	resp, err := client.getWithAuth(
		fmt.Sprintf(
			"/channels/%d/messages?after=%s&before=%s",
			channelID,
			seed.Messages[0].MessageID,
			seed.Messages[3].MessageID,
		),
		account.Token,
	)
	if err != nil {
		t.Fatalf("fetch failed: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var messages []struct{ ID string }
	decodeJSONResponse(t, resp, &messages)

	if len(messages) != 2 {
		t.Fatalf("expected 2 messages, got %d", len(messages))
	}
}
