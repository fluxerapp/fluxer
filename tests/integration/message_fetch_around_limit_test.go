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
	"sort"
	"testing"
	"time"
)

func TestMessageFetchAroundLimit(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	guild := createGuild(t, client, account.Token, "Message Fetch Around Limit")
	channelID := parseSnowflake(t, guild.SystemChannel)

	clearChannelMessages(t, client, channelID, account.Token)

	const totalMessages = 60
	seedResult := seedMessagesWithContent(t, client, channelID, totalMessages, account.UserID)
	if len(seedResult.Messages) != totalMessages {
		t.Fatalf("expected %d seeded messages, got %d", totalMessages, len(seedResult.Messages))
	}

	const limit = 50
	olderCount := limit / 2
	newerCount := limit - 1 - olderCount

	type timedMessage struct {
		ID        string
		Timestamp time.Time
	}

	ordered := make([]timedMessage, len(seedResult.Messages))
	for i, message := range seedResult.Messages {
		ts, err := time.Parse(time.RFC3339Nano, message.Timestamp)
		if err != nil {
			t.Fatalf("failed to parse message timestamp %q: %v", message.Timestamp, err)
		}
		ordered[i] = timedMessage{ID: message.MessageID, Timestamp: ts}
	}

	sort.Slice(ordered, func(i, j int) bool {
		return ordered[i].Timestamp.Before(ordered[j].Timestamp)
	})

	if olderCount+newerCount+1 > len(ordered) {
		t.Fatalf("not enough seeded messages (%d) for older=%d and newer=%d requests", len(ordered), olderCount, newerCount)
	}

	anchorIndex := olderCount + 5
	if anchorIndex+newerCount >= len(ordered) {
		t.Fatalf("anchor index %d too close to end for %d newer messages", anchorIndex, newerCount)
	}

	anchorMessageID := ordered[anchorIndex].ID

	resp, err := client.getWithAuth(
		fmt.Sprintf("/channels/%d/messages?limit=%d&around=%s", channelID, limit, anchorMessageID),
		account.Token,
	)
	if err != nil {
		t.Fatalf("failed to fetch messages around anchor: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var fetchedMessages []struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &fetchedMessages)

	if len(fetchedMessages) != limit {
		t.Fatalf("expected %d messages, got %d", limit, len(fetchedMessages))
	}

	anchorPos := -1
	for i, msg := range fetchedMessages {
		if msg.ID == anchorMessageID {
			anchorPos = i
			break
		}
	}
	if anchorPos == -1 {
		t.Fatalf("anchor message %s not present in response", anchorMessageID)
	}

	if anchorPos != newerCount {
		t.Fatalf("expected %d messages before anchor, got %d", newerCount, anchorPos)
	}

	afterCount := len(fetchedMessages) - anchorPos - 1
	if afterCount != olderCount {
		t.Fatalf("expected %d messages after anchor, got %d", olderCount, afterCount)
	}

	expectedNewerIDs := make(map[string]struct{})
	for i := anchorIndex + 1; i <= anchorIndex+newerCount; i++ {
		expectedNewerIDs[ordered[i].ID] = struct{}{}
	}

	expectedOlderIDs := make(map[string]struct{})
	for i := anchorIndex - olderCount; i < anchorIndex; i++ {
		expectedOlderIDs[ordered[i].ID] = struct{}{}
	}

	actualNewerIDs := make(map[string]struct{})
	actualNewer := fetchedMessages[:anchorPos]
	for i, msg := range actualNewer {
		actualNewerIDs[msg.ID] = struct{}{}
		msgValue := parseSnowflake(t, msg.ID)
		anchorValue := parseSnowflake(t, anchorMessageID)
		if msgValue <= anchorValue {
			t.Fatalf("newer message %s has snowflake %d <= anchor %s (%d)", msg.ID, msgValue, anchorMessageID, anchorValue)
		}
		if i > 0 {
			prevValue := parseSnowflake(t, actualNewer[i-1].ID)
			if prevValue <= msgValue {
				t.Fatalf("newer messages not in descending order: %d <= %d", prevValue, msgValue)
			}
		}
	}

	actualOlderIDs := make(map[string]struct{})
	actualOlder := fetchedMessages[anchorPos+1:]
	for i, msg := range actualOlder {
		actualOlderIDs[msg.ID] = struct{}{}
		msgValue := parseSnowflake(t, msg.ID)
		anchorValue := parseSnowflake(t, anchorMessageID)
		if msgValue >= anchorValue {
			t.Fatalf("older message %s has snowflake %d >= anchor %s (%d)", msg.ID, msgValue, anchorMessageID, anchorValue)
		}
		if i > 0 {
			prevValue := parseSnowflake(t, actualOlder[i-1].ID)
			if prevValue <= msgValue {
				t.Fatalf("older messages not in descending order: %d <= %d", prevValue, msgValue)
			}
		}
	}

	if len(actualNewerIDs) != newerCount {
		t.Fatalf("expected %d unique newer messages, got %d", newerCount, len(actualNewerIDs))
	}
	if len(actualOlderIDs) != olderCount {
		t.Fatalf("expected %d unique older messages, got %d", olderCount, len(actualOlderIDs))
	}

	for id := range expectedNewerIDs {
		if _, ok := actualNewerIDs[id]; !ok {
			t.Fatalf("expected newer message %s missing", id)
		}
	}
	for id := range expectedOlderIDs {
		if _, ok := actualOlderIDs[id]; !ok {
			t.Fatalf("expected older message %s missing", id)
		}
	}

	t.Logf("received %d messages around anchor %s (newer=%d, older=%d)", len(fetchedMessages), anchorMessageID, newerCount, olderCount)
}
