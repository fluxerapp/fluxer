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

func TestMessageBulkDeleteAcrossBuckets(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	guild := createGuild(t, client, account.Token, "Bulk Delete Buckets Test Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	clearChannelMessages(t, client, channelID, account.Token)

	// Seed messages in 3 buckets
	// We'll create timestamps that span across different buckets
	// Assuming buckets are time-based (e.g., daily or weekly buckets)
	const messagesPerBucket = 20
	const totalBuckets = 3
	const totalMessages = messagesPerBucket * totalBuckets

	timestamps := make([]time.Time, totalMessages)
	baseTime := time.Now().Add(-time.Hour * 24 * 90)

	for bucketIdx := 0; bucketIdx < totalBuckets; bucketIdx++ {
		for msgIdx := 0; msgIdx < messagesPerBucket; msgIdx++ {
			idx := bucketIdx*messagesPerBucket + msgIdx
			timestamps[idx] = baseTime.Add(time.Hour * 24 * 30 * time.Duration(bucketIdx)).Add(time.Hour * time.Duration(msgIdx))
		}
	}

	seedResult := seedMessagesAtTimestamps(t, client, channelID, timestamps, account.UserID)

	if len(seedResult.Messages) != totalMessages {
		t.Fatalf("expected %d messages to be seeded, got %d", totalMessages, len(seedResult.Messages))
	}

	if len(seedResult.BucketsPopulated) != totalBuckets {
		t.Fatalf("expected %d buckets to be populated, got %d", totalBuckets, len(seedResult.BucketsPopulated))
	}

	bucketMessages := make(map[int][]string)
	for _, msg := range seedResult.Messages {
		bucketMessages[msg.Bucket] = append(bucketMessages[msg.Bucket], msg.MessageID)
	}

	if len(bucketMessages) != totalBuckets {
		t.Fatalf("expected messages in %d distinct buckets, got %d", totalBuckets, len(bucketMessages))
	}

	bucketIDs := seedResult.BucketsPopulated
	if len(bucketIDs) != totalBuckets {
		t.Fatalf("expected %d bucket IDs, got %d", totalBuckets, len(bucketIDs))
	}

	sort.Ints(bucketIDs)
	firstBucket := bucketIDs[0]
	middleBucket := bucketIDs[1]
	lastBucket := bucketIDs[2]

	middleBucketMessageIDs := bucketMessages[middleBucket]
	if len(middleBucketMessageIDs) == 0 {
		t.Fatalf("expected messages in middle bucket, got none")
	}

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages/bulk-delete", channelID), map[string]any{
		"message_ids": middleBucketMessageIDs,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to bulk delete messages: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	bucketsAfterDelete := getChannelBuckets(t, client, channelID, account.Token)

	resp, err = client.getWithAuth(fmt.Sprintf("/channels/%d/messages?limit=100", channelID), account.Token)
	if err != nil {
		t.Fatalf("failed to fetch messages after bulk delete: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var fetchedMessages []struct {
		ID        string `json:"id"`
		ChannelID string `json:"channel_id"`
		Content   string `json:"content"`
	}
	decodeJSONResponse(t, resp, &fetchedMessages)

	expectedRemainingMessages := messagesPerBucket * 2
	if len(fetchedMessages) != expectedRemainingMessages {
		t.Fatalf("expected %d messages after deleting middle bucket, got %d", expectedRemainingMessages, len(fetchedMessages))
	}

	for _, msg := range fetchedMessages {
		for _, deletedID := range middleBucketMessageIDs {
			if msg.ID == deletedID {
				t.Fatalf("deleted message %s still appears in fetch results", msg.ID)
			}
		}
	}

	firstBucketMessageIDs := bucketMessages[firstBucket]
	lastBucketMessageIDs := bucketMessages[lastBucket]

	fetchedIDMap := make(map[string]bool)
	for _, msg := range fetchedMessages {
		fetchedIDMap[msg.ID] = true
	}

	for _, msgID := range firstBucketMessageIDs {
		if !fetchedIDMap[msgID] {
			t.Fatalf("message %s from first bucket should still exist", msgID)
		}
	}

	for _, msgID := range lastBucketMessageIDs {
		if !fetchedIDMap[msgID] {
			t.Fatalf("message %s from last bucket should still exist", msgID)
		}
	}

	if bucketsAfterDelete.Count != 2 {
		t.Fatalf("expected exactly 2 buckets after deleting middle bucket, got %d", bucketsAfterDelete.Count)
	}

	for _, bucket := range bucketsAfterDelete.Buckets {
		if bucket.Bucket == middleBucket {
			t.Fatalf("middle bucket %d should not be in bucket index after deletion", middleBucket)
		}
	}

	foundFirst, foundLast := false, false
	for _, bucket := range bucketsAfterDelete.Buckets {
		if bucket.Bucket == firstBucket {
			foundFirst = true
		}
		if bucket.Bucket == lastBucket {
			foundLast = true
		}
	}
	if !foundFirst {
		t.Fatalf("first bucket %d should still be in bucket index", firstBucket)
	}
	if !foundLast {
		t.Fatalf("last bucket %d should still be in bucket index", lastBucket)
	}

	emptyBuckets := getChannelEmptyBuckets(t, client, channelID, account.Token)
	foundMiddleInEmpty := false
	for _, bucket := range emptyBuckets.EmptyBuckets {
		if bucket.Bucket == middleBucket {
			foundMiddleInEmpty = true
			break
		}
	}
	if !foundMiddleInEmpty {
		t.Fatalf("middle bucket %d should be marked as empty in channel_empty_buckets", middleBucket)
	}

	state := getChannelState(t, client, channelID, account.Token)
	if !state.HasMessages {
		t.Fatalf("expected channel state to show has_messages = true, got false")
	}
	if state.LastMessageID == nil {
		t.Fatalf("expected channel state to have last_message_id")
	}
}
