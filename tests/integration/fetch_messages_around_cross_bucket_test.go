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
	"testing"
	"time"
)

// TestFetchMessagesAroundCrossBucket verifies that fetchMessagesAround correctly
// traverses into older buckets when the "around" message's bucket doesn't have
// enough messages to satisfy the limit.
//
// Bug scenario: With limit=50 and messages spanning multiple 10-day buckets,
// the function was returning only messages from the around message's bucket
// due to restrictToBeforeBucket: true preventing traversal.
func TestFetchMessagesAroundCrossBucket(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	ensureSessionStarted(t, client, owner.Token)

	guild := createGuild(t, client, owner.Token, "Around Cross Bucket Test Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	now := time.Now()

	oldestBucketTimestamps := make([]time.Time, 5)
	for i := 0; i < 5; i++ {
		oldestBucketTimestamps[i] = now.Add(-25*24*time.Hour + time.Duration(i)*time.Minute)
	}
	oldestSeeded := seedMessagesAtTimestamps(t, client, channelID, oldestBucketTimestamps, owner.UserID)

	olderBucketTimestamps := make([]time.Time, 5)
	for i := 0; i < 5; i++ {
		olderBucketTimestamps[i] = now.Add(-15*24*time.Hour + time.Duration(i)*time.Minute)
	}
	olderSeeded := seedMessagesAtTimestamps(t, client, channelID, olderBucketTimestamps, owner.UserID)

	currentBucketTimestamps := make([]time.Time, 3)
	for i := 0; i < 3; i++ {
		currentBucketTimestamps[i] = now.Add(-time.Duration(i) * time.Minute)
	}
	currentSeeded := seedMessagesAtTimestamps(t, client, channelID, currentBucketTimestamps, owner.UserID)

	allBuckets := make(map[int]bool)
	for _, msg := range oldestSeeded.Messages {
		allBuckets[msg.Bucket] = true
	}
	for _, msg := range olderSeeded.Messages {
		allBuckets[msg.Bucket] = true
	}
	for _, msg := range currentSeeded.Messages {
		allBuckets[msg.Bucket] = true
	}
	if len(allBuckets) < 2 {
		t.Fatalf("expected messages to span at least 2 buckets, got %d bucket(s)", len(allBuckets))
	}

	aroundMessageID := currentSeeded.Messages[0].MessageID

	limit := 20
	messages := getChannelMessagesAround(t, client, owner.Token, channelID, aroundMessageID, limit)

	totalSeeded := len(oldestSeeded.Messages) + len(olderSeeded.Messages) + len(currentSeeded.Messages)
	expectedCount := totalSeeded
	if expectedCount > limit {
		expectedCount = limit
	}

	if len(messages) != expectedCount {
		t.Fatalf("expected %d messages, got %d", expectedCount, len(messages))
	}

	foundOldestBucket := false
	foundOlderBucket := false
	for _, msg := range messages {
		for _, seeded := range oldestSeeded.Messages {
			if msg.ID == seeded.MessageID {
				foundOldestBucket = true
				break
			}
		}
		for _, seeded := range olderSeeded.Messages {
			if msg.ID == seeded.MessageID {
				foundOlderBucket = true
				break
			}
		}
	}

	if !foundOldestBucket {
		t.Error("did not find any messages from the oldest bucket (25 days ago)")
	}
	if !foundOlderBucket {
		t.Error("did not find any messages from the older bucket (15 days ago)")
	}
}
