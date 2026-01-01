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

// Response types for message seeding endpoints

type seedMessageResponse struct {
	MessageID string `json:"message_id"`
	Bucket    int    `json:"bucket"`
	Timestamp string `json:"timestamp"`
}

type seedMessagesResponse struct {
	Messages            []seedMessageResponse `json:"messages"`
	BucketsPopulated    []int                 `json:"buckets_populated"`
	ChannelStateUpdated bool                  `json:"channel_state_updated"`
}

type channelStateResponse struct {
	ChannelID         string  `json:"channel_id"`
	Exists            bool    `json:"exists"`
	CreatedBucket     int     `json:"created_bucket"`
	HasMessages       bool    `json:"has_messages"`
	LastMessageID     *string `json:"last_message_id"`
	LastMessageBucket *int    `json:"last_message_bucket"`
	UpdatedAt         string  `json:"updated_at"`
}

type bucketEntry struct {
	Bucket    int    `json:"bucket"`
	UpdatedAt string `json:"updated_at"`
}

type channelBucketsResponse struct {
	ChannelID string        `json:"channel_id"`
	Buckets   []bucketEntry `json:"buckets"`
	Count     int           `json:"count"`
}

type channelEmptyBucketsResponse struct {
	ChannelID    string        `json:"channel_id"`
	EmptyBuckets []bucketEntry `json:"empty_buckets"`
	Count        int           `json:"count"`
}

// Helper functions for message seeding

// seedMessagesAtTimestamps seeds messages at specific timestamps with bucket index populated
func seedMessagesAtTimestamps(t testing.TB, client *testClient, channelID int64, timestamps []time.Time, authorID string) seedMessagesResponse {
	t.Helper()

	messages := make([]map[string]interface{}, len(timestamps))
	for i, ts := range timestamps {
		messages[i] = map[string]interface{}{
			"timestamp": ts.Format(time.RFC3339Nano),
			"content":   fmt.Sprintf("Test message %d", i+1),
		}
	}

	payload := map[string]interface{}{
		"channel_id":        fmt.Sprintf("%d", channelID),
		"messages":          messages,
		"skip_bucket_index": false,
		"author_id":         authorID,
	}

	resp, err := client.postJSON("/test/messages/seed", payload)
	if err != nil {
		t.Fatalf("failed to seed messages: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var result seedMessagesResponse
	decodeJSONResponse(t, resp, &result)
	return result
}

// seedMessagesWithContent seeds a specific number of messages with generated content and timestamps
func seedMessagesWithContent(t testing.TB, client *testClient, channelID int64, count int, authorID string) seedMessagesResponse {
	t.Helper()

	messages := make([]map[string]interface{}, count)
	baseTime := time.Now().Add(-time.Hour * 24)
	for i := 0; i < count; i++ {
		messages[i] = map[string]interface{}{
			"timestamp": baseTime.Add(time.Minute * time.Duration(i)).Format(time.RFC3339Nano),
			"content":   fmt.Sprintf("Test message %d", i+1),
		}
	}

	payload := map[string]interface{}{
		"channel_id":        fmt.Sprintf("%d", channelID),
		"messages":          messages,
		"skip_bucket_index": false,
		"author_id":         authorID,
	}

	resp, err := client.postJSON("/test/messages/seed", payload)
	if err != nil {
		t.Fatalf("failed to seed messages: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var result seedMessagesResponse
	decodeJSONResponse(t, resp, &result)
	return result
}

// seedMessagesRaw seeds messages at specific timestamps without populating bucket index (for legacy scan tests)
func seedMessagesRaw(t testing.TB, client *testClient, channelID int64, timestamps []time.Time, authorID string) seedMessagesResponse {
	t.Helper()

	messages := make([]map[string]interface{}, len(timestamps))
	for i, ts := range timestamps {
		messages[i] = map[string]interface{}{
			"timestamp": ts.Format(time.RFC3339Nano),
			"content":   fmt.Sprintf("Test message %d", i+1),
		}
	}

	payload := map[string]interface{}{
		"channel_id":        fmt.Sprintf("%d", channelID),
		"messages":          messages,
		"skip_bucket_index": true,
		"author_id":         authorID,
	}

	resp, err := client.postJSON("/test/messages/seed", payload)
	if err != nil {
		t.Fatalf("failed to seed messages: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var result seedMessagesResponse
	decodeJSONResponse(t, resp, &result)
	return result
}

// getChannelState retrieves the channel state for verification
func getChannelState(t testing.TB, client *testClient, channelID int64, token string) channelStateResponse {
	t.Helper()

	resp, err := client.get(fmt.Sprintf("/test/channels/%d/state", channelID))
	if err != nil {
		t.Fatalf("failed to get channel state: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var state channelStateResponse
	decodeJSONResponse(t, resp, &state)
	return state
}

// getChannelBuckets retrieves the bucket index for verification
func getChannelBuckets(t testing.TB, client *testClient, channelID int64, token string) channelBucketsResponse {
	t.Helper()

	resp, err := client.get(fmt.Sprintf("/test/channels/%d/buckets", channelID))
	if err != nil {
		t.Fatalf("failed to get channel buckets: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var buckets channelBucketsResponse
	decodeJSONResponse(t, resp, &buckets)
	return buckets
}

// clearChannelMessages clears all messages and indexes for a channel
func clearChannelMessages(t testing.TB, client *testClient, channelID int64, token string) {
	t.Helper()

	resp, err := client.delete(fmt.Sprintf("/test/channels/%d/messages", channelID), "")
	if err != nil {
		t.Fatalf("failed to clear channel messages: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()
}

// getChannelEmptyBuckets retrieves the empty bucket markers for verification
func getChannelEmptyBuckets(t testing.TB, client *testClient, channelID int64, token string) channelEmptyBucketsResponse {
	t.Helper()

	resp, err := client.get(fmt.Sprintf("/test/channels/%d/empty-buckets", channelID))
	if err != nil {
		t.Fatalf("failed to get channel empty buckets: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var emptyBuckets channelEmptyBucketsResponse
	decodeJSONResponse(t, resp, &emptyBuckets)
	return emptyBuckets
}
