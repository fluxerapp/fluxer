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
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"strings"
	"testing"
	"time"

	"github.com/redis/go-redis/v9"
)

const assetDeletionQueueKey = "asset:deletion:queue"

func TestExpireAttachmentsOnlyDeletesExpiredRecords(t *testing.T) {
	client := newTestClient(t)
	ctx := context.Background()

	redisURL := os.Getenv("REDIS_URL")
	if redisURL == "" {
		redisURL = "redis://localhost:6379"
	}

	redisOpts, err := redis.ParseURL(redisURL)
	if err != nil {
		t.Fatalf("failed to parse redis url: %v", err)
	}

	redisClient := redis.NewClient(redisOpts)
	if err := redisClient.Ping(ctx).Err(); err != nil {
		_ = redisClient.Close()
		t.Skipf("redis not available: %v", err)
	}
	defer redisClient.Close()

	defer func() {
		if err := redisClient.Del(ctx, assetDeletionQueueKey).Err(); err != nil {
			t.Logf("failed to clear queue after test: %v", err)
		}
	}()

	if err := redisClient.Del(ctx, assetDeletionQueueKey).Err(); err != nil {
		t.Fatalf("failed to clear queue: %v", err)
	}

	resp, err := client.postJSON("/test/attachment-decay/clear", nil)
	if err != nil {
		t.Fatalf("failed to clear attachment decay records: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	now := time.Now().UTC()

	expiredAt := now.AddDate(0, 0, -2)
	futureAt := now.AddDate(0, 0, 2)

	expiredAttachmentID := fmt.Sprintf("%d", expiredAt.UnixNano())
	futureAttachmentID := fmt.Sprintf("%d", futureAt.UnixNano())
	channelID := fmt.Sprintf("%d", now.Unix())
	messageID := fmt.Sprintf("%d", now.Unix()+2)

	rowsPayload := map[string]any{
		"rows": []map[string]any{
			{
				"attachment_id":    expiredAttachmentID,
				"channel_id":       channelID,
				"message_id":       messageID,
				"expires_at":       expiredAt.Format(time.RFC3339Nano),
				"uploaded_at":      expiredAt.Add(-24 * time.Hour).Format(time.RFC3339Nano),
				"last_accessed_at": expiredAt.Add(-12 * time.Hour).Format(time.RFC3339Nano),
				"filename":         "expired.bin",
				"size_bytes":       "2048",
				"cost":             1,
				"lifetime_days":    1,
			},
			{
				"attachment_id":    futureAttachmentID,
				"channel_id":       channelID,
				"message_id":       messageID,
				"expires_at":       futureAt.Format(time.RFC3339Nano),
				"uploaded_at":      futureAt.Add(-24 * time.Hour).Format(time.RFC3339Nano),
				"last_accessed_at": futureAt.Add(-12 * time.Hour).Format(time.RFC3339Nano),
				"filename":         "future.bin",
				"size_bytes":       "4096",
				"cost":             1,
				"lifetime_days":    1,
			},
		},
	}

	resp, err = client.postJSON("/test/attachment-decay/rows", rowsPayload)
	if err != nil {
		t.Fatalf("failed to insert rows: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	resp, err = client.postJSON("/test/worker/expire-attachments", nil)
	if err != nil {
		t.Fatalf("failed to trigger expireAttachments worker: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	queueEntries, err := redisClient.LRange(ctx, assetDeletionQueueKey, 0, -1).Result()
	if err != nil {
		t.Fatalf("failed to read queue: %v", err)
	}
	if len(queueEntries) != 1 {
		t.Fatalf("expected 1 queued deletion, got %d", len(queueEntries))
	}

	var queued struct {
		S3Key string `json:"s3Key"`
	}
	if err := json.Unmarshal([]byte(queueEntries[0]), &queued); err != nil {
		t.Fatalf("failed to parse queue entry: %v", err)
	}

	parts := strings.Split(queued.S3Key, "/")
	if len(parts) < 3 {
		t.Fatalf("unexpected s3Key format: %s", queued.S3Key)
	}
	if parts[2] != expiredAttachmentID {
		t.Fatalf("expected queued attachment %s but got %s", expiredAttachmentID, parts[2])
	}

	resp, err = client.get("/test/attachment-decay/" + futureAttachmentID)
	if err != nil {
		t.Fatalf("failed to fetch attachment decay row: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var fetchResp struct {
		Row *struct {
			AttachmentID string `json:"attachment_id"`
			ExpiresAt    string `json:"expires_at"`
		} `json:"row"`
	}
	decodeJSONResponse(t, resp, &fetchResp)

	if fetchResp.Row == nil {
		t.Fatalf("expected future row to persist but it was deleted")
	}
	if fetchResp.Row.AttachmentID != futureAttachmentID {
		t.Fatalf("expected future attachment %s, got %s", futureAttachmentID, fetchResp.Row.AttachmentID)
	}
}
