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

func TestAttachmentDecayAppliesToSnapshotClones(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)
	user3 := createTestAccount(t, client)
	ensureSessionStarted(t, client, user1.Token)
	ensureSessionStarted(t, client, user2.Token)
	ensureSessionStarted(t, client, user3.Token)

	createFriendship(t, client, user1, user2)
	createFriendship(t, client, user1, user3)

	channel1 := createDmChannel(t, client, user1.Token, parseSnowflake(t, user2.UserID))
	channel2 := createDmChannel(t, client, user1.Token, parseSnowflake(t, user3.UserID))

	originalMessage, _ := sendChannelMessageWithAttachment(
		t,
		client,
		user1.Token,
		parseSnowflake(t, channel1.ID),
		"Original with attachment",
		"yeah.png",
	)

	forwardPayload := map[string]any{
		"message_reference": map[string]any{
			"message_id": originalMessage.ID,
			"channel_id": channel1.ID,
			"type":       1,
		},
	}

	resp, err := client.postJSONWithAuth(
		fmt.Sprintf("/channels/%d/messages", parseSnowflake(t, channel2.ID)),
		forwardPayload,
		user1.Token,
	)
	if err != nil {
		t.Fatalf("failed to forward message: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var forwarded messageResponse
	decodeJSONResponse(t, resp, &forwarded)

	fetchedSnapshots := fetchMessageSnapshots(t, client, user1.Token, parseSnowflake(t, channel2.ID), parseSnowflake(t, forwarded.ID))
	if len(fetchedSnapshots) == 0 {
		t.Fatalf("expected forwarded message to contain snapshots")
	}

	if len(fetchedSnapshots[0].Attachments) == 0 {
		t.Fatalf("expected snapshot to include attachments")
	}

	now := time.Now().UTC()
	rows := []map[string]any{}
	for _, attachment := range fetchedSnapshots[0].Attachments {
		rows = append(rows, map[string]any{
			"attachment_id":    attachment.ID,
			"channel_id":       channel2.ID,
			"message_id":       forwarded.ID,
			"expires_at":       now.Add(-1 * time.Hour).Format(time.RFC3339Nano),
			"uploaded_at":      now.Add(-2 * time.Hour).Format(time.RFC3339Nano),
			"last_accessed_at": now.Add(-1 * time.Hour).Format(time.RFC3339Nano),
			"filename":         "expired-snapshot.bin",
			"size_bytes":       1024,
			"cost":             1,
			"lifetime_days":    1,
		})
	}

	resp, err = client.postJSONWithAuth("/test/attachment-decay/rows", map[string]any{"rows": rows}, user1.Token)
	if err != nil {
		t.Fatalf("failed to seed attachment decay rows: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	fetchedSnapshots = fetchMessageSnapshots(t, client, user1.Token, parseSnowflake(t, channel2.ID), parseSnowflake(t, forwarded.ID))
	if len(fetchedSnapshots[0].Attachments) == 0 {
		t.Fatalf("expected snapshot to include attachments after update")
	}

	for _, attachment := range fetchedSnapshots[0].Attachments {
		if attachment.URL != nil {
			t.Fatalf("expected expired snapshot attachment to hide url")
		}
		if attachment.Expired == nil || !*attachment.Expired {
			t.Fatalf("expected snapshot attachment to be marked expired")
		}
		if attachment.ExpiresAt == nil {
			t.Fatalf("expected snapshot attachment to include expires_at")
		}
	}
}

type snapshotAttachment struct {
	ID        string  `json:"id"`
	URL       *string `json:"url"`
	Expired   *bool   `json:"expired"`
	ExpiresAt *string `json:"expires_at"`
}

type messageSnapshotResponse struct {
	Attachments []snapshotAttachment `json:"attachments"`
}

func fetchMessageSnapshots(t testing.TB, client *testClient, token string, channelID, messageID int64) []messageSnapshotResponse {
	t.Helper()

	resp, err := client.getWithAuth(
		fmt.Sprintf("/channels/%d/messages/%d", channelID, messageID),
		token,
	)
	if err != nil {
		t.Fatalf("failed to fetch message: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var fetched struct {
		MessageSnapshots []messageSnapshotResponse `json:"message_snapshots"`
	}
	decodeJSONResponse(t, resp, &fetched)
	return fetched.MessageSnapshots
}
