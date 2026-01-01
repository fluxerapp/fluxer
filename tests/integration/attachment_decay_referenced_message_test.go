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

func TestAttachmentDecayPopulatesReferencedMessage(t *testing.T) {
	client := newTestClient(t)
	author := createTestAccount(t, client)
	recipient := createTestAccount(t, client)
	ensureSessionStarted(t, client, author.Token)
	ensureSessionStarted(t, client, recipient.Token)

	createFriendship(t, client, author, recipient)

	channel := createDmChannel(t, client, author.Token, parseSnowflake(t, recipient.UserID))

	originalMessage, originalAttachmentID := sendChannelMessageWithAttachment(
		t,
		client,
		author.Token,
		parseSnowflake(t, channel.ID),
		"Original message for reference",
		"document.pdf",
	)

	replyPayload := map[string]any{
		"content": "Reply referencing expired attachment",
		"message_reference": map[string]any{
			"message_id": originalMessage.ID,
			"channel_id": channel.ID,
			"type":       0,
		},
	}

	resp, err := client.postJSONWithAuth(
		fmt.Sprintf("/channels/%d/messages", parseSnowflake(t, channel.ID)),
		replyPayload,
		author.Token,
	)
	if err != nil {
		t.Fatalf("failed to post reply: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var reply messageResponse
	decodeJSONResponse(t, resp, &reply)

	rows := []map[string]any{
		{
			"attachment_id":    fmt.Sprintf("%d", originalAttachmentID),
			"channel_id":       channel.ID,
			"message_id":       originalMessage.ID,
			"expires_at":       time.Now().Add(-1 * time.Hour).Format(time.RFC3339Nano),
			"uploaded_at":      time.Now().Add(-2 * time.Hour).Format(time.RFC3339Nano),
			"last_accessed_at": time.Now().Add(-1 * time.Hour).Format(time.RFC3339Nano),
			"filename":         "expired-reference.bin",
			"size_bytes":       2048,
			"cost":             2,
			"lifetime_days":    1,
		},
	}

	resp, err = client.postJSONWithAuth("/test/attachment-decay/rows", map[string]any{"rows": rows}, author.Token)
	if err != nil {
		t.Fatalf("failed to seed attachment decay rows: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	resp, err = client.getWithAuth(
		fmt.Sprintf("/test/messages/%d/%d/with-reference", parseSnowflake(t, channel.ID), parseSnowflake(t, reply.ID)),
		author.Token,
	)
	if err != nil {
		t.Fatalf("failed to fetch message with reference: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var fetched struct {
		ID                string `json:"id"`
		ReferencedMessage struct {
			Attachments []struct {
				ID        string  `json:"id"`
				URL       *string `json:"url"`
				Expired   *bool   `json:"expired"`
				ExpiresAt *string `json:"expires_at"`
			} `json:"attachments"`
		} `json:"referenced_message"`
	}
	decodeJSONResponse(t, resp, &fetched)

	if len(fetched.ReferencedMessage.Attachments) == 0 {
		t.Fatalf("expected referenced message to include attachments")
	}

	for _, attachment := range fetched.ReferencedMessage.Attachments {
		if attachment.URL != nil {
			t.Fatalf("expected referenced message attachment to hide url")
		}
		if attachment.Expired == nil || !*attachment.Expired {
			t.Fatalf("expected referenced message attachment to be marked expired")
		}
		if attachment.ExpiresAt == nil {
			t.Fatalf("expected referenced message attachment to include expires_at")
		}
	}
}
