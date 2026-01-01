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
	"encoding/json"
	"fmt"
	"net/http"
	"testing"
	"time"
)

// TestMessageAttachmentDelete_LastAttachmentWithContent verifies that deleting the last
// attachment from a message that has text content updates the message but doesn't delete it
func TestMessageAttachmentDelete_LastAttachmentWithContent(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	gateway := newGatewayClient(t, client, user.Token)
	t.Cleanup(gateway.Close)

	guild := createGuild(t, client, user.Token, "Attachment Delete Test Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	msg, attachmentID := sendChannelMessageWithAttachment(t, client, user.Token, channelID, "This message has content", "yeah.png")
	messageID := parseSnowflake(t, msg.ID)

	resp, err := client.deleteJSONWithAuth(
		fmt.Sprintf("/channels/%d/messages/%d/attachments/%d", channelID, messageID, attachmentID),
		nil,
		user.Token,
	)
	if err != nil {
		t.Fatalf("failed to delete attachment: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	gateway.WaitForEvent(t, "MESSAGE_UPDATE", 10*time.Second, func(data json.RawMessage) bool {
		var update struct {
			ID          string `json:"id"`
			ChannelID   string `json:"channel_id"`
			Content     string `json:"content"`
			Attachments []any  `json:"attachments"`
		}
		if err := json.Unmarshal(data, &update); err != nil {
			return false
		}
		if update.ID != msg.ID || update.ChannelID != msg.ChannelID {
			return false
		}
		if update.Content != "This message has content" {
			t.Errorf("expected content to remain, got %q", update.Content)
		}
		if len(update.Attachments) != 0 {
			t.Errorf("expected 0 attachments, got %d", len(update.Attachments))
		}
		return true
	})

	resp, err = client.getWithAuth(fmt.Sprintf("/channels/%d/messages/%d", channelID, messageID), user.Token)
	if err != nil {
		t.Fatalf("failed to get message: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()
}
