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

// TestMessageAttachmentDelete_LastAttachmentWithEmbeds verifies that deleting the last
// attachment from a message that has embeds updates the message but doesn't delete it
func TestMessageAttachmentDelete_LastAttachmentWithEmbeds(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	gateway := newGatewayClient(t, client, user.Token)
	t.Cleanup(gateway.Close)

	guild := createGuild(t, client, user.Token, "Attachment Embed Test Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	msg, attachmentID := sendChannelMessageWithAttachment(t, client, user.Token, channelID, "Message with attachment", "yeah.png")
	messageID := parseSnowflake(t, msg.ID)

	editPayload := map[string]any{
		"content": "",
		"embeds": []map[string]any{
			{
				"title":       "Test Embed",
				"description": "This message has an embed",
			},
		},
	}
	resp, err := client.patchJSONWithAuth(
		fmt.Sprintf("/channels/%d/messages/%d", channelID, messageID),
		editPayload,
		user.Token,
	)
	if err != nil {
		t.Fatalf("failed to edit message: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	gateway.WaitForEvent(t, "MESSAGE_UPDATE", 10*time.Second, func(data json.RawMessage) bool {
		var update struct {
			ID string `json:"id"`
		}
		json.Unmarshal(data, &update)
		return update.ID == msg.ID
	})

	resp, err = client.deleteJSONWithAuth(
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
			Embeds      []any  `json:"embeds"`
			Attachments []any  `json:"attachments"`
		}
		if err := json.Unmarshal(data, &update); err != nil {
			return false
		}
		if update.ID != msg.ID {
			return false
		}
		if len(update.Embeds) == 0 {
			t.Error("expected embed to remain")
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
