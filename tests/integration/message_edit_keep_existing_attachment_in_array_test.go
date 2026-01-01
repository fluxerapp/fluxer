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
)

// TestMessageEdit_KeepExistingAttachmentInArray tests that when editing a message
// and explicitly including the existing attachment in the attachments array,
// it is preserved correctly
func TestMessageEdit_KeepExistingAttachmentInArray(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	guild := createGuild(t, client, user.Token, "Keep Attachment Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	msg, attachmentID := sendChannelMessageWithAttachment(t, client, user.Token, channelID, "Original message", "yeah.png")

	updatePayload := map[string]any{
		"content": "Edited, keeping attachment",
		"attachments": []map[string]any{
			{
				"id":       0,
				"filename": "yeah.png",
			},
		},
	}

	resp, err := client.patchJSONWithAuth(
		fmt.Sprintf("/channels/%d/messages/%s", channelID, msg.ID),
		updatePayload,
		user.Token,
	)
	if err != nil {
		t.Fatalf("failed to edit message: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusOK)

	var edited struct {
		Content     string `json:"content"`
		Attachments []struct {
			ID       string `json:"id"`
			Filename string `json:"filename"`
		} `json:"attachments"`
	}
	decodeJSONResponse(t, resp, &edited)

	if edited.Content != "Edited, keeping attachment" {
		t.Errorf("expected content 'Edited, keeping attachment', got %q", edited.Content)
	}

	if len(edited.Attachments) != 1 {
		t.Fatalf("expected 1 attachment, got %d", len(edited.Attachments))
	}

	editedAttachmentID := parseSnowflake(t, edited.Attachments[0].ID)
	if editedAttachmentID != attachmentID {
		t.Errorf("expected attachment ID %d, got %d", attachmentID, editedAttachmentID)
	}

	if edited.Attachments[0].Filename != "yeah.png" {
		t.Errorf("expected filename 'yeah.png', got %q", edited.Attachments[0].Filename)
	}
}
