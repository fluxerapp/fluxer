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
	"bytes"
	"fmt"
	"net/http"
	"testing"
)

// TestMessageEdit_WithEmbedAttachmentURL tests editing a message to add an embed
// that uses attachment:// URL referencing existing attachments
func TestMessageEdit_WithEmbedAttachmentURL(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	guild := createGuild(t, client, user.Token, "Message Edit Embed Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	msg, _ := sendChannelMessageWithAttachment(t, client, user.Token, channelID, "Original message", "yeah.png")

	updatePayload := map[string]any{
		"content": "Edited with embed",
		"embeds": []map[string]any{
			{
				"title": "Image Embed",
				"image": map[string]string{
					"url": "attachment://yeah.png",
				},
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
		Content string `json:"content"`
		Embeds  []struct {
			Title string `json:"title"`
			Image struct {
				URL string `json:"url"`
			} `json:"image"`
		} `json:"embeds"`
		Attachments []struct {
			Filename string `json:"filename"`
		} `json:"attachments"`
	}
	decodeJSONResponse(t, resp, &edited)

	if edited.Content != "Edited with embed" {
		t.Errorf("expected content 'Edited with embed', got %q", edited.Content)
	}

	if len(edited.Embeds) != 1 {
		t.Fatalf("expected 1 embed, got %d", len(edited.Embeds))
	}

	if edited.Embeds[0].Title != "Image Embed" {
		t.Errorf("expected embed title 'Image Embed', got %q", edited.Embeds[0].Title)
	}

	embedImageURL := edited.Embeds[0].Image.URL
	if embedImageURL == "" {
		t.Error("expected embed image URL to be set")
	}
	if embedImageURL == "attachment://yeah.png" {
		t.Error("expected attachment:// URL to be resolved to CDN URL, but it wasn't")
	}
	if !bytes.Contains([]byte(embedImageURL), []byte("/attachments/")) {
		t.Errorf("expected CDN URL to contain '/attachments/', got %q", embedImageURL)
	}

	if len(edited.Attachments) != 1 {
		t.Fatalf("expected 1 attachment, got %d", len(edited.Attachments))
	}
}
