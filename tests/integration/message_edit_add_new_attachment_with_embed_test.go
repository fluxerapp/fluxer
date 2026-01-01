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
	"encoding/json"
	"fmt"
	"mime/multipart"
	"net/http"
	"testing"
)

// TestMessageEdit_AddNewAttachmentWithEmbed tests editing a message to add both
// a new attachment and an embed that references it
func TestMessageEdit_AddNewAttachmentWithEmbed(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	guild := createGuild(t, client, user.Token, "Add Attachment Embed Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	payload := map[string]string{"content": "Original message"}
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages", channelID), payload, user.Token)
	if err != nil {
		t.Fatalf("failed to send message: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var msg struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &msg)
	resp.Body.Close()

	fileData, err := fixturesFS.ReadFile("fixtures/yeah.png")
	if err != nil {
		t.Fatalf("failed to read fixture: %v", err)
	}

	var body bytes.Buffer
	writer := multipart.NewWriter(&body)

	editPayload := map[string]any{
		"content": "Edited with new attachment",
		"attachments": []map[string]any{
			{"id": 0, "filename": "yeah.png"},
		},
		"embeds": []map[string]any{
			{
				"title": "New Image",
				"image": map[string]string{
					"url": "attachment://yeah.png",
				},
			},
		},
	}
	payloadJSON, err := json.Marshal(editPayload)
	if err != nil {
		t.Fatalf("failed to encode payload JSON: %v", err)
	}

	if err := writer.WriteField("payload_json", string(payloadJSON)); err != nil {
		t.Fatalf("failed to write payload_json field: %v", err)
	}

	fileWriter, err := writer.CreateFormFile("files[0]", "yeah.png")
	if err != nil {
		t.Fatalf("failed to create file field: %v", err)
	}
	if _, err := fileWriter.Write(fileData); err != nil {
		t.Fatalf("failed to write file data: %v", err)
	}

	if err := writer.Close(); err != nil {
		t.Fatalf("failed to close multipart writer: %v", err)
	}

	req, err := http.NewRequest(
		http.MethodPatch,
		fmt.Sprintf("%s/channels/%d/messages/%s", client.baseURL, channelID, msg.ID),
		&body,
	)
	if err != nil {
		t.Fatalf("failed to create request: %v", err)
	}
	req.Header.Set("Content-Type", writer.FormDataContentType())
	client.applyCommonHeaders(req)
	req.Header.Set("Authorization", user.Token)

	resp, err = client.httpClient.Do(req)
	if err != nil {
		t.Fatalf("failed to send request: %v", err)
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

	if edited.Content != "Edited with new attachment" {
		t.Errorf("expected content 'Edited with new attachment', got %q", edited.Content)
	}

	if len(edited.Embeds) != 1 {
		t.Fatalf("expected 1 embed, got %d", len(edited.Embeds))
	}

	embedImageURL := edited.Embeds[0].Image.URL
	if embedImageURL == "attachment://yeah.png" {
		t.Error("expected attachment:// URL to be resolved to CDN URL")
	}
	if !bytes.Contains([]byte(embedImageURL), []byte("/attachments/")) {
		t.Errorf("expected CDN URL to contain '/attachments/', got %q", embedImageURL)
	}

	if len(edited.Attachments) != 1 {
		t.Fatalf("expected 1 attachment, got %d", len(edited.Attachments))
	}
	if edited.Attachments[0].Filename != "yeah.png" {
		t.Errorf("expected filename 'yeah.png', got %q", edited.Attachments[0].Filename)
	}
}
