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

// TestAttachmentUpload_IDMatching_ReversedMetadata tests that when metadata IDs
// are reversed (id=1, id=0), they still match correctly with files[0] and files[1]
func TestAttachmentUpload_IDMatching_ReversedMetadata(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	guild := createGuild(t, client, user.Token, "Reversed Metadata Test Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	file1Data, err := fixturesFS.ReadFile("fixtures/yeah.png")
	if err != nil {
		t.Fatalf("failed to read fixture: %v", err)
	}

	file2Data, err := fixturesFS.ReadFile("fixtures/thisisfine.gif")
	if err != nil {
		t.Fatalf("failed to read fixture: %v", err)
	}

	var body bytes.Buffer
	writer := multipart.NewWriter(&body)

	payload := map[string]any{
		"content": "Reversed metadata test",
		"attachments": []map[string]any{
			{"id": 1, "filename": "thisisfine.gif", "description": "Second file", "title": "GIF"},
			{"id": 0, "filename": "yeah.png", "description": "First file", "title": "PNG"},
		},
	}
	payloadJSON, err := json.Marshal(payload)
	if err != nil {
		t.Fatalf("failed to encode payload JSON: %v", err)
	}

	if err := writer.WriteField("payload_json", string(payloadJSON)); err != nil {
		t.Fatalf("failed to write payload_json field: %v", err)
	}

	file1Writer, err := writer.CreateFormFile("files[0]", "yeah.png")
	if err != nil {
		t.Fatalf("failed to create files[0] field: %v", err)
	}
	if _, err := file1Writer.Write(file1Data); err != nil {
		t.Fatalf("failed to write file1 data: %v", err)
	}

	file2Writer, err := writer.CreateFormFile("files[1]", "thisisfine.gif")
	if err != nil {
		t.Fatalf("failed to create files[1] field: %v", err)
	}
	if _, err := file2Writer.Write(file2Data); err != nil {
		t.Fatalf("failed to write file2 data: %v", err)
	}

	if err := writer.Close(); err != nil {
		t.Fatalf("failed to close multipart writer: %v", err)
	}

	req, err := http.NewRequest(
		http.MethodPost,
		fmt.Sprintf("%s/channels/%d/messages", client.baseURL, channelID),
		&body,
	)
	if err != nil {
		t.Fatalf("failed to create request: %v", err)
	}
	req.Header.Set("Content-Type", writer.FormDataContentType())
	client.applyCommonHeaders(req)
	req.Header.Set("Authorization", user.Token)

	resp, err := client.httpClient.Do(req)
	if err != nil {
		t.Fatalf("failed to send request: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusOK)

	var msgResp struct {
		Attachments []struct {
			Filename    string `json:"filename"`
			Description string `json:"description"`
			Title       string `json:"title"`
		} `json:"attachments"`
	}
	decodeJSONResponse(t, resp, &msgResp)

	if len(msgResp.Attachments) != 2 {
		t.Fatalf("expected 2 attachments, got %d", len(msgResp.Attachments))
	}

	if msgResp.Attachments[0].Filename != "thisisfine.gif" {
		t.Errorf("expected first filename 'thisisfine.gif' (first in metadata array), got %q", msgResp.Attachments[0].Filename)
	}
	if msgResp.Attachments[0].Description != "Second file" {
		t.Errorf("expected first description 'Second file' (from metadata[0] with id=1), got %q", msgResp.Attachments[0].Description)
	}
	if msgResp.Attachments[0].Title != "GIF" {
		t.Errorf("expected first title 'GIF' (from metadata[0] with id=1), got %q", msgResp.Attachments[0].Title)
	}

	if msgResp.Attachments[1].Filename != "yeah.png" {
		t.Errorf("expected second filename 'yeah.png' (second in metadata array), got %q", msgResp.Attachments[1].Filename)
	}
	if msgResp.Attachments[1].Description != "First file" {
		t.Errorf("expected second description 'First file' (from metadata[1] with id=0), got %q", msgResp.Attachments[1].Description)
	}
	if msgResp.Attachments[1].Title != "PNG" {
		t.Errorf("expected second title 'PNG' (from metadata[1] with id=0), got %q", msgResp.Attachments[1].Title)
	}
}
