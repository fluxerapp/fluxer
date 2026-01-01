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

// TestAttachmentUpload_IDMatching_SparseIDs tests using non-sequential IDs like 2, 5
func TestAttachmentUpload_IDMatching_SparseIDs(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	guild := createGuild(t, client, user.Token, "Sparse IDs Test Guild")
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
		"content": "Sparse IDs test",
		"attachments": []map[string]any{
			{"id": 2, "filename": "yeah.png", "description": "ID is 2", "title": "Two"},
			{"id": 5, "filename": "thisisfine.gif", "description": "ID is 5", "title": "Five"},
		},
	}
	payloadJSON, err := json.Marshal(payload)
	if err != nil {
		t.Fatalf("failed to encode payload JSON: %v", err)
	}

	if err := writer.WriteField("payload_json", string(payloadJSON)); err != nil {
		t.Fatalf("failed to write payload_json field: %v", err)
	}

	file1Writer, err := writer.CreateFormFile("files[2]", "yeah.png")
	if err != nil {
		t.Fatalf("failed to create files[2] field: %v", err)
	}
	if _, err := file1Writer.Write(file1Data); err != nil {
		t.Fatalf("failed to write file1 data: %v", err)
	}

	file2Writer, err := writer.CreateFormFile("files[5]", "thisisfine.gif")
	if err != nil {
		t.Fatalf("failed to create files[5] field: %v", err)
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

	if msgResp.Attachments[0].Description != "ID is 2" {
		t.Errorf("expected first description 'ID is 2', got %q", msgResp.Attachments[0].Description)
	}
	if msgResp.Attachments[0].Title != "Two" {
		t.Errorf("expected first title 'Two', got %q", msgResp.Attachments[0].Title)
	}

	if msgResp.Attachments[1].Description != "ID is 5" {
		t.Errorf("expected second description 'ID is 5', got %q", msgResp.Attachments[1].Description)
	}
	if msgResp.Attachments[1].Title != "Five" {
		t.Errorf("expected second title 'Five', got %q", msgResp.Attachments[1].Title)
	}
}
