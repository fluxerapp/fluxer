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

// TestFileUpload_Multipart_MultipleFiles tests multipart upload with multiple files
func TestFileUpload_Multipart_MultipleFiles(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	guild := createGuild(t, client, user.Token, "Multi Multipart Upload Test Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	file1Data, err := fixturesFS.ReadFile("fixtures/yeah.png")
	if err != nil {
		t.Fatalf("failed to read fixture yeah.png: %v", err)
	}

	file2Data, err := fixturesFS.ReadFile("fixtures/thisisfine.gif")
	if err != nil {
		t.Fatalf("failed to read fixture thisisfine.gif: %v", err)
	}

	// Create multipart form
	var body bytes.Buffer
	writer := multipart.NewWriter(&body)

	payload := map[string]any{
		"content": "Multiple files test",
		"attachments": []map[string]any{
			{"id": 0, "filename": "yeah.png", "description": "First image"},
			{"id": 1, "filename": "thisisfine.gif", "description": "Second image"},
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
		Content     string `json:"content"`
		Attachments []struct {
			ID          string `json:"id"`
			Filename    string `json:"filename"`
			Description string `json:"description"`
		} `json:"attachments"`
	}
	decodeJSONResponse(t, resp, &msgResp)

	if msgResp.Content != "Multiple files test" {
		t.Errorf("expected content 'Multiple files test', got %q", msgResp.Content)
	}

	if len(msgResp.Attachments) != 2 {
		t.Fatalf("expected 2 attachments, got %d", len(msgResp.Attachments))
	}

	if msgResp.Attachments[0].Filename != "yeah.png" {
		t.Errorf("expected first filename 'yeah.png', got %q", msgResp.Attachments[0].Filename)
	}
	if msgResp.Attachments[0].Description != "First image" {
		t.Errorf("expected first description 'First image', got %q", msgResp.Attachments[0].Description)
	}

	if msgResp.Attachments[1].Filename != "thisisfine.gif" {
		t.Errorf("expected second filename 'thisisfine.gif', got %q", msgResp.Attachments[1].Filename)
	}
	if msgResp.Attachments[1].Description != "Second image" {
		t.Errorf("expected second description 'Second image', got %q", msgResp.Attachments[1].Description)
	}
}
