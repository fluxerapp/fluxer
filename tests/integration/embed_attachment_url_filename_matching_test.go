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

// TestEmbedAttachmentURL_FilenameMatching tests verify exact filename matching works correctly
func TestEmbedAttachmentURL_FilenameMatching(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	guild := createGuild(t, client, user.Token, "Filename Matching Test Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	fileData, err := fixturesFS.ReadFile("fixtures/yeah.png")
	if err != nil {
		t.Fatalf("failed to read fixture yeah.png: %v", err)
	}

	// Test case-sensitive filename matching
	var body bytes.Buffer
	writer := multipart.NewWriter(&body)

	payload := map[string]any{
		"content": "Case-sensitive filename test",
		"attachments": []map[string]any{
			{"id": 0, "filename": "yeah.png"},
		},
		"embeds": []map[string]any{
			{
				"title": "Exact Match Required",
				"image": map[string]string{
					"url": "attachment://yeah.png",
				},
			},
		},
	}

	payloadJSON, err := json.Marshal(payload)
	if err != nil {
		t.Fatalf("failed to encode payload JSON: %v", err)
	}

	if err := writer.WriteField("payload_json", string(payloadJSON)); err != nil {
		t.Fatalf("failed to write payload_json field: %v", err)
	}

	fileWriter, err := writer.CreateFormFile("files[0]", "yeah.png")
	if err != nil {
		t.Fatalf("failed to create files[0] field: %v", err)
	}
	if _, err := fileWriter.Write(fileData); err != nil {
		t.Fatalf("failed to write file data: %v", err)
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

	// Now test with mismatched case - should fail
	var body2 bytes.Buffer
	writer2 := multipart.NewWriter(&body2)

	payload2 := map[string]any{
		"content": "Case mismatch test",
		"attachments": []map[string]any{
			{"id": 0, "filename": "yeah.png"},
		},
		"embeds": []map[string]any{
			{
				"title": "Wrong Case",
				"image": map[string]string{
					"url": "attachment://Yeah.png",
				},
			},
		},
	}

	payloadJSON2, err := json.Marshal(payload2)
	if err != nil {
		t.Fatalf("failed to encode payload JSON: %v", err)
	}

	if err := writer2.WriteField("payload_json", string(payloadJSON2)); err != nil {
		t.Fatalf("failed to write payload_json field: %v", err)
	}

	fileWriter2, err := writer2.CreateFormFile("files[0]", "yeah.png")
	if err != nil {
		t.Fatalf("failed to create files[0] field: %v", err)
	}
	if _, err := fileWriter2.Write(fileData); err != nil {
		t.Fatalf("failed to write file data: %v", err)
	}

	if err := writer2.Close(); err != nil {
		t.Fatalf("failed to close multipart writer: %v", err)
	}

	req2, err := http.NewRequest(
		http.MethodPost,
		fmt.Sprintf("%s/channels/%d/messages", client.baseURL, channelID),
		&body2,
	)
	if err != nil {
		t.Fatalf("failed to create request: %v", err)
	}
	req2.Header.Set("Content-Type", writer2.FormDataContentType())
	client.applyCommonHeaders(req2)
	req2.Header.Set("Authorization", user.Token)

	resp2, err := client.httpClient.Do(req2)
	if err != nil {
		t.Fatalf("failed to send request: %v", err)
	}
	defer resp2.Body.Close()

	assertStatus(t, resp2, http.StatusBadRequest)
}
