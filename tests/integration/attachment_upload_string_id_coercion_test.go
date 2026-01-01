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
	"mime/multipart"
	"net/http"
	"testing"
)

// TestAttachmentUpload_StringIDCoercion tests that string IDs are coerced to numbers
func TestAttachmentUpload_StringIDCoercion(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	guild := createGuild(t, client, user.Token, "String ID Coercion Test Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	fileData := []byte("test content")

	var body bytes.Buffer
	writer := multipart.NewWriter(&body)

	payloadStr := `{"content":"test","attachments":[{"id":"0","filename":"test.txt","flags":"0"}]}`

	if err := writer.WriteField("payload_json", payloadStr); err != nil {
		t.Fatalf("failed to write payload_json field: %v", err)
	}

	fileWriter, err := writer.CreateFormFile("files[0]", "test.txt")
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
			Filename string `json:"filename"`
			Flags    int    `json:"flags"`
		} `json:"attachments"`
	}
	decodeJSONResponse(t, resp, &msgResp)

	if len(msgResp.Attachments) != 1 {
		t.Fatalf("expected 1 attachment, got %d", len(msgResp.Attachments))
	}

	if msgResp.Attachments[0].Filename != "test.txt" {
		t.Errorf("expected filename 'test.txt', got %q", msgResp.Attachments[0].Filename)
	}
}
