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
	"strings"
	"testing"
)

// TestAttachmentUpload_NullByteInFilename tests that null bytes in filenames are sanitized
func TestAttachmentUpload_NullByteInFilename(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	guild := createGuild(t, client, user.Token, "Null Byte Test Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	fileData := []byte("test content")

	maliciousFilenames := []string{
		"innocent.txt\x00.exe",
		"test\x00.jpg",
		"file.pdf\x00malicious",
		"\x00hidden.txt",
	}

	for _, filename := range maliciousFilenames {
		t.Run("filename_with_null_byte", func(t *testing.T) {
			var body bytes.Buffer
			writer := multipart.NewWriter(&body)

			payload := map[string]any{
				"content": "Null byte test",
				"attachments": []map[string]any{
					{"id": 0, "filename": filename},
				},
			}
			payloadJSON, err := json.Marshal(payload)
			if err != nil {
				t.Fatalf("failed to encode payload JSON: %v", err)
			}

			if err := writer.WriteField("payload_json", string(payloadJSON)); err != nil {
				t.Fatalf("failed to write payload_json field: %v", err)
			}

			fileWriter, err := writer.CreateFormFile("files[0]", filename)
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
				} `json:"attachments"`
			}
			decodeJSONResponse(t, resp, &msgResp)

			if len(msgResp.Attachments) != 1 {
				t.Fatalf("expected 1 attachment, got %d", len(msgResp.Attachments))
			}

			sanitized := msgResp.Attachments[0].Filename
			if strings.Contains(sanitized, "\x00") {
				t.Errorf("sanitized filename still contains null bytes")
			}
		})
	}
}
