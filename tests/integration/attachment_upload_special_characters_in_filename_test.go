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

// TestAttachmentUpload_SpecialCharactersInFilename tests handling of special characters
func TestAttachmentUpload_SpecialCharactersInFilename(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	guild := createGuild(t, client, user.Token, "Special Chars Test Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	fileData := []byte("test content")

	testCases := []struct {
		filename          string
		shouldBeSanitized bool
		description       string
	}{
		{"file<script>.txt", true, "HTML tag in filename"},
		{"file>output.txt", true, "redirect operator"},
		{"file|pipe.txt", true, "pipe character"},
		{"file:colon.txt", true, "colon (Windows reserved)"},
		{"file*star.txt", true, "asterisk (wildcard)"},
		{"file?question.txt", true, "question mark"},
		{"file\"quote.txt", true, "double quote"},
		{"COM1.txt", true, "Windows reserved name"},
		{"LPT1.txt", true, "Windows reserved name"},
		{"file with spaces.txt", false, "spaces should be OK"},
		{"file-dash_underscore.txt", false, "dash and underscore OK"},
		{"file.multiple.dots.txt", false, "multiple dots OK"},
		{"файл.txt", false, "unicode characters OK"},
		{"文件.txt", false, "CJK characters OK"},
		{"😀.txt", false, "emoji OK"},
	}

	for _, tc := range testCases {
		t.Run(tc.description, func(t *testing.T) {
			var body bytes.Buffer
			writer := multipart.NewWriter(&body)

			payload := map[string]any{
				"content": "Special char test: " + tc.description,
				"attachments": []map[string]any{
					{"id": 0, "filename": tc.filename},
				},
			}
			payloadJSON, err := json.Marshal(payload)
			if err != nil {
				t.Fatalf("failed to encode payload JSON: %v", err)
			}

			if err := writer.WriteField("payload_json", string(payloadJSON)); err != nil {
				t.Fatalf("failed to write payload_json field: %v", err)
			}

			safeFilename := "test.txt"
			fileWriter, err := writer.CreateFormFile("files[0]", safeFilename)
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

			if tc.shouldBeSanitized {
				if strings.ContainsAny(sanitized, "<>:\"|?*") {
					t.Errorf("sanitized filename %q still contains dangerous characters", sanitized)
				}
			}
		})
	}
}
