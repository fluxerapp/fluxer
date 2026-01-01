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

type messageWithAttachmentResponse struct {
	messageResponse
	Attachments []struct {
		ID string `json:"id"`
	} `json:"attachments"`
}

func sendChannelMessageWithAttachment(
	t testing.TB,
	client *testClient,
	token string,
	channelID int64,
	content string,
	fixtureName string,
) (messageResponse, int64) {
	t.Helper()
	ensureSessionStarted(t, client, token)

	fileData, err := fixturesFS.ReadFile(fmt.Sprintf("fixtures/%s", fixtureName))
	if err != nil {
		t.Fatalf("failed to read attachment fixture %s: %v", fixtureName, err)
	}

	var body bytes.Buffer
	writer := multipart.NewWriter(&body)

	payload := map[string]any{
		"content": content,
		"attachments": []map[string]any{
			{"id": 0, "filename": fixtureName},
		},
	}
	payloadJSON, err := json.Marshal(payload)
	if err != nil {
		t.Fatalf("failed to encode payload JSON: %v", err)
	}

	if err := writer.WriteField("payload_json", string(payloadJSON)); err != nil {
		t.Fatalf("failed to write payload_json field: %v", err)
	}

	fileWriter, err := writer.CreateFormFile("files[0]", fixtureName)
	if err != nil {
		t.Fatalf("failed to create multipart file field: %v", err)
	}
	if _, err := fileWriter.Write(fileData); err != nil {
		t.Fatalf("failed to write attachment data: %v", err)
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
		t.Fatalf("failed to create multipart request: %v", err)
	}
	req.Header.Set("Content-Type", writer.FormDataContentType())
	client.applyCommonHeaders(req)
	if token != "" {
		req.Header.Set("Authorization", token)
	}

	resp, err := client.httpClient.Do(req)
	if err != nil {
		t.Fatalf("failed to send multipart message: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var msg messageWithAttachmentResponse
	decodeJSONResponse(t, resp, &msg)
	if len(msg.Attachments) == 0 {
		t.Fatalf("expected message to include at least one attachment")
	}

	attachmentID := parseSnowflake(t, msg.Attachments[0].ID)
	return msg.messageResponse, attachmentID
}
