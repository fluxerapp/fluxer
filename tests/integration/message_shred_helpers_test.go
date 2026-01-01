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
	"net/http"
	"testing"
	"time"
)

type messageShredStatusResponse struct {
	Status      string  `json:"status"`
	Requested   int     `json:"requested"`
	Total       int     `json:"total"`
	Processed   int     `json:"processed"`
	Skipped     int     `json:"skipped"`
	StartedAt   string  `json:"started_at"`
	CompletedAt *string `json:"completed_at"`
	FailedAt    *string `json:"failed_at"`
	Error       *string `json:"error"`
}

func adminPostJSON(t testing.TB, client *testClient, token, path string, payload any) *http.Response {
	t.Helper()

	var body *bytes.Buffer
	if payload != nil {
		body = &bytes.Buffer{}
		if err := json.NewEncoder(body).Encode(payload); err != nil {
			t.Fatalf("failed to encode admin payload: %v", err)
		}
	}

	req, err := http.NewRequest(http.MethodPost, client.baseURL+path, body)
	if err != nil {
		t.Fatalf("failed to build admin request: %v", err)
	}
	if payload != nil {
		req.Header.Set("Content-Type", "application/json")
	}
	req.Header.Set("Authorization", token)
	client.applyCommonHeaders(req)

	resp, err := client.httpClient.Do(req)
	if err != nil {
		t.Fatalf("admin request failed: %v", err)
	}
	return resp
}

func waitForMessageShredJobCompletion(t testing.TB, client *testClient, token, jobID string) messageShredStatusResponse {
	t.Helper()

	deadline := time.Now().Add(20 * time.Second)
	var status messageShredStatusResponse

	for {
		resp := adminPostJSON(t, client, token, "/admin/messages/shred-status", map[string]any{"job_id": jobID})
		assertStatus(t, resp, http.StatusOK)
		decodeJSONResponse(t, resp, &status)

		switch status.Status {
		case "completed":
			return status
		case "failed":
			errMsg := "unknown error"
			if status.Error != nil {
				errMsg = *status.Error
			}
			t.Fatalf("message shred job %s failed: %s", jobID, errMsg)
		}

		if time.Now().After(deadline) {
			t.Fatalf("message shred job %s did not complete before timeout (last status %s)", jobID, status.Status)
		}

		time.Sleep(500 * time.Millisecond)
	}
}
