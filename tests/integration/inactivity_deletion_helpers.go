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
	"fmt"
	"net/http"
	"testing"
	"time"
)

// inactivityWorkerResponse represents the response from the inactivity worker endpoint
type inactivityWorkerResponse struct {
	Processed          int    `json:"processed"`
	WarningsSent       int    `json:"warnings_sent"`
	DeletionsScheduled int    `json:"deletions_scheduled"`
	Message            string `json:"message"`
}

// setLastActiveAtRequest represents the request to set last_active_at
type setLastActiveAtRequest struct {
	Timestamp string `json:"timestamp"`
}

// setLastActiveAt sets the last_active_at timestamp for a user
func setLastActiveAt(t testing.TB, client *testClient, userID string, lastActiveAt time.Time) {
	t.Helper()

	resp, err := client.postJSON(
		fmt.Sprintf("/test/users/%s/set-last-active-at", userID),
		setLastActiveAtRequest{
			Timestamp: lastActiveAt.UTC().Format(time.RFC3339),
		},
	)
	if err != nil {
		t.Fatalf("failed to set last_active_at: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Fatalf("expected status 200 when setting last_active_at, got %d", resp.StatusCode)
	}
}
