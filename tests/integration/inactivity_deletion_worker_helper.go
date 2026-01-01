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
	"net/http"
	"testing"
)

// triggerInactivityWorker triggers the inactivity deletion worker
func triggerInactivityWorker(t testing.TB, client *testClient) inactivityWorkerResponse {
	t.Helper()

	resp, err := client.postJSON("/test/worker/process-inactivity-deletions", nil)
	if err != nil {
		t.Fatalf("failed to trigger inactivity worker: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Fatalf("expected status 200 when triggering inactivity worker, got %d", resp.StatusCode)
	}

	var result inactivityWorkerResponse
	decodeJSONResponse(t, resp, &result)

	t.Logf("Inactivity worker result: processed=%d, warnings_sent=%d, deletions_scheduled=%d",
		result.Processed, result.WarningsSent, result.DeletionsScheduled)

	return result
}
