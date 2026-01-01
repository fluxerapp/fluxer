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
	"testing"
)

// TestUserWithoutActivityNeverScheduled verifies that users with no
// last_active_at value are never scheduled for inactivity-based deletion,
// as they have no activity tracking data.
func TestUserWithoutActivityNeverScheduled(t *testing.T) {
	client := newTestClient(t)

	user := createTestAccount(t, client)

	clearTestEmails(t, client)

	workerResp := triggerInactivityWorker(t, client)

	if workerResp.WarningsSent > 0 {
		t.Errorf("expected no warnings sent for user without activity, got %d", workerResp.WarningsSent)
	}

	if workerResp.DeletionsScheduled > 0 {
		t.Errorf("expected no deletions scheduled for user without activity, got %d", workerResp.DeletionsScheduled)
	}

	dataResp, err := client.get(fmt.Sprintf("/test/users/%s/data-exists", user.UserID))
	if err != nil {
		t.Fatalf("failed to check user data: %v", err)
	}
	defer dataResp.Body.Close()

	var data userDataExistsResponse
	decodeJSONResponse(t, dataResp, &data)

	if data.PendingDeletionAt != nil {
		t.Error("expected user without activity to not have pending_deletion_at, but it was set")
	}

	t.Log("No-activity immunity test passed")
}
