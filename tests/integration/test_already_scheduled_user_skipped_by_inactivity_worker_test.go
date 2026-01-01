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
	"time"
)

// TestAlreadyScheduledUserSkippedByInactivityWorker verifies that users
// who are already scheduled for deletion by other means (e.g., manual deletion)
// are skipped by the inactivity deletion worker and not processed again.
func TestAlreadyScheduledUserSkippedByInactivityWorker(t *testing.T) {
	client := newTestClient(t)

	user := createTestAccount(t, client)

	threeYearsAgo := time.Now().AddDate(-3, 0, 0)
	setLastActiveAt(t, client, user.UserID, threeYearsAgo)

	t.Log("Manually scheduling user for deletion")
	resp, err := client.postJSONWithAuth("/users/@me/delete", map[string]string{
		"password": user.Password,
	}, user.Token)
	if err != nil {
		t.Fatalf("failed to schedule deletion: %v", err)
	}
	if resp.StatusCode != 204 {
		t.Fatalf("expected status 204, got %d", resp.StatusCode)
	}

	dataResp, err := client.get(fmt.Sprintf("/test/users/%s/data-exists", user.UserID))
	if err != nil {
		t.Fatalf("failed to check user data after manual deletion: %v", err)
	}
	defer dataResp.Body.Close()

	var dataBeforeWorker userDataExistsResponse
	decodeJSONResponse(t, dataResp, &dataBeforeWorker)

	if dataBeforeWorker.PendingDeletionAt == nil {
		t.Fatal("expected user to have pending_deletion_at after manual deletion")
	}

	beforeDeletionAt := *dataBeforeWorker.PendingDeletionAt

	clearTestEmails(t, client)

	t.Log("Running inactivity worker on already-scheduled user")
	workerResp := triggerInactivityWorker(t, client)

	if workerResp.WarningsSent > 0 {
		t.Errorf("expected no warnings sent for already-scheduled user, got %d", workerResp.WarningsSent)
	}

	if workerResp.DeletionsScheduled > 0 {
		t.Errorf("expected no deletions scheduled for already-scheduled user, got %d", workerResp.DeletionsScheduled)
	}

	emails := getTestEmails(t, client)
	if len(emails) > 0 {
		t.Errorf("expected no emails sent for already-scheduled user, got %d", len(emails))
	}

	dataRespAfter, err := client.get(fmt.Sprintf("/test/users/%s/data-exists", user.UserID))
	if err != nil {
		t.Fatalf("failed to check user data after worker: %v", err)
	}
	defer dataRespAfter.Body.Close()

	var dataAfterWorker userDataExistsResponse
	decodeJSONResponse(t, dataRespAfter, &dataAfterWorker)

	if dataAfterWorker.PendingDeletionAt == nil {
		t.Fatal("expected user to still have pending_deletion_at after worker")
	}

	afterDeletionAt := *dataAfterWorker.PendingDeletionAt

	beforeTime, err := time.Parse(time.RFC3339, beforeDeletionAt)
	if err != nil {
		t.Fatalf("failed to parse before time: %v", err)
	}

	afterTime, err := time.Parse(time.RFC3339, afterDeletionAt)
	if err != nil {
		t.Fatalf("failed to parse after time: %v", err)
	}

	timeDiff := afterTime.Sub(beforeTime).Abs()
	if timeDiff > 1*time.Second {
		t.Errorf("expected pending_deletion_at to remain unchanged, but it changed by %v", timeDiff)
	}

	t.Log("Already-scheduled user skipping test passed")
}
