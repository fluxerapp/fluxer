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
	"time"
)

const (
	standardMinimumDays = 60
	userRequestedDays   = 14
)

func TestAdminUserDeletionScheduleMinimums(t *testing.T) {
	client := newTestClient(t)
	admin := createTestAccount(t, client)
	setUserACLs(t, client, admin.UserID, []string{"admin:authenticate", "user:delete"})

	t.Run("NonUserRequestedRequiresStandardMinimum", func(t *testing.T) {
		target := createTestAccount(t, client)
		scheduleAdminDeletion(t, client, admin.Token, target.UserID, 2, 10)

		requirePendingDurationBetween(
			t,
			client,
			target.UserID,
			(standardMinimumDays-1)*24*time.Hour,
			(standardMinimumDays+2)*24*time.Hour,
		)
	})

	t.Run("UserRequestedUsesUserMinimum", func(t *testing.T) {
		target := createTestAccount(t, client)
		scheduleAdminDeletion(t, client, admin.Token, target.UserID, 1, 5)

		requirePendingDurationBetween(
			t,
			client,
			target.UserID,
			(userRequestedDays-1)*24*time.Hour,
			(userRequestedDays+2)*24*time.Hour,
		)
	})
}

func scheduleAdminDeletion(t testing.TB, client *testClient, token, userID string, reasonCode, days int) {
	t.Helper()

	payload := map[string]any{
		"user_id":             userID,
		"reason_code":         reasonCode,
		"days_until_deletion": days,
	}

	resp, err := client.postJSONWithAuth("/admin/users/schedule-deletion", payload, token)
	if err != nil {
		t.Fatalf("failed to request admin deletion: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Fatalf("expected 200 when scheduling deletion, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}
}

func requirePendingDurationBetween(t testing.TB, client *testClient, userID string, min, max time.Duration) {
	t.Helper()

	diff := pendingDeletionDuration(t, client, userID)

	if diff < min {
		t.Fatalf("expected pending deletion at least %s from now, got %s", min, diff)
	}
	if diff > max {
		t.Fatalf("expected pending deletion at most %s from now, got %s", max, diff)
	}
}

func pendingDeletionDuration(t testing.TB, client *testClient, userID string) time.Duration {
	t.Helper()

	resp, err := client.get("/test/users/" + userID + "/data-exists")
	if err != nil {
		t.Fatalf("failed to fetch user data: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Fatalf("expected 200 when fetching user data, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var data userDataExistsResponse
	decodeJSONResponse(t, resp, &data)

	if data.PendingDeletionAt == nil {
		t.Fatalf("expected pending_deletion_at to be set, but it was nil")
	}

	pendingAt, err := time.Parse(time.RFC3339, *data.PendingDeletionAt)
	if err != nil {
		t.Fatalf("invalid pending_deletion_at: %v", err)
	}

	diff := time.Until(pendingAt)
	if diff < 0 {
		t.Fatalf("expected pending deletion to be in the future, got %s", diff)
	}

	return diff
}
