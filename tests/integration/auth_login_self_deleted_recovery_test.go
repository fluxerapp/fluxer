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

// Covers auto-recovery for self-deleted accounts with pending deletion.
func TestAuthLoginSelfDeletedRecovery(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	setPendingDeletion(t, client, account.UserID, time.Now().Add(-1*time.Hour), true)

	resp, err := client.postJSON("/auth/login", loginRequest{
		Email:    account.Email,
		Password: account.Password,
	})
	if err != nil {
		t.Fatalf("failed to login after marking self-deleted: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	statusResp, err := client.getWithAuth("/test/users/"+account.UserID+"/data-exists", account.Token)
	if err != nil {
		t.Fatalf("failed to fetch data-exists: %v", err)
	}
	if statusResp.StatusCode != http.StatusOK {
		t.Fatalf("data-exists returned %d: %s", statusResp.StatusCode, readResponseBody(statusResp))
	}
	var payload struct {
		PendingDeletionAt  *string `json:"pending_deletion_at"`
		HasSelfDeletedFlag bool    `json:"has_self_deleted_flag"`
		HasDeletedFlag     bool    `json:"has_deleted_flag"`
	}
	decodeJSONResponse(t, statusResp, &payload)
	if payload.PendingDeletionAt != nil {
		t.Fatalf("expected pending_deletion_at to be cleared, got %v", *payload.PendingDeletionAt)
	}
	if payload.HasSelfDeletedFlag {
		t.Fatalf("expected SELF_DELETED flag to be cleared")
	}
	if payload.HasDeletedFlag {
		t.Fatalf("expected DELETED flag to be false")
	}
	statusResp.Body.Close()
}
