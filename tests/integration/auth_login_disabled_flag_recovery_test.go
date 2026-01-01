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

// Covers auto-clearing of DISABLED flag on login (when not temp-banned).
func TestAuthLoginDisabledFlagRecovery(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	updateUserSecurityFlags(t, client, account.UserID, userSecurityFlagsPayload{
		SetFlags: []string{"DISABLED"},
	})

	resp, err := client.postJSON("/auth/login", loginRequest{
		Email:    account.Email,
		Password: account.Password,
	})
	if err != nil {
		t.Fatalf("failed to login with disabled flag: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	statusResp, err := client.getWithAuth("/test/users/"+account.UserID+"/data-exists", account.Token)
	if err != nil {
		t.Fatalf("failed to fetch data-exists: %v", err)
	}
	assertStatus(t, statusResp, http.StatusOK)
	var payload struct {
		HasSelfDeletedFlag bool   `json:"has_self_deleted_flag"`
		HasDeletedFlag     bool   `json:"has_deleted_flag"`
		Flags              string `json:"flags"`
	}
	decodeJSONResponse(t, statusResp, &payload)
	if payload.HasDeletedFlag || payload.HasSelfDeletedFlag {
		t.Fatalf("expected DELETED/SELF_DELETED to be false")
	}
	if payload.Flags == "" {
		statusResp.Body.Close()
		return
	}
	if payload.Flags == "2" {
		t.Fatalf("expected DISABLED flag to be cleared, got flags=%s", payload.Flags)
	}
	statusResp.Body.Close()
}
