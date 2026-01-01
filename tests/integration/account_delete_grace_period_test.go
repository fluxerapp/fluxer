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
	"testing"
)

func TestAccountDeleteGracePeriod(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	resp, err := client.postJSONWithAuth("/users/@me/delete", map[string]string{
		"password": account.Password,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to delete account: %v", err)
	}
	assertStatus(t, resp, 204)

	resp, err = client.getWithAuth("/users/@me", account.Token)
	if err != nil {
		t.Fatalf("failed to get user: %v", err)
	}
	if resp.StatusCode != 401 {
		t.Errorf("expected old token to be invalid (401), got %d", resp.StatusCode)
	}

	dataResp, err := client.get("/test/users/" + account.UserID + "/data-exists")
	if err != nil {
		t.Fatalf("failed to check user data before login: %v", err)
	}
	defer dataResp.Body.Close()

	var dataExistsBeforeLogin userDataExistsResponse
	decodeJSONResponse(t, dataResp, &dataExistsBeforeLogin)

	if !dataExistsBeforeLogin.HasSelfDeletedFlag {
		t.Error("expected SELF_DELETED flag to be set before login")
	}

	if dataExistsBeforeLogin.PendingDeletionAt == nil {
		t.Error("expected pending_deletion_at to be set before login")
	}

	loginResp := loginTestUser(t, client, account.Email, account.Password)
	if loginResp.Token == "" {
		t.Fatal("expected to be able to login during grace period")
	}

	dataRespAfterLogin, err := client.get("/test/users/" + loginResp.UserID + "/data-exists")
	if err != nil {
		t.Fatalf("failed to check user data after login: %v", err)
	}
	defer dataRespAfterLogin.Body.Close()

	var dataExistsAfterLogin userDataExistsResponse
	decodeJSONResponse(t, dataRespAfterLogin, &dataExistsAfterLogin)

	if dataExistsAfterLogin.HasSelfDeletedFlag {
		t.Error("expected SELF_DELETED flag to be cleared after login")
	}

	if dataExistsAfterLogin.PendingDeletionAt != nil {
		t.Error("expected pending_deletion_at to be cleared after login")
	}

	t.Log("Account deletion grace period test passed")
}
