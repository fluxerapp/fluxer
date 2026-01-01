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

func TestAccountDeleteAutoCancelOnLogin(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	resp, err := client.postJSONWithAuth("/users/@me/delete", map[string]string{
		"password": account.Password,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to delete account: %v", err)
	}
	assertStatus(t, resp, 204)

	loginResp := loginTestUser(t, client, account.Email, account.Password)
	if loginResp.Token == "" {
		t.Fatal("expected to be able to login")
	}

	dataResp, err := client.get("/test/users/" + loginResp.UserID + "/data-exists")
	if err != nil {
		t.Fatalf("failed to check user data: %v", err)
	}
	defer dataResp.Body.Close()

	var dataExists userDataExistsResponse
	decodeJSONResponse(t, dataResp, &dataExists)

	if dataExists.HasSelfDeletedFlag {
		t.Error("expected SELF_DELETED flag to be removed after login")
	}

	if dataExists.PendingDeletionAt != nil {
		t.Error("expected pending_deletion_at to be cleared after login")
	}

	t.Log("Auto-cancel deletion on login test passed")
}
