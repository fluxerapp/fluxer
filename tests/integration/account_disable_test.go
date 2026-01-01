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
)

func TestAccountDisable(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	friend := createTestAccount(t, client)
	createFriendship(t, client, account, friend)

	resp, err := client.postJSONWithAuth("/users/@me/disable", map[string]string{
		"password": account.Password,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to disable account: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.getWithAuth("/users/@me", account.Token)
	if err != nil {
		t.Fatalf("failed to get user: %v", err)
	}
	if resp.StatusCode != http.StatusUnauthorized {
		t.Errorf("expected old token to be invalid (401), got %d", resp.StatusCode)
	}
	resp.Body.Close()

	loginResp := loginTestUser(t, client, account.Email, account.Password)
	if loginResp.Token == "" {
		t.Fatal("expected to be able to login after disable")
	}

	dataResp, err := client.get(fmt.Sprintf("/test/users/%s/data-exists", loginResp.UserID))
	if err != nil {
		t.Fatalf("failed to check user data: %v", err)
	}
	defer dataResp.Body.Close()

	var dataExists userDataExistsResponse
	decodeJSONResponse(t, dataResp, &dataExists)

	if dataExists.RelationshipsCount == 0 {
		t.Error("expected relationships to be preserved after disable")
	}

	if dataExists.EmailCleared {
		t.Error("expected email to be preserved after disable")
	}

	if dataExists.PasswordCleared {
		t.Error("expected password to be preserved after disable")
	}

	t.Log("Account disable test passed")
}
