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

func TestUserDisableAndDeleteEndpoints(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	resp, err := client.postJSONWithAuth("/users/@me/disable", map[string]string{"password": account.Password}, account.Token)
	if err != nil {
		t.Fatalf("failed to disable account: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.getWithAuth("/users/@me", account.Token)
	if err != nil {
		t.Fatalf("failed to call /users/@me after disable: %v", err)
	}
	if resp.StatusCode != http.StatusUnauthorized {
		t.Fatalf("expected old token to be invalid after disable, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	login := loginTestUser(t, client, account.Email, account.Password)
	newToken := login.Token

	resp, err = client.postJSONWithAuth("/users/@me/delete", map[string]string{"password": account.Password}, newToken)
	if err != nil {
		t.Fatalf("failed to request self delete: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.getWithAuth("/users/@me", newToken)
	if err != nil {
		t.Fatalf("failed to call /users/@me after delete: %v", err)
	}
	if resp.StatusCode != http.StatusUnauthorized {
		t.Fatalf("expected token to be revoked after delete request, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	loginAfterDelete := loginTestUser(t, client, account.Email, account.Password)
	if loginAfterDelete.Token == "" {
		t.Fatalf("expected ability to log in after delete grace period")
	}
}
