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

// Covers reset token reuse rejection and session invalidation after password reset.
func TestAuthResetTokenReuseIsRejectedAndOldSessionsInvalidated(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)
	clearTestEmails(t, client)

	resp, err := client.postJSON("/auth/forgot", map[string]string{"email": account.Email})
	if err != nil {
		t.Fatalf("failed to call forgot password: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	email := waitForEmail(t, client, "password_reset", account.Email)
	token, ok := email.Metadata["token"]
	if !ok || token == "" {
		t.Fatalf("expected password reset token in email metadata")
	}

	newPassword := uniquePassword()
	resp, err = client.postJSON("/auth/reset", map[string]string{"token": token, "password": newPassword})
	if err != nil {
		t.Fatalf("failed to call reset password: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var resetResp loginResponse
	decodeJSONResponse(t, resp, &resetResp)
	if resetResp.Token == "" {
		t.Fatalf("expected reset to return a new token")
	}
	resp.Body.Close()

	resp, err = client.getWithAuth("/users/@me", account.Token)
	if err != nil {
		t.Fatalf("failed to call users/@me with old token: %v", err)
	}
	if resp.StatusCode == http.StatusOK {
		t.Fatalf("expected old token to be invalid after password reset")
	}
	resp.Body.Close()

	anotherPassword := uniquePassword()
	resp, err = client.postJSON("/auth/reset", map[string]string{"token": token, "password": anotherPassword})
	if err != nil {
		t.Fatalf("failed to call reset password with reused token: %v", err)
	}
	if resp.StatusCode == http.StatusOK {
		t.Fatalf("expected reset with reused token to fail")
	}
	assertStatus(t, resp, http.StatusBadRequest)
	resp.Body.Close()

	login := loginTestUser(t, client, account.Email, newPassword)
	if login.Token == "" {
		t.Fatalf("expected login with latest password to succeed")
	}
}
