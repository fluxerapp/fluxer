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

func TestAuthPasswordChangeInvalidatesSessions(t *testing.T) {
	client := newTestClient(t)

	t.Run("changing password invalidates other sessions", func(t *testing.T) {
		account := createTestAccount(t, client)

		session1Token := account.Token

		loginResp := loginTestUser(t, client, account.Email, account.Password)
		session2Token := loginResp.Token

		loginResp = loginTestUser(t, client, account.Email, account.Password)
		session3Token := loginResp.Token

		resp, err := client.getWithAuth("/users/@me", session1Token)
		if err != nil {
			t.Fatalf("failed to verify session1: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()

		resp, err = client.getWithAuth("/users/@me", session2Token)
		if err != nil {
			t.Fatalf("failed to verify session2: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()

		resp, err = client.getWithAuth("/users/@me", session3Token)
		if err != nil {
			t.Fatalf("failed to verify session3: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()

		newPassword := uniquePassword()
		updatePayload := map[string]any{
			"password":     account.Password,
			"new_password": newPassword,
		}

		resp, err = client.patchJSONWithAuth("/users/@me", updatePayload, session1Token)
		if err != nil {
			t.Fatalf("failed to change password: %v", err)
		}

		if resp.StatusCode != http.StatusOK {
			t.Fatalf("expected password change to succeed, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}

		var updatedUser userPrivateResponse
		decodeJSONResponse(t, resp, &updatedUser)
		resp.Body.Close()

		resp, err = client.getWithAuth("/users/@me", session1Token)
		if err != nil {
			t.Fatalf("failed to check session1 after password change: %v", err)
		}
		if resp.StatusCode != http.StatusUnauthorized {
			t.Fatalf("expected session1 to be invalidated after password change, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
		resp.Body.Close()

		resp, err = client.getWithAuth("/users/@me", session2Token)
		if err != nil {
			t.Fatalf("failed to check session2 after password change: %v", err)
		}
		if resp.StatusCode != http.StatusUnauthorized {
			t.Fatalf("expected session2 to be invalidated after password change, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
		resp.Body.Close()

		resp, err = client.getWithAuth("/users/@me", session3Token)
		if err != nil {
			t.Fatalf("failed to check session3 after password change: %v", err)
		}
		if resp.StatusCode != http.StatusUnauthorized {
			t.Fatalf("expected session3 to be invalidated after password change, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
		resp.Body.Close()

		loginResp = loginTestUser(t, client, account.Email, newPassword)
		if loginResp.Token == "" {
			t.Fatalf("expected login with new password to succeed")
		}

		oldPasswordLogin := loginRequest{
			Email:    account.Email,
			Password: account.Password,
		}
		resp, err = client.postJSON("/auth/login", oldPasswordLogin)
		if err != nil {
			t.Fatalf("failed to attempt login with old password: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusUnauthorized && resp.StatusCode != http.StatusBadRequest {
			t.Fatalf("expected old password to be rejected, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
	})

	t.Run("password reset invalidates all sessions", func(t *testing.T) {
		account := createTestAccount(t, client)
		clearTestEmails(t, client)

		session1Token := account.Token

		loginResp := loginTestUser(t, client, account.Email, account.Password)
		session2Token := loginResp.Token

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
		resp.Body.Close()

		resp, err = client.getWithAuth("/users/@me", session1Token)
		if err != nil {
			t.Fatalf("failed to check session1 after reset: %v", err)
		}
		if resp.StatusCode != http.StatusUnauthorized {
			t.Fatalf("expected session1 to be invalidated after password reset, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
		resp.Body.Close()

		resp, err = client.getWithAuth("/users/@me", session2Token)
		if err != nil {
			t.Fatalf("failed to check session2 after reset: %v", err)
		}
		if resp.StatusCode != http.StatusUnauthorized {
			t.Fatalf("expected session2 to be invalidated after password reset, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
		resp.Body.Close()

		loginResp = loginTestUser(t, client, account.Email, newPassword)
		if loginResp.Token == "" {
			t.Fatalf("expected login with new password after reset to succeed")
		}
	})
}
