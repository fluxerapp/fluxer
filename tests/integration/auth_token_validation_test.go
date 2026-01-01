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

func TestAuthTokenValidation(t *testing.T) {
	client := newTestClient(t)

	t.Run("malformed token returns unauthorized", func(t *testing.T) {
		malformedTokens := []string{
			"",
			"not-a-token",
			"Bearer invalid",
			"invalid.token.format",
			"ey123.ey456.sig789",
		}

		for _, token := range malformedTokens {
			resp, err := client.getWithAuth("/users/@me", token)
			if err != nil {
				t.Fatalf("failed to call /users/@me with malformed token: %v", err)
			}

			if resp.StatusCode != http.StatusUnauthorized {
				t.Errorf("expected 401 Unauthorized for malformed token %q, got %d", token, resp.StatusCode)
			}
			resp.Body.Close()
		}
	})

	t.Run("non-existent token returns unauthorized", func(t *testing.T) {
		fakeToken := "MTcyNjg2NjQ1NjE5NTk5MjA2NA.GzMjIx.iFakeTokenThatDoesNotExistInDatabase123456789"

		resp, err := client.getWithAuth("/users/@me", fakeToken)
		if err != nil {
			t.Fatalf("failed to call /users/@me with fake token: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusUnauthorized {
			t.Fatalf("expected 401 Unauthorized for non-existent token, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
	})

	t.Run("revoked token returns unauthorized", func(t *testing.T) {
		account := createTestAccount(t, client)

		resp, err := client.getWithAuth("/users/@me", account.Token)
		if err != nil {
			t.Fatalf("failed to call /users/@me before logout: %v", err)
		}
		if resp.StatusCode != http.StatusOK {
			t.Fatalf("expected 200 OK before logout, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
		resp.Body.Close()

		logoutResp, err := client.postJSONWithAuth("/auth/logout", nil, account.Token)
		if err != nil {
			t.Fatalf("failed to call logout endpoint: %v", err)
		}
		assertStatus(t, logoutResp, http.StatusNoContent)
		logoutResp.Body.Close()

		resp, err = client.getWithAuth("/users/@me", account.Token)
		if err != nil {
			t.Fatalf("failed to call /users/@me after logout: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusUnauthorized {
			t.Fatalf("expected 401 Unauthorized for revoked token, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
	})

	t.Run("valid token allows access", func(t *testing.T) {
		account := createTestAccount(t, client)

		resp, err := client.getWithAuth("/users/@me", account.Token)
		if err != nil {
			t.Fatalf("failed to call /users/@me with valid token: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusOK {
			t.Fatalf("expected 200 OK with valid token, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}

		var user userPrivateResponse
		decodeJSONResponse(t, resp, &user)

		if user.ID != account.UserID {
			t.Fatalf("expected user ID %s, got %s", account.UserID, user.ID)
		}
	})

	t.Run("token with wrong signature returns unauthorized", func(t *testing.T) {
		account := createTestAccount(t, client)

		tamperedToken := account.Token[:len(account.Token)-10] + "0123456789"

		resp, err := client.getWithAuth("/users/@me", tamperedToken)
		if err != nil {
			t.Fatalf("failed to call /users/@me with tampered token: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusUnauthorized {
			t.Fatalf("expected 401 Unauthorized for tampered token, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
	})

	t.Run("missing authorization header returns unauthorized", func(t *testing.T) {
		resp, err := client.get("/users/@me")
		if err != nil {
			t.Fatalf("failed to call /users/@me without auth: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusUnauthorized {
			t.Fatalf("expected 401 Unauthorized for missing auth, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
	})
}
