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

func TestAuthConcurrentSessions(t *testing.T) {
	client := newTestClient(t)

	t.Run("same user can have multiple concurrent sessions", func(t *testing.T) {
		account := createTestAccount(t, client)

		session1Token := account.Token

		loginResp := loginTestUser(t, client, account.Email, account.Password)
		session2Token := loginResp.Token

		if session1Token == session2Token {
			t.Logf("warning: multiple logins returned the same token, may indicate single-session behavior")
		}

		resp1, err := client.getWithAuth("/users/@me", session1Token)
		if err != nil {
			t.Fatalf("failed to call /users/@me with session1 token: %v", err)
		}
		if resp1.StatusCode != http.StatusOK {
			t.Fatalf("expected session1 to be valid, got %d: %s", resp1.StatusCode, readResponseBody(resp1))
		}
		resp1.Body.Close()

		resp2, err := client.getWithAuth("/users/@me", session2Token)
		if err != nil {
			t.Fatalf("failed to call /users/@me with session2 token: %v", err)
		}
		if resp2.StatusCode != http.StatusOK {
			t.Fatalf("expected session2 to be valid, got %d: %s", resp2.StatusCode, readResponseBody(resp2))
		}
		resp2.Body.Close()
	})

	t.Run("logging out one session does not affect other sessions", func(t *testing.T) {
		account := createTestAccount(t, client)

		session1Token := account.Token

		loginResp := loginTestUser(t, client, account.Email, account.Password)
		session2Token := loginResp.Token

		loginResp = loginTestUser(t, client, account.Email, account.Password)
		session3Token := loginResp.Token

		resp, err := client.getWithAuth("/auth/sessions", session1Token)
		if err != nil {
			t.Fatalf("failed to list sessions: %v", err)
		}
		var sessions []authSessionResponse
		decodeJSONResponse(t, resp, &sessions)
		resp.Body.Close()

		if len(sessions) < 3 {
			t.Fatalf("expected at least 3 sessions, got %d", len(sessions))
		}

		logoutResp, err := client.postJSONWithAuth("/auth/logout", nil, session2Token)
		if err != nil {
			t.Fatalf("failed to logout session2: %v", err)
		}
		assertStatus(t, logoutResp, http.StatusNoContent)
		logoutResp.Body.Close()

		resp, err = client.getWithAuth("/users/@me", session1Token)
		if err != nil {
			t.Fatalf("failed to call /users/@me with session1: %v", err)
		}
		if resp.StatusCode != http.StatusOK {
			t.Fatalf("expected session1 to still be valid after session2 logout, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
		resp.Body.Close()

		resp, err = client.getWithAuth("/users/@me", session3Token)
		if err != nil {
			t.Fatalf("failed to call /users/@me with session3: %v", err)
		}
		if resp.StatusCode != http.StatusOK {
			t.Fatalf("expected session3 to still be valid after session2 logout, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
		resp.Body.Close()

		resp, err = client.getWithAuth("/users/@me", session2Token)
		if err != nil {
			t.Fatalf("failed to call /users/@me with session2: %v", err)
		}
		if resp.StatusCode != http.StatusUnauthorized {
			t.Fatalf("expected session2 to be invalid after logout, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
		resp.Body.Close()
	})

	t.Run("can list all active sessions", func(t *testing.T) {
		account := createTestAccount(t, client)

		loginTestUser(t, client, account.Email, account.Password)
		loginTestUser(t, client, account.Email, account.Password)

		resp, err := client.getWithAuth("/auth/sessions", account.Token)
		if err != nil {
			t.Fatalf("failed to list sessions: %v", err)
		}
		defer resp.Body.Close()

		assertStatus(t, resp, http.StatusOK)

		var sessions []authSessionResponse
		decodeJSONResponse(t, resp, &sessions)

		if len(sessions) == 0 {
			t.Fatalf("expected at least one session")
		}

		for _, session := range sessions {
			if session.ID == "" {
				t.Errorf("session missing ID")
			}
		}
	})

	t.Run("can log out specific session by ID", func(t *testing.T) {
		account := createTestAccount(t, client)

		loginResp := loginTestUser(t, client, account.Email, account.Password)
		session2Token := loginResp.Token

		resp, err := client.getWithAuth("/auth/sessions", account.Token)
		if err != nil {
			t.Fatalf("failed to list sessions: %v", err)
		}
		var sessions []authSessionResponse
		decodeJSONResponse(t, resp, &sessions)
		resp.Body.Close()

		if len(sessions) < 2 {
			t.Fatalf("expected at least 2 sessions, got %d", len(sessions))
		}

		targetSessionID := sessions[1].ID

		payload := map[string]any{
			"session_id_hashes": []string{targetSessionID},
			"password":          account.Password,
		}

		logoutResp, err := client.postJSONWithAuth("/auth/sessions/logout", payload, account.Token)
		if err != nil {
			t.Fatalf("failed to logout specific session: %v", err)
		}
		assertStatus(t, logoutResp, http.StatusNoContent)
		logoutResp.Body.Close()

		resp, err = client.getWithAuth("/users/@me", session2Token)
		if err != nil {
			t.Fatalf("failed to verify session2 is logged out: %v", err)
		}
		if resp.StatusCode != http.StatusUnauthorized {
			t.Fatalf("expected session2 to be logged out, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
		resp.Body.Close()
	})
}
