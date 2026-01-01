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

func TestAuthSudoPasswordVerification(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	resp, err := client.getWithAuth("/auth/sessions", account.Token)
	if err != nil {
		t.Fatalf("failed to get sessions: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var sessions []authSessionResponse
	decodeJSONResponse(t, resp, &sessions)
	if len(sessions) == 0 {
		t.Fatalf("expected at least one session")
	}

	resp, err = client.postJSONWithAuth("/auth/sessions/logout", map[string]any{
		"session_id_hashes": []string{sessions[0].ID},
		"password":          account.Password,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to logout session with correct password: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.getWithAuth("/users/@me", account.Token)
	if err != nil {
		t.Fatalf("failed to check token after logout: %v", err)
	}
	if resp.StatusCode != http.StatusUnauthorized {
		t.Fatalf("expected token to be revoked after logout, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	account.login(t, client)

	resp, err = client.getWithAuth("/auth/sessions", account.Token)
	if err != nil {
		t.Fatalf("failed to get sessions after re-login: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	decodeJSONResponse(t, resp, &sessions)
	if len(sessions) == 0 {
		t.Fatalf("expected at least one session after re-login")
	}

	resp, err = client.postJSONWithAuth("/auth/sessions/logout", map[string]any{
		"session_id_hashes": []string{sessions[0].ID},
		"password":          "wrong-password",
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to call logout with wrong password: %v", err)
	}
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("expected 400 with wrong password, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	resp, err = client.getWithAuth("/users/@me", account.Token)
	if err != nil {
		t.Fatalf("failed to check token after failed logout: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.postJSONWithAuth("/auth/sessions/logout", map[string]any{
		"session_id_hashes": []string{sessions[0].ID},
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to call logout without password: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 without password, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()
}
