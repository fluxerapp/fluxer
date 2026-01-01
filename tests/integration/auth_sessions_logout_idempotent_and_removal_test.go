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

// Ensures logout endpoint is idempotent for unknown hashes and removes specified session when present.
func TestAuthSessionsLogoutIdempotentAndRemoval(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	resp, err := client.postJSONWithAuth("/auth/sessions/logout", map[string]any{
		"session_id_hashes": []string{"nonexistent-hash-1", "nonexistent-hash-2"},
		"password":          account.Password,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to call sessions/logout with unknown hashes: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.getWithAuth("/auth/sessions", account.Token)
	if err != nil {
		t.Fatalf("failed to fetch sessions: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var sessions []authSessionResponse
	decodeJSONResponse(t, resp, &sessions)
	resp.Body.Close()
	if len(sessions) == 0 {
		t.Fatalf("expected at least one session for logged-in user")
	}

	target := sessions[0].ID
	resp, err = client.postJSONWithAuth("/auth/sessions/logout", map[string]any{
		"session_id_hashes": []string{target},
		"password":          account.Password,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to logout specific session: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	account.login(t, client)

	resp, err = client.getWithAuth("/auth/sessions", account.Token)
	if err != nil {
		t.Fatalf("failed to fetch sessions after removal: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var sessionsAfter []authSessionResponse
	decodeJSONResponse(t, resp, &sessionsAfter)
	resp.Body.Close()

	for _, sess := range sessionsAfter {
		if sess.ID == target {
			t.Fatalf("expected session %s to be removed", target)
		}
	}
}
