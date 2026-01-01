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

func TestAuthSessionsAndLogoutEndpoints(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	resp, err := client.getWithAuth("/auth/sessions", account.Token)
	if err != nil {
		t.Fatalf("failed to list sessions: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var sessions []authSessionResponse
	decodeJSONResponse(t, resp, &sessions)
	if len(sessions) == 0 {
		t.Fatalf("expected at least one session")
	}

	payload := map[string]any{
		"session_id_hashes": []string{sessions[0].ID},
		"password":          account.Password,
	}
	resp, err = client.postJSONWithAuth("/auth/sessions/logout", payload, account.Token)
	if err != nil {
		t.Fatalf("failed to call sessions logout: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	meResp, err := client.getWithAuth("/users/@me", account.Token)
	if err != nil {
		t.Fatalf("failed to call /users/@me after logout: %v", err)
	}
	if meResp.StatusCode != http.StatusUnauthorized {
		t.Fatalf("expected token to be revoked, got status %d: %s", meResp.StatusCode, readResponseBody(meResp))
	}
	meResp.Body.Close()

	account.login(t, client)

	resp, err = client.postJSONWithAuth("/auth/logout", nil, account.Token)
	if err != nil {
		t.Fatalf("failed to call logout: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	meResp, err = client.getWithAuth("/users/@me", account.Token)
	if err != nil {
		t.Fatalf("failed to call /users/@me post-logout: %v", err)
	}
	if meResp.StatusCode != http.StatusUnauthorized {
		t.Fatalf("expected unauthorized after logout, got %d", meResp.StatusCode)
	}
	meResp.Body.Close()
}
