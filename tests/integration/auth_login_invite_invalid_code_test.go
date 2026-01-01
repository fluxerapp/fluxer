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

// Login with an invalid invite_code should still succeed and not add guilds.
func TestAuthLoginInviteInvalidCode(t *testing.T) {
	client := newTestClient(t)
	member := createTestAccount(t, client)

	badCode := "invalidcode123"
	loginReq := loginRequest{
		Email:      member.Email,
		Password:   member.Password,
		InviteCode: &badCode,
	}

	resp, err := client.postJSON("/auth/login", loginReq)
	if err != nil {
		t.Fatalf("failed to login with invalid invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var loginResp loginResponse
	decodeJSONResponse(t, resp, &loginResp)
	if loginResp.Token == "" {
		t.Fatalf("expected login to return token")
	}
	resp.Body.Close()

	guildsResp, err := client.getWithAuth("/users/@me/guilds", loginResp.Token)
	if err != nil {
		t.Fatalf("failed to fetch guilds: %v", err)
	}
	assertStatus(t, guildsResp, http.StatusOK)
	var guilds []struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, guildsResp, &guilds)
	guildsResp.Body.Close()

	if len(guilds) != 0 {
		t.Fatalf("expected no guilds joined when invite code is invalid, got %d", len(guilds))
	}
}
