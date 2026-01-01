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

// Users without totpSecret should bypass TOTP check and still get a token.
func TestAuthLoginMfaTotpWithoutSecretReturnsSession(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	ticket := "mfa-no-secret"
	seedMfaTicket(t, client, ticket, account.UserID, 300)

	resp, err := client.postJSON("/auth/login/mfa/totp", map[string]string{
		"ticket": ticket,
		"code":   "123456",
	})
	if err != nil {
		t.Fatalf("failed to call login mfa totp: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var loginResp loginResponse
	decodeJSONResponse(t, resp, &loginResp)
	if loginResp.Token == "" {
		t.Fatalf("expected token for user without totpSecret")
	}
	resp.Body.Close()
}
