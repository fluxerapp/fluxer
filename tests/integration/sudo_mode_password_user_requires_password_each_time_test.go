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

// TestSudoModePasswordUserRequiresPasswordEachTime verifies that users without MFA
// must provide their password for each sensitive operation and do not receive
// a sudo token that would allow bypassing password verification.
func TestSudoModePasswordUserRequiresPasswordEachTime(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	resp, err := client.postJSONWithAuth("/users/@me/disable", map[string]string{
		"password": account.Password,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to disable account: %v", err)
	}
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("expected 204, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	sudoToken := resp.Header.Get(sudoModeHeader)
	if sudoToken != "" {
		t.Fatalf("password-only users should not receive sudo tokens, but got: %s", sudoToken)
	}
	resp.Body.Close()

	login := loginTestUser(t, client, account.Email, account.Password)
	account.Token = login.Token

	resp, err = client.postJSONWithAuth("/users/@me/disable", map[string]any{}, account.Token)
	if err != nil {
		t.Fatalf("failed to make request: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 for missing password, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var errResp errorResponse
	decodeJSONResponse(t, resp, &errResp)
	if errResp.Code != "SUDO_MODE_REQUIRED" {
		t.Fatalf("expected SUDO_MODE_REQUIRED error code, got: %s", errResp.Code)
	}
}
