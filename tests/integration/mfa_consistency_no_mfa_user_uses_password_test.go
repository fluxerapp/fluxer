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

// TestMfaConsistencyNoMfaUserUsesPassword verifies that users without any MFA
// can use their password for sudo verification, but do NOT receive a sudo token.
func TestMfaConsistencyNoMfaUserUsesPassword(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	resp, err := client.postJSONWithAuth("/users/@me/disable", map[string]string{
		"password": account.Password,
	}, account.Token)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("expected 204 for non-MFA user with password, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	sudoToken := resp.Header.Get(sudoModeHeader)
	if sudoToken != "" {
		t.Fatalf("expected no sudo token for non-MFA user, but got one")
	}

	t.Logf("correctly allowed password for non-MFA user without issuing sudo token")
}
