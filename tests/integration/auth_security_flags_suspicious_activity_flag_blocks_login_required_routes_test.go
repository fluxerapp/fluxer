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

func TestSuspiciousActivityFlagBlocksLoginRequiredRoutes(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	updateUserSecurityFlags(t, client, account.UserID, userSecurityFlagsPayload{
		SuspiciousActivityFlagNames: []string{"REQUIRE_VERIFIED_EMAIL"},
	})

	resp, err := client.getWithAuth("/users/@me", account.Token)
	if err != nil {
		t.Fatalf("failed to fetch /users/@me: %v", err)
	}
	assertStatus(t, resp, http.StatusForbidden)
	resp.Body.Close()

	resp, err = client.postJSONWithAuth("/auth/verify/resend", nil, account.Token)
	if err != nil {
		t.Fatalf("failed to call /auth/verify/resend: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	updateUserSecurityFlags(t, client, account.UserID, userSecurityFlagsPayload{
		SuspiciousActivityFlags: intPtr(0),
	})

	resp, err = client.getWithAuth("/users/@me", account.Token)
	if err != nil {
		t.Fatalf("failed to fetch /users/@me after clearing flags: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()
}
