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

func verifyOriginalEmailChange(t testing.TB, client *testClient, account testAccount, ticket, code, password string) string {
	t.Helper()
	resp, err := client.postJSONWithAuth("/users/@me/email-change/verify-original", map[string]any{
		"ticket":   ticket,
		"code":     code,
		"password": password,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to verify original email: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	defer resp.Body.Close()
	var parsed emailChangeVerifyOriginalResponse
	decodeJSONResponse(t, resp, &parsed)
	if parsed.OriginalProof == "" {
		t.Fatalf("expected original_proof in response")
	}
	return parsed.OriginalProof
}
