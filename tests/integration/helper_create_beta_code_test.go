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

func createBetaCode(t testing.TB, client *testClient, token string) string {
	t.Helper()
	resp, err := client.postJSONWithAuth("/users/@me/beta-codes", nil, token)
	if err != nil {
		t.Fatalf("failed to create beta code: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var entry betaCodeEntry
	decodeJSONResponse(t, resp, &entry)
	if entry.Code == "" {
		t.Fatalf("beta code response missing code")
	}
	return entry.Code
}
