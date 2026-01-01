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

func loginTestUser(t testing.TB, client *testClient, email, password string) loginResponse {
	t.Helper()

	req := loginRequest{
		Email:    email,
		Password: password,
	}

	resp, err := client.postJSON("/auth/login", req)
	if err != nil {
		t.Fatalf("failed to call login endpoint: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("login returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var loginResp loginResponse
	decodeJSONResponse(t, resp, &loginResp)
	return loginResp
}
