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
	"fmt"
	"net/http"
	"testing"
	"time"
)

func TestAuthRegisterDerivesUsernameFromDisplayName(t *testing.T) {
	client := newTestClient(t)

	email := fmt.Sprintf("integration-derived-username-%d@example.com", time.Now().UnixNano())
	password := uniquePassword()

	payload := map[string]any{
		"email":         email,
		"password":      password,
		"global_name":   "Magic Tester",
		"date_of_birth": adultDateOfBirth(),
		"consent":       true,
	}

	resp, err := client.postJSON("/auth/register", payload)
	if err != nil {
		t.Fatalf("failed to call register endpoint: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("register returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var parsed registerResponse
	decodeJSONResponse(t, resp, &parsed)
	if parsed.Token == "" {
		t.Fatalf("expected register response to include a token")
	}

	meResp, err := client.getWithAuth("/users/@me", parsed.Token)
	if err != nil {
		t.Fatalf("failed to fetch current user: %v", err)
	}
	if meResp.StatusCode != http.StatusOK {
		t.Fatalf("/users/@me returned %d: %s", meResp.StatusCode, readResponseBody(meResp))
	}

	var userResp userPrivateResponse
	decodeJSONResponse(t, meResp, &userResp)
	if userResp.Username != "Magic_Tester" {
		t.Fatalf("expected derived username to be %q, got %q", "Magic_Tester", userResp.Username)
	}
}
