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

// TestAuthRegularUserRequiresIPAuthorization verifies that regular users (without
// APP_STORE_REVIEWER flag) still require IP authorization when logging in from a new location.
func TestAuthRegularUserRequiresIPAuthorization(t *testing.T) {
	client := newTestClient(t)
	initialIP := client.clientIP

	email := fmt.Sprintf("regular-user-ip-check-%d@example.com", time.Now().UnixNano())
	password := uniquePassword()

	_ = registerTestUser(t, client, email, password)

	differentIP := "10.88.77.66"
	if differentIP == initialIP {
		differentIP = "10.88.77.67"
	}
	clientWithDifferentIP := &testClient{
		baseURL:    client.baseURL,
		httpClient: client.httpClient,
		clientIP:   differentIP,
	}

	loginReq := loginRequest{
		Email:    email,
		Password: password,
	}

	resp, err := clientWithDifferentIP.postJSON("/auth/login", loginReq)
	if err != nil {
		t.Fatalf("failed to call login endpoint: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusForbidden {
		body := readResponseBody(resp)
		t.Fatalf("expected login to fail for regular user from new IP with 403, got status %d: %s", resp.StatusCode, body)
	}
}
