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

// TestAuthIPAuthorizationBypassFlagAddedLater validates that when a bypass flag
// is added to an existing user, they can immediately login from new IPs without
// requiring authorization.
func TestAuthIPAuthorizationBypassFlagAddedLater(t *testing.T) {
	client := newTestClient(t)
	originalIP := client.clientIP

	email := fmt.Sprintf("bypass-added-later-%d@example.com", time.Now().UnixNano())
	password := uniquePassword()

	reg := registerTestUser(t, client, email, password)

	newIP := "10.80.90.100"
	if newIP == originalIP {
		newIP = "10.80.90.101"
	}
	clientFromNewIP := &testClient{
		baseURL:    client.baseURL,
		httpClient: client.httpClient,
		clientIP:   newIP,
	}

	loginReq := loginRequest{
		Email:    email,
		Password: password,
	}

	resp, err := clientFromNewIP.postJSON("/auth/login", loginReq)
	if err != nil {
		t.Fatalf("failed to attempt login from new IP: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		body := readResponseBody(resp)
		t.Fatalf("expected first login attempt to require IP auth, got %d: %s", resp.StatusCode, body)
	}
	resp.Body.Close()

	// Add the APP_STORE_REVIEWER flag
	updateUserSecurityFlags(t, client, reg.UserID, userSecurityFlagsPayload{
		SetFlags: []string{"APP_STORE_REVIEWER"},
	})

	resp, err = clientFromNewIP.postJSON("/auth/login", loginReq)
	if err != nil {
		t.Fatalf("failed to login after adding bypass flag: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body := readResponseBody(resp)
		t.Fatalf("expected login to succeed after adding bypass flag, got %d: %s", resp.StatusCode, body)
	}

	var loginResp loginResponse
	decodeJSONResponse(t, resp, &loginResp)

	if loginResp.Token == "" {
		t.Fatalf("expected login response to include token")
	}
}
