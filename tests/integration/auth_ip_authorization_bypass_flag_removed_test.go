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

// TestAuthIPAuthorizationBypassFlagRemoved validates that when a bypass flag
// is removed from a user, they once again require IP authorization for new IPs.
func TestAuthIPAuthorizationBypassFlagRemoved(t *testing.T) {
	client := newTestClient(t)
	originalIP := client.clientIP

	email := fmt.Sprintf("bypass-removed-%d@example.com", time.Now().UnixNano())
	password := uniquePassword()

	reg := registerTestUser(t, client, email, password)

	// Set the APP_STORE_REVIEWER flag initially
	updateUserSecurityFlags(t, client, reg.UserID, userSecurityFlagsPayload{
		SetFlags: []string{"APP_STORE_REVIEWER"},
	})

	newIP := "10.100.110.120"
	if newIP == originalIP {
		newIP = "10.100.110.121"
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
		t.Fatalf("failed to login with bypass flag: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		body := readResponseBody(resp)
		t.Fatalf("expected login with bypass flag to succeed, got %d: %s", resp.StatusCode, body)
	}
	resp.Body.Close()

	// Remove the APP_STORE_REVIEWER flag
	updateUserSecurityFlags(t, client, reg.UserID, userSecurityFlagsPayload{
		ClearFlags: []string{"APP_STORE_REVIEWER"},
	})

	anotherNewIP := "10.130.140.150"
	if anotherNewIP == originalIP || anotherNewIP == newIP {
		anotherNewIP = "10.130.140.151"
	}
	clientFromAnotherIP := &testClient{
		baseURL:    client.baseURL,
		httpClient: client.httpClient,
		clientIP:   anotherNewIP,
	}

	resp, err = clientFromAnotherIP.postJSON("/auth/login", loginReq)
	if err != nil {
		t.Fatalf("failed to attempt login after removing bypass flag: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusForbidden {
		body := readResponseBody(resp)
		t.Fatalf("expected login after removing bypass flag to require IP auth, got %d: %s", resp.StatusCode, body)
	}

	var ipAuthResp struct {
		IPAuthorizationRequired bool `json:"ip_authorization_required"`
	}
	decodeJSONResponse(t, resp, &ipAuthResp)

	if !ipAuthResp.IPAuthorizationRequired {
		t.Fatalf("expected ip_authorization_required to be true after flag removal")
	}
}
