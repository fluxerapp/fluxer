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

// TestAuthIPAuthorizationRegularUserStillRequiresAuth validates that users
// without bypass flags still require IP authorization for new IPs, even with
// other non-bypass flags set.
func TestAuthIPAuthorizationRegularUserStillRequiresAuth(t *testing.T) {
	client := newTestClient(t)
	originalIP := client.clientIP

	email := fmt.Sprintf("regular-with-flags-%d@example.com", time.Now().UnixNano())
	password := uniquePassword()

	reg := registerTestUser(t, client, email, password)

	updateUserSecurityFlags(t, client, reg.UserID, userSecurityFlagsPayload{
		SetFlags: []string{"CTP_MEMBER", "BUG_HUNTER"},
	})

	newIP := "10.160.170.180"
	if newIP == originalIP {
		newIP = "10.160.170.181"
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
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusForbidden {
		body := readResponseBody(resp)
		t.Fatalf("expected regular user with non-bypass flags to require IP auth, got %d: %s", resp.StatusCode, body)
	}

	var ipAuthResp struct {
		IPAuthorizationRequired bool `json:"ip_authorization_required"`
	}
	decodeJSONResponse(t, resp, &ipAuthResp)

	if !ipAuthResp.IPAuthorizationRequired {
		t.Fatalf("expected ip_authorization_required to be true for regular user")
	}
}
