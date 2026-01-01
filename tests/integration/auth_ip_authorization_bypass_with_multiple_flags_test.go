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

// TestAuthIPAuthorizationBypassWithMultipleFlags validates that users with
// bypass flags can login from any IP without authorization, even when combined
// with other security flags.
func TestAuthIPAuthorizationBypassWithMultipleFlags(t *testing.T) {
	client := newTestClient(t)
	originalIP := client.clientIP

	testCases := []struct {
		name  string
		flags []string
	}{
		{
			name:  "APP_STORE_REVIEWER only",
			flags: []string{"APP_STORE_REVIEWER"},
		},
		{
			name:  "APP_STORE_REVIEWER with STAFF",
			flags: []string{"APP_STORE_REVIEWER", "STAFF"},
		},
		{
			name:  "APP_STORE_REVIEWER with CTP_MEMBER",
			flags: []string{"APP_STORE_REVIEWER", "CTP_MEMBER"},
		},
		{
			name:  "APP_STORE_REVIEWER with BUG_HUNTER",
			flags: []string{"APP_STORE_REVIEWER", "BUG_HUNTER"},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			email := fmt.Sprintf("bypass-flags-%d@example.com", time.Now().UnixNano())
			password := uniquePassword()

			reg := registerTestUser(t, client, email, password)

			updateUserSecurityFlags(t, client, reg.UserID, userSecurityFlagsPayload{
				SetFlags: tc.flags,
			})

			newIP := "10.50.60.70"
			if newIP == originalIP {
				newIP = "10.50.60.71"
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
				t.Fatalf("failed to login from new IP: %v", err)
			}
			defer resp.Body.Close()

			if resp.StatusCode != http.StatusOK {
				body := readResponseBody(resp)
				t.Fatalf("expected login to succeed for user with bypass flags from new IP, got %d: %s", resp.StatusCode, body)
			}

			var loginResp loginResponse
			decodeJSONResponse(t, resp, &loginResp)

			if loginResp.Token == "" {
				t.Fatalf("expected login response to include token")
			}
			if loginResp.UserID != reg.UserID {
				t.Fatalf("expected user_id %s to match registration %s", loginResp.UserID, reg.UserID)
			}
		})
	}
}
