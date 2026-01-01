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

// TestAuthAppStoreReviewerIPBypass verifies that users with the APP_STORE_REVIEWER flag
// can login from any IP address without triggering the "new login location" email verification.
func TestAuthAppStoreReviewerIPBypass(t *testing.T) {
	client := newTestClient(t)
	initialIP := client.clientIP

	email := fmt.Sprintf("app-store-reviewer-%d@example.com", time.Now().UnixNano())
	password := uniquePassword()

	reg := registerTestUser(t, client, email, password)

	// Set the APP_STORE_REVIEWER flag on the user
	updateUserSecurityFlags(t, client, reg.UserID, userSecurityFlagsPayload{
		SetFlags: []string{"APP_STORE_REVIEWER"},
	})

	differentIP := "10.99.88.77"
	if differentIP == initialIP {
		differentIP = "10.99.88.78"
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

	if resp.StatusCode != http.StatusOK {
		body := readResponseBody(resp)
		t.Fatalf("expected login to succeed for APP_STORE_REVIEWER from new IP, got status %d: %s", resp.StatusCode, body)
	}

	var loginResp loginResponse
	decodeJSONResponse(t, resp, &loginResp)

	if loginResp.MFA {
		t.Fatalf("expected MFA to be false for APP_STORE_REVIEWER account without MFA enabled")
	}
	if loginResp.Token == "" {
		t.Fatalf("expected login response to include token")
	}
	if loginResp.UserID != reg.UserID {
		t.Fatalf("expected login user_id %s to match registration %s", loginResp.UserID, reg.UserID)
	}
}
