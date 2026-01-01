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

// TestAuthIPAuthorizationFlow validates the complete IP authorization flow:
// 1. User registers from IP A
// 2. User attempts to login from IP B (new IP)
// 3. Login is blocked with 403 and returns IP authorization ticket
// 4. User retrieves IP authorization token from email
// 5. User authorizes the new IP with the token
// 6. User can now login from IP B successfully
func TestAuthIPAuthorizationFlow(t *testing.T) {
	client := newTestClient(t)
	originalIP := client.clientIP

	email := fmt.Sprintf("ip-auth-flow-%d@example.com", time.Now().UnixNano())
	password := uniquePassword()

	reg := registerTestUser(t, client, email, password)
	clearTestEmails(t, client)

	newIP := "10.20.30.40"
	if newIP == originalIP {
		newIP = "10.20.30.41"
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
		t.Fatalf("failed to call login endpoint from new IP: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusForbidden {
		body := readResponseBody(resp)
		t.Fatalf("expected login from new IP to return 403, got %d: %s", resp.StatusCode, body)
	}

	var ipAuthResp struct {
		Code                    string `json:"code"`
		Ticket                  string `json:"ticket"`
		IPAuthorizationRequired bool   `json:"ip_authorization_required"`
		Email                   string `json:"email"`
		ResendAvailableIn       int    `json:"resend_available_in"`
		Message                 string `json:"message"`
	}
	decodeJSONResponse(t, resp, &ipAuthResp)

	if !ipAuthResp.IPAuthorizationRequired {
		t.Fatalf("expected ip_authorization_required to be true")
	}
	if ipAuthResp.Ticket == "" {
		t.Fatalf("expected ticket to be present in response")
	}
	if ipAuthResp.Email != email {
		t.Fatalf("expected email to match registered email, got %s", ipAuthResp.Email)
	}

	emailData := waitForEmail(t, client, "ip_authorization", email)
	authToken, ok := emailData.Metadata["token"]
	if !ok || authToken == "" {
		t.Fatalf("expected ip authorization token in email metadata")
	}

	resp, err = clientFromNewIP.postJSON("/auth/authorize-ip", map[string]string{"token": authToken})
	if err != nil {
		t.Fatalf("failed to authorize IP: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusNoContent {
		body := readResponseBody(resp)
		t.Fatalf("expected IP authorization to succeed with 204, got %d: %s", resp.StatusCode, body)
	}

	resp, err = clientFromNewIP.postJSON("/auth/login", loginReq)
	if err != nil {
		t.Fatalf("failed to login after IP authorization: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body := readResponseBody(resp)
		t.Fatalf("expected login to succeed after IP authorization, got %d: %s", resp.StatusCode, body)
	}

	var loginResp loginResponse
	decodeJSONResponse(t, resp, &loginResp)

	if loginResp.Token == "" {
		t.Fatalf("expected login response to include token")
	}
	if loginResp.UserID != reg.UserID {
		t.Fatalf("expected login user_id %s to match registration %s", loginResp.UserID, reg.UserID)
	}
}
