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

// TestAuthIPAuthorizationTokenExpiration validates that IP authorization
// tokens expire after a reasonable time period and cannot be used after expiration.
//
// Note: This test assumes a relatively short expiration time (e.g., 15-30 minutes).
// If the actual expiration is longer, this test may need adjustment or be marked
// as a longer-running integration test.
func TestAuthIPAuthorizationTokenExpiration(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping expiration test in short mode")
	}

	client := newTestClient(t)
	originalIP := client.clientIP

	email := fmt.Sprintf("ip-expire-%d@example.com", time.Now().UnixNano())
	password := uniquePassword()

	registerTestUser(t, client, email, password)
	clearTestEmails(t, client)

	newIP := "10.40.50.60"
	if newIP == originalIP {
		newIP = "10.40.50.61"
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
		t.Fatalf("failed to trigger IP authorization: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		body := readResponseBody(resp)
		t.Fatalf("expected IP authorization required, got %d: %s", resp.StatusCode, body)
	}
	resp.Body.Close()

	emailData := waitForEmail(t, client, "ip_authorization", email)
	authToken := emailData.Metadata["token"]
	if authToken == "" {
		t.Fatalf("expected authorization token in email")
	}

	expireIPAuthorization(t, client, "", authToken)

	resp, err = clientFromNewIP.postJSON("/auth/authorize-ip", map[string]string{"token": authToken})
	if err != nil {
		t.Fatalf("failed to attempt authorization with expired token: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode == http.StatusNoContent || resp.StatusCode == http.StatusOK {
		t.Fatalf("expected expired token to be rejected, got success status %d", resp.StatusCode)
	}

	if resp.StatusCode != http.StatusUnauthorized && resp.StatusCode != http.StatusBadRequest && resp.StatusCode != http.StatusGone {
		body := readResponseBody(resp)
		t.Fatalf("expected expired token to return 401, 400, or 410, got %d: %s", resp.StatusCode, body)
	}

	resp, err = clientFromNewIP.postJSON("/auth/login", loginReq)
	if err != nil {
		t.Fatalf("failed to verify login still requires auth: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusForbidden {
		body := readResponseBody(resp)
		t.Fatalf("expected login to still require IP authorization after expired token, got %d: %s", resp.StatusCode, body)
	}
}
