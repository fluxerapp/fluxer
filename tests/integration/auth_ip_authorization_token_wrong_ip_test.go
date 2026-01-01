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

// TestAuthIPAuthorizationTokenForWrongIP validates that a token generated
// for one IP cannot be used to authorize a different IP.
func TestAuthIPAuthorizationTokenForWrongIP(t *testing.T) {
	client := newTestClient(t)
	originalIP := client.clientIP

	email := fmt.Sprintf("ip-wrong-ip-%d@example.com", time.Now().UnixNano())
	password := uniquePassword()

	registerTestUser(t, client, email, password)
	clearTestEmails(t, client)

	firstNewIP := "10.110.120.130"
	if firstNewIP == originalIP {
		firstNewIP = "10.110.120.131"
	}
	clientFromFirstIP := &testClient{
		baseURL:    client.baseURL,
		httpClient: client.httpClient,
		clientIP:   firstNewIP,
	}

	loginReq := loginRequest{
		Email:    email,
		Password: password,
	}

	resp, err := clientFromFirstIP.postJSON("/auth/login", loginReq)
	if err != nil {
		t.Fatalf("failed to trigger IP authorization from first IP: %v", err)
	}
	resp.Body.Close()

	emailData := waitForEmail(t, client, "ip_authorization", email)
	authToken := emailData.Metadata["token"]

	secondNewIP := "10.140.150.160"
	if secondNewIP == originalIP || secondNewIP == firstNewIP {
		secondNewIP = "10.140.150.161"
	}
	clientFromSecondIP := &testClient{
		baseURL:    client.baseURL,
		httpClient: client.httpClient,
		clientIP:   secondNewIP,
	}

	resp, err = clientFromSecondIP.postJSON("/auth/authorize-ip", map[string]string{"token": authToken})
	if err != nil {
		t.Fatalf("failed to attempt authorization from wrong IP: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode == http.StatusNoContent {
		resp2, err := clientFromSecondIP.postJSON("/auth/login", loginReq)
		if err != nil {
			t.Fatalf("failed to verify second IP still requires auth: %v", err)
		}
		defer resp2.Body.Close()

		if resp2.StatusCode != http.StatusForbidden {
			body := readResponseBody(resp2)
			t.Fatalf("expected second IP to still require authorization, got %d: %s", resp2.StatusCode, body)
		}
	}
}
