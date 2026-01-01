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

// TestAuthIPAuthorizationMultipleIPs validates that a user can authorize
// multiple different IPs independently.
func TestAuthIPAuthorizationMultipleIPs(t *testing.T) {
	client := newTestClient(t)
	originalIP := client.clientIP

	email := fmt.Sprintf("ip-multi-%d@example.com", time.Now().UnixNano())
	password := uniquePassword()

	registerTestUser(t, client, email, password)
	clearTestEmails(t, client)

	loginReq := loginRequest{
		Email:    email,
		Password: password,
	}

	firstNewIP := "10.11.12.13"
	if firstNewIP == originalIP {
		firstNewIP = "10.11.12.14"
	}
	clientFromFirstIP := &testClient{
		baseURL:    client.baseURL,
		httpClient: client.httpClient,
		clientIP:   firstNewIP,
	}

	resp, err := clientFromFirstIP.postJSON("/auth/login", loginReq)
	if err != nil {
		t.Fatalf("failed to login from first new IP: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		body := readResponseBody(resp)
		t.Fatalf("expected first new IP to trigger authorization, got %d: %s", resp.StatusCode, body)
	}
	resp.Body.Close()

	email1 := waitForEmail(t, client, "ip_authorization", email)
	token1 := email1.Metadata["token"]
	resp, err = clientFromFirstIP.postJSON("/auth/authorize-ip", map[string]string{"token": token1})
	if err != nil {
		t.Fatalf("failed to authorize first IP: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	clearTestEmails(t, client)

	secondNewIP := "10.22.33.44"
	if secondNewIP == originalIP || secondNewIP == firstNewIP {
		secondNewIP = "10.22.33.45"
	}
	clientFromSecondIP := &testClient{
		baseURL:    client.baseURL,
		httpClient: client.httpClient,
		clientIP:   secondNewIP,
	}

	resp, err = clientFromSecondIP.postJSON("/auth/login", loginReq)
	if err != nil {
		t.Fatalf("failed to login from second new IP: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		body := readResponseBody(resp)
		t.Fatalf("expected second new IP to trigger authorization, got %d: %s", resp.StatusCode, body)
	}
	resp.Body.Close()

	email2 := waitForEmail(t, client, "ip_authorization", email)
	token2 := email2.Metadata["token"]
	resp, err = clientFromSecondIP.postJSON("/auth/authorize-ip", map[string]string{"token": token2})
	if err != nil {
		t.Fatalf("failed to authorize second IP: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = clientFromFirstIP.postJSON("/auth/login", loginReq)
	if err != nil {
		t.Fatalf("failed to login from first IP after auth: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = clientFromSecondIP.postJSON("/auth/login", loginReq)
	if err != nil {
		t.Fatalf("failed to login from second IP after auth: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()
}
