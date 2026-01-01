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

// TestAuthIPAuthorizationTicketExpiration validates that the ticket returned
// in the login response expires and cannot be used for resending after expiration.
func TestAuthIPAuthorizationTicketExpiration(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping ticket expiration test in short mode")
	}

	client := newTestClient(t)
	originalIP := client.clientIP

	email := fmt.Sprintf("ip-ticket-expire-%d@example.com", time.Now().UnixNano())
	password := uniquePassword()

	registerTestUser(t, client, email, password)

	newIP := "10.70.80.90"
	if newIP == originalIP {
		newIP = "10.70.80.91"
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

	var ipAuthResp struct {
		Ticket string `json:"ticket"`
	}
	decodeJSONResponse(t, resp, &ipAuthResp)

	if ipAuthResp.Ticket == "" {
		t.Fatalf("expected ticket in response")
	}

	expireIPAuthorization(t, client, ipAuthResp.Ticket, "")

	resp, err = clientFromNewIP.postJSON("/auth/ip-authorization/resend", map[string]string{
		"ticket": ipAuthResp.Ticket,
	})
	if err != nil {
		t.Fatalf("failed to attempt resend with expired ticket: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusNoContent {
		t.Fatalf("expected expired ticket to be rejected, got success status %d", resp.StatusCode)
	}

	acceptableStatuses := []int{
		http.StatusUnauthorized,
		http.StatusBadRequest,
		http.StatusGone,
		http.StatusNotFound,
	}

	statusAcceptable := false
	for _, status := range acceptableStatuses {
		if resp.StatusCode == status {
			statusAcceptable = true
			break
		}
	}

	if !statusAcceptable {
		body := readResponseBody(resp)
		t.Fatalf("expected expired ticket to return 401/400/410/404, got %d: %s", resp.StatusCode, body)
	}
}
