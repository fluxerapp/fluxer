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

// TestAuthIpAuthorizationTicketFlow validates the new IP authorization ticket error response
// and the resend rate limit behavior.
func TestAuthIpAuthorizationTicketFlow(t *testing.T) {
	client := newTestClient(t)
	primaryIP := client.clientIP

	email := fmt.Sprintf("ip-ticket-%d@example.com", time.Now().UnixNano())
	password := uniquePassword()

	_ = registerTestUser(t, client, email, password)

	otherIP := "10.55.44.33"
	if otherIP == primaryIP {
		otherIP = "10.55.44.34"
	}

	clientWithNewIP := &testClient{
		baseURL:    client.baseURL,
		httpClient: client.httpClient,
		clientIP:   otherIP,
	}

	loginReq := loginRequest{
		Email:    email,
		Password: password,
	}

	resp, err := clientWithNewIP.postJSON("/auth/login", loginReq)
	if err != nil {
		t.Fatalf("failed to call login endpoint: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusForbidden {
		body := readResponseBody(resp)
		t.Fatalf("expected login to return 403 for IP authorization flow, got %d: %s", resp.StatusCode, body)
	}

	var body struct {
		Code                    string `json:"code"`
		Ticket                  string `json:"ticket"`
		IPAuthorizationRequired bool   `json:"ip_authorization_required"`
		Email                   string `json:"email"`
		ResendAvailableIn       int    `json:"resend_available_in"`
		Message                 string `json:"message"`
	}
	decodeJSONResponse(t, resp, &body)

	if !body.IPAuthorizationRequired || body.Ticket == "" || body.Email == "" {
		t.Fatalf("expected ip authorization payload, got: %+v", body)
	}

	resendResp, err := clientWithNewIP.postJSON("/auth/ip-authorization/resend", map[string]string{"ticket": body.Ticket})
	if err != nil {
		t.Fatalf("failed to call resend endpoint: %v", err)
	}
	defer resendResp.Body.Close()

	if resendResp.StatusCode != http.StatusTooManyRequests {
		t.Fatalf("expected resend to be rate limited with 429, got %d: %s", resendResp.StatusCode, readResponseBody(resendResp))
	}
}
