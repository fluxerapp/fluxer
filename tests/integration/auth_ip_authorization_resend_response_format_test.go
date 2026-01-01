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

// TestAuthIPAuthorizationResendResponseFormat validates that the resend
// endpoint returns appropriate response data when rate limited.
func TestAuthIPAuthorizationResendResponseFormat(t *testing.T) {
	client := newTestClient(t)
	originalIP := client.clientIP

	email := fmt.Sprintf("ip-resend-format-%d@example.com", time.Now().UnixNano())
	password := uniquePassword()

	registerTestUser(t, client, email, password)

	newIP := "10.230.240.250"
	if newIP == originalIP {
		newIP = "10.230.240.251"
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

	var ipAuthResp2 struct {
		Ticket                  string `json:"ticket"`
		IPAuthorizationRequired bool   `json:"ip_authorization_required"`
		ResendAvailableIn       int    `json:"resend_available_in"`
	}
	decodeJSONResponse(t, resp, &ipAuthResp2)

	resp, err = clientFromNewIP.postJSON("/auth/ip-authorization/resend", map[string]string{
		"ticket": ipAuthResp2.Ticket,
	})
	if err != nil {
		t.Fatalf("failed to attempt resend: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusTooManyRequests {
		body := readResponseBody(resp)
		t.Fatalf("expected rate limit, got %d: %s", resp.StatusCode, body)
	}

	var rateLimitResp struct {
		Code              string `json:"code"`
		Message           string `json:"message"`
		ResendAvailableIn *int   `json:"resend_available_in,omitempty"`
		RetryAfter        *int   `json:"retry_after,omitempty"`
	}
	decodeJSONResponse(t, resp, &rateLimitResp)

	if rateLimitResp.Code == "" {
		t.Fatalf("expected error code in rate limit response")
	}
	if rateLimitResp.Message == "" {
		t.Fatalf("expected error message in rate limit response")
	}
}
