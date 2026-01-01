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

// TestAuthIPAuthorizationMultipleResendAttempts validates that multiple
// resend attempts in quick succession are all rate limited.
func TestAuthIPAuthorizationMultipleResendAttempts(t *testing.T) {
	client := newTestClient(t)
	originalIP := client.clientIP

	email := fmt.Sprintf("ip-multi-resend-%d@example.com", time.Now().UnixNano())
	password := uniquePassword()

	registerTestUser(t, client, email, password)

	newIP := "10.30.31.32"
	if newIP == originalIP {
		newIP = "10.30.31.33"
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

	for i := 0; i < 3; i++ {
		resp, err = clientFromNewIP.postJSON("/auth/ip-authorization/resend", map[string]string{
			"ticket": ipAuthResp.Ticket,
		})
		if err != nil {
			t.Fatalf("failed resend attempt %d: %v", i+1, err)
		}

		if resp.StatusCode != http.StatusTooManyRequests {
			body := readResponseBody(resp)
			t.Fatalf("expected resend attempt %d to be rate limited, got %d: %s", i+1, resp.StatusCode, body)
		}
		resp.Body.Close()

		time.Sleep(100 * time.Millisecond)
	}
}
