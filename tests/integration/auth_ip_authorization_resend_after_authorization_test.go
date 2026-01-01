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

// TestAuthIPAuthorizationResendAfterAuthorization validates that attempting
// to resend after the IP has already been authorized fails appropriately.
func TestAuthIPAuthorizationResendAfterAuthorization(t *testing.T) {
	client := newTestClient(t)
	originalIP := client.clientIP

	email := fmt.Sprintf("ip-resend-after-auth-%d@example.com", time.Now().UnixNano())
	password := uniquePassword()

	registerTestUser(t, client, email, password)
	clearTestEmails(t, client)

	newIP := "10.26.27.28"
	if newIP == originalIP {
		newIP = "10.26.27.29"
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

	var ipAuthResp3 struct {
		Ticket                  string `json:"ticket"`
		IPAuthorizationRequired bool   `json:"ip_authorization_required"`
	}
	decodeJSONResponse(t, resp, &ipAuthResp3)

	emailData := waitForEmail(t, client, "ip_authorization", email)
	authToken := emailData.Metadata["token"]

	resp, err = clientFromNewIP.postJSON("/auth/authorize-ip", map[string]string{"token": authToken})
	if err != nil {
		t.Fatalf("failed to authorize IP: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	time.Sleep(2 * time.Second)

	resp, err = clientFromNewIP.postJSON("/auth/ip-authorization/resend", map[string]string{
		"ticket": ipAuthResp3.Ticket,
	})
	if err != nil {
		t.Fatalf("failed to attempt resend after authorization: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusNoContent {
		t.Fatalf("expected resend after authorization to fail, got success status %d", resp.StatusCode)
	}
}
