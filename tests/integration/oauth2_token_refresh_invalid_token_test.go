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
	"net/url"
	"strings"
	"testing"
	"time"
)

// TestOAuth2TokenRefreshInvalidToken verifies that invalid refresh tokens
// are rejected.
func TestOAuth2TokenRefreshInvalidToken(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)

	redirectURI := "https://example.com/refresh/invalid"
	appID, _, _, clientSecret := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Invalid Refresh %d", time.Now().UnixNano()),
		[]string{redirectURI},
		[]string{"identify"},
	)

	form := url.Values{
		"grant_type":    {"refresh_token"},
		"refresh_token": {"invalid-refresh-token-12345"},
		"client_id":     {appID},
	}

	req, err := http.NewRequest(http.MethodPost, fmt.Sprintf("%s/oauth2/token", client.baseURL), strings.NewReader(form.Encode()))
	if err != nil {
		t.Fatalf("failed to build request: %v", err)
	}
	req.Header.Set("Content-Type", "application/x-www-form-urlencoded")
	req.SetBasicAuth(appID, clientSecret)
	client.applyCommonHeaders(req)

	resp, err := client.httpClient.Do(req)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode == http.StatusOK {
		t.Fatal("invalid refresh token should be rejected")
	}
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("expected 400 Bad Request, got %d", resp.StatusCode)
	}
}
