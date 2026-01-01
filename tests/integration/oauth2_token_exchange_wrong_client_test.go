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

// TestOAuth2TokenExchangeWrongClient verifies that an authorization code
// cannot be exchanged by a different client.
func TestOAuth2TokenExchangeWrongClient(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/token/wrong-client"

	appID1, _, _, _ := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("App 1 %d", time.Now().UnixNano()),
		[]string{redirectURI},
		[]string{"identify"},
	)
	appID2, _, _, clientSecret2 := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("App 2 %d", time.Now().UnixNano()),
		[]string{redirectURI},
		[]string{"identify"},
	)

	authCode, _ := authorizeOAuth2(
		t,
		client,
		endUser.Token,
		appID1,
		redirectURI,
		[]string{"identify"},
		"",
		"",
		"",
	)

	form := url.Values{
		"grant_type":   {"authorization_code"},
		"code":         {authCode},
		"redirect_uri": {redirectURI},
		"client_id":    {appID2},
	}

	req, err := http.NewRequest(http.MethodPost, fmt.Sprintf("%s/oauth2/token", client.baseURL), strings.NewReader(form.Encode()))
	if err != nil {
		t.Fatalf("failed to build request: %v", err)
	}
	req.Header.Set("Content-Type", "application/x-www-form-urlencoded")
	req.SetBasicAuth(appID2, clientSecret2)
	client.applyCommonHeaders(req)

	resp, err := client.httpClient.Do(req)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode == http.StatusOK {
		t.Fatal("should not allow exchanging code with wrong client")
	}
	if resp.StatusCode != http.StatusBadRequest && resp.StatusCode != http.StatusUnauthorized {
		t.Fatalf("expected 400/401 for wrong client, got %d", resp.StatusCode)
	}
}
