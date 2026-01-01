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

// TestOAuth2TokenExchangeWithoutAuthentication verifies that applications require authentication.
func TestOAuth2TokenExchangeWithoutAuthentication(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/token/no-auth"
	appID, _, _, _ := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("No Auth %d", time.Now().UnixNano()),
		[]string{redirectURI},
		[]string{"identify"},
	)

	authCode, _ := authorizeOAuth2(
		t,
		client,
		endUser.Token,
		appID,
		redirectURI,
		[]string{"identify"},
		"",
		"",
		"",
	)

	form := map[string][]string{
		"grant_type":   {"authorization_code"},
		"code":         {authCode},
		"redirect_uri": {redirectURI},
		"client_id":    {appID},
	}

	resp, err := client.postForm("/oauth2/token", form, "")
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode == http.StatusOK {
		t.Fatal("application should require authentication")
	}
	if resp.StatusCode != http.StatusUnauthorized {
		t.Fatalf("expected 401 for missing auth, got %d", resp.StatusCode)
	}
}
