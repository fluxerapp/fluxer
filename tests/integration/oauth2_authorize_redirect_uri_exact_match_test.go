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

// TestOAuth2AuthorizeRedirectURIExactMatch verifies that redirect URIs
// must match exactly (no partial matches).
func TestOAuth2AuthorizeRedirectURIExactMatch(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	registeredURI := "https://example.com/callback"

	appID, _, _, _ := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Exact Match %d", time.Now().UnixNano()),
		[]string{registeredURI},
		[]string{"identify"},
	)

	testCases := []struct {
		name        string
		redirectURI string
	}{
		{"with extra path", "https://example.com/callback/extra"},
		{"with query param", "https://example.com/callback?foo=bar"},
		{"with fragment", "https://example.com/callback#fragment"},
		{"different scheme", "http://example.com/callback"},
		{"different host", "https://other.com/callback"},
		{"different port", "https://example.com:8080/callback"},
		{"trailing slash", "https://example.com/callback/"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			resp, err := client.postJSONWithAuth("/oauth2/authorize/consent", map[string]any{
				"response_type": "code",
				"client_id":     appID,
				"redirect_uri":  tc.redirectURI,
				"scope":         "identify",
				"state":         "test-state",
			}, endUser.Token)
			if err != nil {
				t.Fatalf("request failed: %v", err)
			}
			if resp.Body != nil {
				resp.Body.Close()
			}

			if resp.StatusCode == http.StatusOK {
				t.Fatalf("should not accept non-registered redirect URI %s", tc.redirectURI)
			}
		})
	}
}
