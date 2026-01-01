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
	"testing"
	"time"
)

// TestOAuth2TokenExpirationMultipleScopes verifies that expiration handling
// is consistent across different scope configurations.
func TestOAuth2TokenExpirationMultipleScopes(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/expire/scopes"
	appID, _, _, clientSecret := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Scopes Exp %d", time.Now().UnixNano()),
		[]string{redirectURI},
		[]string{"identify", "email", "guilds"},
	)

	testCases := []struct {
		name   string
		scopes []string
	}{
		{"single scope", []string{"identify"}},
		{"two scopes", []string{"identify", "email"}},
		{"three scopes", []string{"identify", "email", "guilds"}},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			authCode, _ := authorizeOAuth2(
				t,
				client,
				endUser.Token,
				appID,
				redirectURI,
				tc.scopes,
				"",
				"",
				"",
			)
			tokens := exchangeOAuth2AuthorizationCode(
				t, client,
				appID,
				clientSecret,
				authCode,
				redirectURI,
				"",
			)

			if tokens.ExpiresIn <= 0 {
				t.Fatalf("expires_in must be positive for scopes %v", tc.scopes)
			}

			introspection := introspectOAuth2Token(t, client, appID, clientSecret, tokens.AccessToken)
			if !introspection.Active {
				t.Fatalf("token should be active for scopes %v", tc.scopes)
			}
			if introspection.Exp <= time.Now().Unix() {
				t.Fatalf("exp should be in future for scopes %v", tc.scopes)
			}
		})
	}
}
