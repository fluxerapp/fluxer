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

// TestOAuth2TokenRefreshPreservesScopes verifies that refresh maintains
// the original granted scopes.
func TestOAuth2TokenRefreshPreservesScopes(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/refresh/scopes"
	appID, _, _, clientSecret := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Refresh Scopes %d", time.Now().UnixNano()),
		[]string{redirectURI},
		[]string{"identify", "email", "guilds"},
	)

	requestedScopes := []string{"identify", "email"}
	authCode, _ := authorizeOAuth2(
		t,
		client,
		endUser.Token,
		appID,
		redirectURI,
		requestedScopes,
		"",
		"",
		"",
	)
	initialTokens := exchangeOAuth2AuthorizationCode(
		t, client,
		appID,
		clientSecret,
		authCode,
		redirectURI,
		"",
	)

	refreshedTokens := refreshOAuth2Token(t, client, appID, clientSecret, initialTokens.RefreshToken)

	if refreshedTokens.Scope != "identify email" {
		t.Fatalf("refresh should preserve scopes: expected 'identify email', got %q", refreshedTokens.Scope)
	}

	introspection := introspectOAuth2Token(t, client, appID, clientSecret, refreshedTokens.AccessToken)
	if introspection.Scope != "identify email" {
		t.Fatalf("introspection scope should be 'identify email', got %s", introspection.Scope)
	}
}
