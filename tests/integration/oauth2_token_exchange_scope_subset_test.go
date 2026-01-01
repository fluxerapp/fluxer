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

// TestOAuth2TokenExchangeScopeSubset verifies that the token response
// contains the scopes that were actually granted.
func TestOAuth2TokenExchangeScopeSubset(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/token/scopes"
	appID, _, _, clientSecret := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Scope Test %d", time.Now().UnixNano()),
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

	tokenResp := exchangeOAuth2AuthorizationCode(
		t, client,
		appID,
		clientSecret,
		authCode,
		redirectURI,
		"",
	)

	if tokenResp.Scope != "identify email" {
		t.Fatalf("expected scope 'identify email', got %q", tokenResp.Scope)
	}

	introspection := introspectOAuth2Token(t, client, appID, clientSecret, tokenResp.AccessToken)
	if introspection.Scope != "identify email" {
		t.Fatalf("introspection scope should be 'identify email', got %s", introspection.Scope)
	}
}
