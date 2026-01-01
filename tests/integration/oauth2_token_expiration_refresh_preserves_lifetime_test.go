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

// TestOAuth2TokenExpirationRefreshPreservesLifetime verifies that refreshed
// tokens get a new expiration time.
func TestOAuth2TokenExpirationRefreshPreservesLifetime(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/expire/refresh"
	appID, _, _, clientSecret := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Refresh Exp %d", time.Now().UnixNano()),
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
	initialTokens := exchangeOAuth2AuthorizationCode(
		t, client,
		appID,
		clientSecret,
		authCode,
		redirectURI,
		"",
	)

	initialIntrospection := introspectOAuth2Token(t, client, appID, clientSecret, initialTokens.AccessToken)
	initialExp := initialIntrospection.Exp

	time.Sleep(2 * time.Second)

	refreshedTokens := refreshOAuth2Token(t, client, appID, clientSecret, initialTokens.RefreshToken)

	refreshedIntrospection := introspectOAuth2Token(t, client, appID, clientSecret, refreshedTokens.AccessToken)
	refreshedExp := refreshedIntrospection.Exp

	if refreshedExp <= initialExp {
		t.Fatalf("refreshed token exp (%d) should be later than initial exp (%d)", refreshedExp, initialExp)
	}

	if initialTokens.ExpiresIn != refreshedTokens.ExpiresIn {
		t.Logf("Note: expires_in changed from %d to %d after refresh", initialTokens.ExpiresIn, refreshedTokens.ExpiresIn)
	}
}
