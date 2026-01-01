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

// TestOAuth2TokenRevokeCascade verifies that revoking a refresh token
// may cascade to associated access tokens (implementation-specific).
func TestOAuth2TokenRevokeCascade(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/revoke/cascade"
	appID, _, _, clientSecret := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Revoke Cascade %d", time.Now().UnixNano()),
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
	tokens := exchangeOAuth2AuthorizationCode(
		t, client,
		appID,
		clientSecret,
		authCode,
		redirectURI,
		"",
	)

	refreshed := refreshOAuth2Token(t, client, appID, clientSecret, tokens.RefreshToken)

	revokeOAuth2Token(t, client, appID, clientSecret, tokens.RefreshToken, "refresh_token")

	introspectionOriginal := introspectOAuth2Token(t, client, appID, clientSecret, tokens.AccessToken)
	introspectionRefreshed := introspectOAuth2Token(t, client, appID, clientSecret, refreshed.AccessToken)

	t.Logf("Original access token active after refresh token revocation: %v", introspectionOriginal.Active)
	t.Logf("Refreshed access token active after refresh token revocation: %v", introspectionRefreshed.Active)

}
