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

// TestOAuth2TokenExpirationActiveState verifies that tokens are active
// before expiration and properly report their status.
func TestOAuth2TokenExpirationActiveState(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/expire/active"
	appID, _, _, clientSecret := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Active State %d", time.Now().UnixNano()),
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

	introspection := introspectOAuth2Token(t, client, appID, clientSecret, tokens.AccessToken)
	if !introspection.Active {
		t.Fatal("token should be active before expiration")
	}

	userInfo := getOAuth2UserInfo(t, client, tokens.AccessToken)
	if userInfo["sub"] == nil {
		t.Fatal("active token should work for API calls")
	}

	now := time.Now().Unix()
	timeUntilExpiration := introspection.Exp - now

	t.Logf("Token will expire in approximately %d seconds (%d minutes)",
		timeUntilExpiration, timeUntilExpiration/60)

	if timeUntilExpiration < 0 {
		t.Fatal("token appears to be already expired")
	}
}
