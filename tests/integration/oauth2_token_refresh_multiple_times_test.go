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

// TestOAuth2TokenRefreshMultipleTimes verifies that refresh tokens can be
// used multiple times in sequence (refresh rotation).
func TestOAuth2TokenRefreshMultipleTimes(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/refresh/multiple"
	appID, _, _, clientSecret := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Multiple Refresh %d", time.Now().UnixNano()),
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

	currentRefreshToken := tokens.RefreshToken
	for i := 0; i < 3; i++ {
		t.Logf("Refresh iteration %d", i+1)

		refreshed := refreshOAuth2Token(t, client, appID, clientSecret, currentRefreshToken)
		if refreshed.AccessToken == "" {
			t.Fatalf("refresh %d failed to return access token", i+1)
		}
		if refreshed.RefreshToken == "" {
			t.Fatalf("refresh %d failed to return new refresh token", i+1)
		}

		userInfo := getOAuth2UserInfo(t, client, refreshed.AccessToken)
		if userInfo["sub"] == nil {
			t.Fatalf("access token from refresh %d should work", i+1)
		}

		currentRefreshToken = refreshed.RefreshToken
	}
}
