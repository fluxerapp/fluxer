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

// TestOAuth2TokenExchange verifies exchanging an authorization code for tokens.
//
// Steps:
// 1. Get an authorization code
// 2. Exchange it for access token and refresh token
// 3. Verify token response format matches the expected OAuth2 contract
func TestOAuth2TokenExchange(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/token/exchange"
	appID, _, _, clientSecret := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Token Exchange %d", time.Now().UnixNano()),
		[]string{redirectURI},
		[]string{"identify", "email"},
	)

	authCode, _ := authorizeOAuth2(
		t, client,
		endUser.Token,
		appID,
		redirectURI,
		[]string{"identify", "email"},
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

	if tokenResp.AccessToken == "" {
		t.Fatal("access_token is required")
	}
	if tokenResp.TokenType != "Bearer" {
		t.Fatalf("token_type must be 'Bearer', got %q", tokenResp.TokenType)
	}
	if tokenResp.ExpiresIn <= 0 {
		t.Fatalf("expires_in must be positive, got %d", tokenResp.ExpiresIn)
	}
	if tokenResp.RefreshToken == "" {
		t.Fatal("refresh_token is required")
	}
	if tokenResp.Scope != "identify email" {
		t.Fatalf("scope should be 'identify email', got %q", tokenResp.Scope)
	}

	userInfo := getOAuth2UserInfo(t, client, tokenResp.AccessToken)
	if userInfo["sub"] == nil {
		t.Fatal("userinfo must include 'sub'")
	}
}
