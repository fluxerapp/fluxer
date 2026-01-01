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

// TestOAuth2AuthorizeCodeFlow verifies the basic authorization code flow works end-to-end.
// This tests the complete flow from authorization request through consent to token exchange.
//
// Steps:
// 1. Create an OAuth2 application
// 2. Submit authorization with consent (POST /oauth2/authorize/consent)
// 3. Exchange authorization code for access token (POST /oauth2/token)
// 4. Verify token works to access protected resources
func TestOAuth2AuthorizeCodeFlow(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/oauth/callback"
	appID, _, _, clientSecret := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("OAuth2 Basic Flow %d", time.Now().UnixNano()),
		[]string{redirectURI},
		[]string{"identify", "email"},
	)

	authCode, returnedState := authorizeOAuth2(
		t,
		client,
		endUser.Token,
		appID,
		redirectURI,
		[]string{"identify", "email"},
		"",
		"",
		"",
	)
	if authCode == "" {
		t.Fatal("authorization should return an authorization code")
	}
	if returnedState == "" {
		t.Fatal("authorization should return the state parameter")
	}

	tokenResp := exchangeOAuth2AuthorizationCode(
		t, client,
		appID,
		clientSecret,
		authCode,
		redirectURI,
		"",
	)

	if tokenResp.AccessToken == "" {
		t.Fatal("token response must include access_token")
	}
	if tokenResp.RefreshToken == "" {
		t.Fatal("token response must include refresh_token")
	}
	if tokenResp.TokenType != "Bearer" {
		t.Fatalf("token_type should be Bearer, got %s", tokenResp.TokenType)
	}
	if tokenResp.ExpiresIn <= 0 {
		t.Fatalf("expires_in must be positive, got %d", tokenResp.ExpiresIn)
	}
	if tokenResp.Scope != "identify email" {
		t.Fatalf("expected scope 'identify email', got %q", tokenResp.Scope)
	}

	userInfo := getOAuth2UserInfo(t, client, tokenResp.AccessToken)
	if userInfo["sub"] == nil {
		t.Fatal("userinfo must include 'sub' claim")
	}
	if userInfo["sub"].(string) != endUser.UserID {
		t.Fatalf("userinfo 'sub' should be %s, got %v", endUser.UserID, userInfo["sub"])
	}

	introspection := introspectOAuth2Token(t, client, appID, clientSecret, tokenResp.AccessToken)
	if !introspection.Active {
		t.Fatal("introspection should show token as active")
	}
	if introspection.ClientID != appID {
		t.Fatalf("introspection client_id should be %s, got %s", appID, introspection.ClientID)
	}
	if introspection.Scope != "identify email" {
		t.Fatalf("introspection scope should be 'identify email', got %s", introspection.Scope)
	}
}
