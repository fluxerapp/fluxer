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

// TestOAuth2TokenExpirationInResponse verifies that token responses include
// proper expiration information. Access tokens should expire after a set period,
// and the expires_in field indicates seconds until expiration.
//
// Steps:
// 1. Get access token
// 2. Verify expires_in is present and reasonable
// 3. Verify introspection shows expiration time
func TestOAuth2TokenExpirationInResponse(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/expire/callback"
	appID, _, _, clientSecret := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Expiration Test %d", time.Now().UnixNano()),
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

	if tokens.ExpiresIn <= 0 {
		t.Fatalf("expires_in must be positive, got %d", tokens.ExpiresIn)
	}

	maxExpiration := 365 * 24 * 60 * 60
	if tokens.ExpiresIn > maxExpiration {
		t.Fatalf("expires_in seems unreasonably long: %d seconds", tokens.ExpiresIn)
	}

	introspection := introspectOAuth2Token(t, client, appID, clientSecret, tokens.AccessToken)
	if introspection.Exp <= 0 {
		t.Fatal("introspection should include exp (expiration time)")
	}

	now := time.Now().Unix()
	if introspection.Exp <= now {
		t.Fatalf("exp should be in the future: exp=%d, now=%d", introspection.Exp, now)
	}

	expectedExp := now + int64(tokens.ExpiresIn)
	slack := int64(60)
	if introspection.Exp < expectedExp-slack || introspection.Exp > expectedExp+slack {
		t.Logf("Warning: exp (%d) doesn't closely match now + expires_in (%d)", introspection.Exp, expectedExp)
	}
}
