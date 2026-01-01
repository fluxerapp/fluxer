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

// TestOAuth2AuthorizeStateParameter verifies that the state parameter is
// correctly echoed back in the authorization redirect.
//
// The state parameter is used to prevent CSRF attacks by allowing the client
// to verify that the authorization response matches the request.
//
// Steps:
// 1. Submit authorization with a specific state value
// 2. Verify the same state value is returned in the redirect
func TestOAuth2AuthorizeStateParameter(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/state/callback"
	appID, _, _, clientSecret := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("State Test App %d", time.Now().UnixNano()),
		[]string{redirectURI},
		[]string{"identify"},
	)

	customState := "my-custom-state-12345"

	authCode, returnedState := authorizeOAuth2(
		t,
		client,
		endUser.Token,
		appID,
		redirectURI,
		[]string{"identify"},
		customState,
		"",
		"",
	)
	if authCode == "" {
		t.Fatal("authorization should return authorization code")
	}

	if returnedState != customState {
		t.Fatalf("expected state %q, got %q", customState, returnedState)
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
		t.Fatal("token exchange should succeed")
	}
}
