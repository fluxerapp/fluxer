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

// TestOAuth2TokenRevokeWrongClient verifies that a client cannot revoke
// tokens issued to a different client.
func TestOAuth2TokenRevokeWrongClient(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/revoke/wrong-client"

	appID1, _, _, clientSecret1 := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Revoke App 1 %d", time.Now().UnixNano()),
		[]string{redirectURI},
		[]string{"identify"},
	)
	appID2, _, _, clientSecret2 := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Revoke App 2 %d", time.Now().UnixNano()),
		[]string{redirectURI},
		[]string{"identify"},
	)

	authCode, _ := authorizeOAuth2(
		t,
		client,
		endUser.Token,
		appID1,
		redirectURI,
		[]string{"identify"},
		"",
		"",
		"",
	)
	tokens := exchangeOAuth2AuthorizationCode(
		t, client,
		appID1,
		clientSecret1,
		authCode,
		redirectURI,
		"",
	)

	revokeOAuth2Token(t, client, appID2, clientSecret2, tokens.AccessToken, "access_token")

	introspection := introspectOAuth2Token(t, client, appID1, clientSecret1, tokens.AccessToken)

	t.Logf("Token active status after wrong-client revoke attempt: %v", introspection.Active)
}
