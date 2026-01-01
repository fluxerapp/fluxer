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

// TestOAuth2AuthorizationsMultipleApps verifies that multiple authorized
// applications are correctly listed.
func TestOAuth2AuthorizationsMultipleApps(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/authz/multi"

	appID1, _, _, clientSecret1 := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Multi Test App 1 %d", time.Now().UnixNano()),
		[]string{redirectURI},
		[]string{"identify"},
	)
	authCode1, _ := authorizeOAuth2(t, client, endUser.Token, appID1, redirectURI, []string{"identify"}, "", "", "")
	exchangeOAuth2AuthorizationCode(t, client, appID1, clientSecret1, authCode1, redirectURI, "")

	appID2, _, _, clientSecret2 := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Multi Test App 2 %d", time.Now().UnixNano()),
		[]string{redirectURI},
		[]string{"identify", "email"},
	)
	authCode2, _ := authorizeOAuth2(t, client, endUser.Token, appID2, redirectURI, []string{"identify", "email"}, "", "", "")
	exchangeOAuth2AuthorizationCode(t, client, appID2, clientSecret2, authCode2, redirectURI, "")

	resp, err := client.getWithAuth("/oauth2/@me/authorizations", endUser.Token)
	if err != nil {
		t.Fatalf("failed to list authorizations: %v", err)
	}
	var authorizations []oauth2AuthorizationResponse
	decodeJSONResponse(t, resp, &authorizations)

	if len(authorizations) != 2 {
		t.Fatalf("expected 2 authorizations, got %d", len(authorizations))
	}

	foundApp1 := false
	foundApp2 := false
	for _, authz := range authorizations {
		if authz.Application.ID == appID1 {
			foundApp1 = true
		}
		if authz.Application.ID == appID2 {
			foundApp2 = true
		}
	}

	if !foundApp1 || !foundApp2 {
		t.Fatal("expected both applications to be in the list")
	}
}
