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

// TestOAuth2AuthorizationsBotOnlyNotListed verifies that bot-only authorizations
// (scope = "bot" only) do not appear in the authorizations list.
func TestOAuth2AuthorizationsBotOnlyNotListed(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/authz/bot"
	appID, _, _, clientSecret := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Bot Only Test %d", time.Now().UnixNano()),
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
	exchangeOAuth2AuthorizationCode(t, client, appID, clientSecret, authCode, redirectURI, "")

	resp, err := client.getWithAuth("/oauth2/@me/authorizations", endUser.Token)
	if err != nil {
		t.Fatalf("failed to list authorizations: %v", err)
	}
	var authorizations []oauth2AuthorizationResponse
	decodeJSONResponse(t, resp, &authorizations)

	if len(authorizations) != 1 {
		t.Fatalf("expected 1 authorization, got %d", len(authorizations))
	}

	for _, scope := range authorizations[0].Scopes {
		if scope == "bot" {
			t.Fatal("bot scope should be filtered from authorizations list")
		}
	}
}
