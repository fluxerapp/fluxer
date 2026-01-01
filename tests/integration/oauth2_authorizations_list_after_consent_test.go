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
	"net/http"
	"testing"
	"time"
)

// TestOAuth2AuthorizationsListAfterConsent verifies that after a user
// authorizes an OAuth2 application, it appears in their authorizations list.
func TestOAuth2AuthorizationsListAfterConsent(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/authz/callback"
	appID, _, _, clientSecret := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Auth List Test %d", time.Now().UnixNano()),
		[]string{redirectURI},
		[]string{"identify", "email"},
	)

	authCode, _ := authorizeOAuth2(
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

	exchangeOAuth2AuthorizationCode(
		t, client,
		appID,
		clientSecret,
		authCode,
		redirectURI,
		"",
	)

	resp, err := client.getWithAuth("/oauth2/@me/authorizations", endUser.Token)
	if err != nil {
		t.Fatalf("failed to list authorizations: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("list authorizations failed with status %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var authorizations []oauth2AuthorizationResponse
	decodeJSONResponse(t, resp, &authorizations)

	if len(authorizations) != 1 {
		t.Fatalf("expected 1 authorization, got %d", len(authorizations))
	}

	authz := authorizations[0]
	if authz.Application.ID != appID {
		t.Fatalf("expected application ID %s, got %s", appID, authz.Application.ID)
	}

	hasIdentify := false
	hasEmail := false
	for _, scope := range authz.Scopes {
		if scope == "identify" {
			hasIdentify = true
		}
		if scope == "email" {
			hasEmail = true
		}
		if scope == "bot" {
			t.Fatal("authorizations list should not include bot-only scopes")
		}
	}
	if !hasIdentify || !hasEmail {
		t.Fatalf("expected identify and email scopes, got %v", authz.Scopes)
	}

	if authz.AuthorizedAt == "" {
		t.Fatal("authorized_at should not be empty")
	}
}
