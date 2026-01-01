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
	"net/url"
	"testing"
	"time"
)

// TestOAuth2RevokeRequiresValidSecret verifies token revocation for confidential
// applications requires valid client authentication.
func TestOAuth2RevokeRequiresValidSecret(t *testing.T) {
	t.Parallel()

	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/revoke/secret-check"
	appID, _, _, clientSecret := createOAuth2Application(
		t,
		client,
		appOwner,
		fmt.Sprintf("Revoke Secret Check %d", time.Now().UnixNano()),
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
	tokens := exchangeOAuth2AuthorizationCode(t, client, appID, clientSecret, authCode, redirectURI, "")

	t.Run("reject invalid client secret on revoke", func(t *testing.T) {
		form := url.Values{
			"token":           {tokens.AccessToken},
			"token_type_hint": {"access_token"},
			"client_id":       {appID},
			"client_secret":   {"not-the-secret"},
		}
		resp, err := client.postForm("/oauth2/token/revoke", form, "")
		if err != nil {
			t.Fatalf("failed to call revoke: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK {
			t.Fatalf("revoke should fail with invalid client_secret")
		}
		if resp.StatusCode != http.StatusUnauthorized {
			t.Fatalf("expected 401 for invalid client_secret, got %d", resp.StatusCode)
		}
	})

	t.Run("reject missing client auth on revoke", func(t *testing.T) {
		form := url.Values{
			"token":           {tokens.AccessToken},
			"token_type_hint": {"access_token"},
			"client_id":       {appID},
		}
		resp, err := client.postForm("/oauth2/token/revoke", form, "")
		if err != nil {
			t.Fatalf("failed to call revoke: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK {
			t.Fatalf("revoke should fail when client authentication is missing")
		}
		if resp.StatusCode != http.StatusUnauthorized {
			t.Fatalf("expected 401 for missing client authentication, got %d", resp.StatusCode)
		}
	})
}
