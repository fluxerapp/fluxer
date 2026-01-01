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

// TestOAuth2ClientSecretNotLeaked verifies that client_secret is only shown once
// during application creation and never exposed in subsequent GET requests.
// Secrets should remain write-only and must be stored securely by the application owner.
func TestOAuth2ClientSecretNotLeaked(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)

	redirectURI := "https://example.com/callback"
	appID, _, _, clientSecret := createOAuth2Application(t, client, appOwner,
		fmt.Sprintf("Secret Test App %d", time.Now().UnixNano()),
		[]string{redirectURI},
		[]string{"identify"},
	)

	if clientSecret == "" {
		t.Fatalf("client_secret should be returned on application creation")
	}

	t.Run("GET application does not expose client_secret", func(t *testing.T) {
		app := getOAuth2Application(t, client, appOwner.Token, appID)

		if app.ID != appID {
			t.Fatalf("expected application ID %s, got %s", appID, app.ID)
		}

		if app.ClientSecret != "" {
			t.Fatalf("GET application exposed the client_secret - security leak")
		}
	})

	t.Run("LIST applications does not expose client_secret", func(t *testing.T) {
		apps := listOAuth2Applications(t, client, appOwner.Token)

		var foundApp *oauth2ApplicationResponse
		for i := range apps {
			if apps[i].ID == appID {
				foundApp = &apps[i]
				break
			}
		}

		if foundApp == nil {
			t.Fatalf("created application not found in list")
			return
		}

		if foundApp.ClientSecret != "" {
			t.Fatalf("LIST applications exposed the client_secret - security leak")
		}
	})

	t.Run("UPDATE application does not expose client_secret", func(t *testing.T) {
		updates := map[string]any{
			"name": fmt.Sprintf("Updated App %d", time.Now().UnixNano()),
		}
		updatedApp := updateOAuth2Application(t, client, appOwner.Token, appID, updates)

		if updatedApp.ID != appID {
			t.Fatalf("expected application ID %s, got %s", appID, updatedApp.ID)
		}

		if updatedApp.ClientSecret != "" {
			t.Fatalf("UPDATE application exposed the client_secret - security leak")
		}
	})

	t.Run("client_secret still works for authentication", func(t *testing.T) {
		endUser := createTestAccount(t, client)

		authCode, _ := authorizeOAuth2(t, client, endUser.Token, appID, redirectURI, []string{"identify"}, "", "", "")

		token := exchangeOAuth2AuthorizationCode(t, client, appID, clientSecret, authCode, redirectURI, "")

		if token.AccessToken == "" {
			t.Fatalf("original client_secret should still work for token exchange")
		}
	})
}
