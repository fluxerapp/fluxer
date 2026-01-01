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

// TestOAuth2ApplicationDeleteCleansUpResources validates comprehensive cleanup.
func TestOAuth2ApplicationDeleteCleansUpResources(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	name := fmt.Sprintf("Cleanup Test %d", time.Now().UnixNano())
	redirectURIs := []string{"https://example.com/callback"}
	scopes := []string{"identify", "email"}

	appID, botUserID, botToken := createOAuth2BotApplication(t, client, owner, name, redirectURIs)

	code, _ := authorizeOAuth2(t, client, endUser.Token, appID, redirectURIs[0], scopes, "", "", "")

	token := exchangeOAuth2AuthorizationCode(t, client, appID, "", code, redirectURIs[0], "")

	userInfo := getOAuth2UserInfo(t, client, token.AccessToken)
	if userInfo["sub"] == nil {
		t.Fatalf("token should work before deletion")
	}

	deleteOAuth2Application(t, client, owner, appID)

	resp1, _ := client.getWithAuth(fmt.Sprintf("/oauth2/applications/%s", appID), owner.Token)
	resp1.Body.Close()
	if resp1.StatusCode != http.StatusNotFound {
		t.Fatalf("application should be deleted")
	}

	resp2, _ := client.getWithAuth(fmt.Sprintf("/users/%s", botUserID), owner.Token)
	resp2.Body.Close()
	if resp2.StatusCode != http.StatusNotFound {
		t.Fatalf("bot user should be deleted")
	}

	req3, _ := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me", client.baseURL), nil)
	req3.Header.Set("Authorization", fmt.Sprintf("Bot %s", botToken))
	client.applyCommonHeaders(req3)
	resp3, _ := client.httpClient.Do(req3)
	resp3.Body.Close()
	if resp3.StatusCode == http.StatusOK {
		t.Fatalf("bot token should be invalidated")
	}

	req4, _ := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/oauth2/userinfo", client.baseURL), nil)
	req4.Header.Set("Authorization", fmt.Sprintf("Bearer %s", token.AccessToken))
	client.applyCommonHeaders(req4)
	resp4, _ := client.httpClient.Do(req4)
	resp4.Body.Close()
	if resp4.StatusCode == http.StatusOK {
		t.Fatalf("OAuth2 access tokens should be invalidated")
	}
}
