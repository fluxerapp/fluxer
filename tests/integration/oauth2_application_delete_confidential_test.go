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

// TestOAuth2ApplicationDeleteConfidential validates deleting an application invalidates tokens.
func TestOAuth2ApplicationDeleteConfidential(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	name := fmt.Sprintf("Delete Conf %d", time.Now().UnixNano())
	redirectURIs := []string{"https://example.com/callback"}
	scopes := []string{"identify"}

	appID, _, _, clientSecret := createOAuth2Application(t, client, owner, name, redirectURIs, scopes)

	user := createTestAccount(t, client)
	code, _ := authorizeOAuth2(t, client, user.Token, appID, redirectURIs[0], scopes, "", "", "")
	token := exchangeOAuth2AuthorizationCode(t, client, appID, clientSecret, code, redirectURIs[0], "")

	userInfo := getOAuth2UserInfo(t, client, token.AccessToken)
	if userInfo["sub"] == nil {
		t.Fatalf("token should work before deletion")
	}

	deleteOAuth2Application(t, client, owner, appID)

	resp, err := client.getWithAuth(fmt.Sprintf("/oauth2/applications/%s", appID), owner.Token)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	resp.Body.Close()

	if resp.StatusCode != http.StatusNotFound {
		t.Fatalf("deleted application should return 404, got %d", resp.StatusCode)
	}

	req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/oauth2/userinfo", client.baseURL), nil)
	if err != nil {
		t.Fatalf("failed to build userinfo request: %v", err)
	}
	req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", token.AccessToken))
	client.applyCommonHeaders(req)

	tokenResp, err := client.httpClient.Do(req)
	if err != nil {
		t.Fatalf("userinfo request failed: %v", err)
	}
	tokenResp.Body.Close()

	if tokenResp.StatusCode == http.StatusOK {
		t.Fatalf("tokens should be invalidated after application deletion")
	}
}
