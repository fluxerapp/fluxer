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

// TestOAuth2AuthorizationsDeauthorizeOneOfMany verifies that deauthorizing
// one application does not affect other authorized applications.
func TestOAuth2AuthorizationsDeauthorizeOneOfMany(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/authz/partial"

	appID1, _, _, clientSecret1 := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Partial Deauth 1 %d", time.Now().UnixNano()),
		[]string{redirectURI},
		[]string{"identify"},
	)
	authCode1, _ := authorizeOAuth2(t, client, endUser.Token, appID1, redirectURI, []string{"identify"}, "", "", "")
	tokens1 := exchangeOAuth2AuthorizationCode(t, client, appID1, clientSecret1, authCode1, redirectURI, "")

	appID2, _, _, clientSecret2 := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Partial Deauth 2 %d", time.Now().UnixNano()),
		[]string{redirectURI},
		[]string{"identify"},
	)
	authCode2, _ := authorizeOAuth2(t, client, endUser.Token, appID2, redirectURI, []string{"identify"}, "", "", "")
	tokens2 := exchangeOAuth2AuthorizationCode(t, client, appID2, clientSecret2, authCode2, redirectURI, "")

	resp, err := client.delete(fmt.Sprintf("/oauth2/@me/authorizations/%s", appID1), endUser.Token)
	if err != nil {
		t.Fatalf("failed to deauthorize: %v", err)
	}
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("deauthorize failed with status %d", resp.StatusCode)
	}

	req, _ := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/oauth2/userinfo", client.baseURL), nil)
	req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", tokens1.AccessToken))
	client.applyCommonHeaders(req)
	resp1, _ := client.httpClient.Do(req)
	if resp1.Body != nil {
		resp1.Body.Close()
	}
	if resp1.StatusCode != http.StatusUnauthorized {
		t.Fatalf("expected first app token to be revoked, got status %d", resp1.StatusCode)
	}

	userInfo := getOAuth2UserInfo(t, client, tokens2.AccessToken)
	if userInfo["sub"] == nil {
		t.Fatal("second app's token should still work")
	}

	resp, err = client.getWithAuth("/oauth2/@me/authorizations", endUser.Token)
	if err != nil {
		t.Fatalf("failed to list authorizations: %v", err)
	}
	var authorizations []oauth2AuthorizationResponse
	decodeJSONResponse(t, resp, &authorizations)

	if len(authorizations) != 1 {
		t.Fatalf("expected 1 authorization after partial deauth, got %d", len(authorizations))
	}
	if authorizations[0].Application.ID != appID2 {
		t.Fatalf("expected remaining app to be %s, got %s", appID2, authorizations[0].Application.ID)
	}
}
