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
	"strings"
	"testing"
	"time"
)

func authorizeOAuth2(t testing.TB, client *testClient, userToken string, clientID string, redirectURI string, scopes []string, state string, codeChallenge string, codeChallengeMethod string) (authCode string, returnedState string) {
	t.Helper()
	if state == "" {
		state = fmt.Sprintf("state-%d", time.Now().UnixNano())
	}
	_ = codeChallenge
	_ = codeChallengeMethod

	payload := map[string]any{
		"response_type": "code",
		"client_id":     clientID,
		"scope":         strings.Join(scopes, " "),
		"state":         state,
	}
	if redirectURI != "" {
		payload["redirect_uri"] = redirectURI
	}

	resp, err := client.postJSONWithAuth("/oauth2/authorize/consent", payload, userToken)
	if err != nil {
		t.Fatalf("failed to submit oauth2 authorization: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("oauth2 authorization failed: %s", readResponseBody(resp))
	}

	var consentResp struct {
		RedirectTo string `json:"redirect_to"`
	}
	decodeJSONResponse(t, resp, &consentResp)
	if consentResp.RedirectTo == "" {
		t.Fatalf("oauth2 authorization response missing redirect_to")
	}

	redirURL, err := url.Parse(consentResp.RedirectTo)
	if err != nil {
		t.Fatalf("failed to parse authorization redirect url: %v", err)
	}

	authCode = redirURL.Query().Get("code")
	if authCode == "" {
		t.Fatalf("redirect missing authorization code")
	}

	returnedState = redirURL.Query().Get("state")
	return authCode, returnedState
}

func createOAuth2Application(t testing.TB, client *testClient, owner testAccount, name string, redirectURIs []string, _ []string) (applicationID string, botUserID string, botToken string, clientSecret string) {
	t.Helper()
	payload := map[string]any{
		"name":          name,
		"redirect_uris": redirectURIs,
	}
	resp, err := client.postJSONWithAuth("/oauth2/applications", payload, owner.Token)
	if err != nil {
		t.Fatalf("failed to create application: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("application creation failed: %s", readResponseBody(resp))
	}
	var created struct {
		ID           string `json:"id"`
		ClientSecret string `json:"client_secret"`
		Bot          struct {
			ID    string `json:"id"`
			Token string `json:"token"`
		} `json:"bot"`
	}
	decodeJSONResponse(t, resp, &created)
	if created.ID == "" {
		t.Fatalf("application response missing id")
	}
	if created.ClientSecret == "" {
		t.Fatalf("application response missing client_secret")
	}
	if created.Bot.ID == "" {
		t.Fatalf("application response missing bot.id")
	}
	if created.Bot.Token == "" {
		t.Fatalf("application response missing bot.token")
	}
	storeClientSecret(created.ID, created.ClientSecret)
	return created.ID, created.Bot.ID, created.Bot.Token, created.ClientSecret
}
