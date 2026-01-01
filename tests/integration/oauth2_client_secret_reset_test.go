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

// TestClientSecretReset verifies client secret rotation requires sudo and returns a new secret.
func TestClientSecretReset(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	appID, _, _, clientSecret := createOAuth2Application(
		t, client, owner,
		fmt.Sprintf("Client Secret Reset %d", time.Now().UnixNano()),
		[]string{"https://example.com/callback"},
		[]string{"identify"},
	)

	if clientSecret == "" {
		t.Fatal("creation should return client secret")
	}

	newSecret := resetClientSecret(t, client, owner, appID)
	if newSecret == clientSecret {
		t.Fatalf("reset should issue new secret")
	}

	code, _ := obtainAuthCode(t, client, appID, "https://example.com/callback", []string{"identify"})
	form := buildTokenForm(code, "https://example.com/callback")
	req := newFormRequest(t, client, "/oauth2/token", form)
	req.SetBasicAuth(appID, clientSecret)

	resp, err := client.httpClient.Do(req)
	if err != nil {
		t.Fatalf("token request failed: %v", err)
	}
	resp.Body.Close()
	if resp.StatusCode == http.StatusOK {
		t.Fatalf("old client secret should not work after rotation")
	}

	req2 := newFormRequest(t, client, "/oauth2/token", form)
	req2.SetBasicAuth(appID, newSecret)
	resp2, err := client.httpClient.Do(req2)
	if err != nil {
		t.Fatalf("token request failed: %v", err)
	}
	defer resp2.Body.Close()
	if resp2.StatusCode != http.StatusOK {
		t.Fatalf("new secret should work, got %d: %s", resp2.StatusCode, readResponseBody(resp2))
	}
}
