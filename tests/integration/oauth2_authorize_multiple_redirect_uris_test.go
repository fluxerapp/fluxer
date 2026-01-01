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

// TestOAuth2AuthorizeMultipleRedirectURIs verifies that an application
// can have multiple registered redirect URIs and use any of them.
func TestOAuth2AuthorizeMultipleRedirectURIs(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	uri1 := "https://app.example.com/callback"
	uri2 := "https://staging.example.com/callback"
	uri3 := "https://localhost:3000/callback"

	appID, _, _, clientSecret := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Multiple URIs %d", time.Now().UnixNano()),
		[]string{uri1, uri2, uri3},
		[]string{"identify"},
	)

	uris := []string{uri1, uri2, uri3}
	for i, uri := range uris {
		t.Run(fmt.Sprintf("URI %d", i+1), func(t *testing.T) {
			authCode, _ := authorizeOAuth2(
				t,
				client,
				endUser.Token,
				appID,
				uri,
				[]string{"identify"},
				fmt.Sprintf("state-%d", i),
				"",
				"",
			)

			tokenResp := exchangeOAuth2AuthorizationCode(
				t, client,
				appID,
				clientSecret,
				authCode,
				uri,
				"",
			)
			if tokenResp.AccessToken == "" {
				t.Fatalf("authorization should work with registered URI %s", uri)
			}
		})
	}
}
