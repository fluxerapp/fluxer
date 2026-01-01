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

// TestOAuth2TokenExpirationConsistency verifies that expires_in values
// are consistent across different token operations.
func TestOAuth2TokenExpirationConsistency(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/expire/consistency"
	appID, _, _, clientSecret := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Consistency %d", time.Now().UnixNano()),
		[]string{redirectURI},
		[]string{"identify"},
	)

	expiresInValues := []int{}

	for i := 0; i < 3; i++ {
		authCode, _ := authorizeOAuth2(
			t,
			client,
			endUser.Token,
			appID,
			redirectURI,
			[]string{"identify"},
			fmt.Sprintf("state-%d", i),
			"",
			"",
		)
		tokens := exchangeOAuth2AuthorizationCode(
			t, client,
			appID,
			clientSecret,
			authCode,
			redirectURI,
			"",
		)

		expiresInValues = append(expiresInValues, tokens.ExpiresIn)

		if i < 2 {
			time.Sleep(100 * time.Millisecond)
		}
	}

	firstValue := expiresInValues[0]
	for i, val := range expiresInValues {
		if abs(val-firstValue) > 5 {
			t.Errorf("expires_in[%d]=%d differs significantly from first value %d", i, val, firstValue)
		}
	}
}
