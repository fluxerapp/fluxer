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

// TestOAuth2TokenRevokeInvalidToken verifies that revoking an invalid
// token succeeds (idempotent operation per RFC 7009).
func TestOAuth2TokenRevokeInvalidToken(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)

	redirectURI := "https://example.com/revoke/invalid"
	appID, _, _, clientSecret := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Revoke Invalid %d", time.Now().UnixNano()),
		[]string{redirectURI},
		[]string{"identify"},
	)

	revokeOAuth2Token(t, client, appID, clientSecret, "invalid-token-12345", "access_token")

}
