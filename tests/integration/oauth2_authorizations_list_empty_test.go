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
	"net/http"
	"testing"
)

// TestOAuth2AuthorizationsListEmpty verifies that a user with no authorized
// apps receives an empty list.
func TestOAuth2AuthorizationsListEmpty(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)

	resp, err := client.getWithAuth("/oauth2/@me/authorizations", user.Token)
	if err != nil {
		t.Fatalf("failed to list authorizations: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("list authorizations failed with status %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var authorizations []oauth2AuthorizationResponse
	decodeJSONResponse(t, resp, &authorizations)

	if len(authorizations) != 0 {
		t.Fatalf("expected empty list, got %d authorizations", len(authorizations))
	}
}
