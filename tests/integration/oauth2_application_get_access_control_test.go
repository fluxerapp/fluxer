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

// TestOAuth2ApplicationGetAccessControl validates that users can only access their own applications.
func TestOAuth2ApplicationGetAccessControl(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	otherUser := createTestAccount(t, client)

	name := fmt.Sprintf("Access Control App %d", time.Now().UnixNano())
	redirectURIs := []string{"https://example.com/callback"}
	scopes := []string{"identify"}

	appID, _, _, _ := createOAuth2Application(t, client, owner, name, redirectURIs, scopes)

	resp, err := client.getWithAuth(fmt.Sprintf("/oauth2/applications/%s", appID), otherUser.Token)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 Forbidden when accessing another user's application, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}
}
