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

// TestOAuth2ApplicationUpdateAccessControl validates that users can only update their own applications.
func TestOAuth2ApplicationUpdateAccessControl(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	otherUser := createTestAccount(t, client)

	name := fmt.Sprintf("Access Control Test %d", time.Now().UnixNano())
	redirectURIs := []string{"https://example.com/callback"}

	appID, _, _ := createOAuth2BotApplication(t, client, owner, name, redirectURIs)

	updates := map[string]any{
		"name": "Hacked Name",
	}

	resp, err := client.patchJSONWithAuth(fmt.Sprintf("/oauth2/applications/%s", appID), updates, otherUser.Token)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 Forbidden, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	app := getOAuth2Application(t, client, owner.Token, appID)
	if app.Name != name {
		t.Fatalf("application should not have been modified by unauthorized user")
	}
}
