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

// TestOAuth2ApplicationDeleteAccessControl validates that users can only delete their own applications.
func TestOAuth2ApplicationDeleteAccessControl(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	otherUser := createTestAccount(t, client)

	name := fmt.Sprintf("Access Control Delete %d", time.Now().UnixNano())
	redirectURIs := []string{"https://example.com/callback"}
	scopes := []string{"identify"}

	var _ []string = scopes
	appID, _, _ := createOAuth2BotApplication(t, client, owner, name, redirectURIs)

	sudoPayload := map[string]any{
		"password": otherUser.Password,
	}

	resp, err := client.deleteJSONWithAuth(fmt.Sprintf("/oauth2/applications/%s", appID), sudoPayload, otherUser.Token)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 Forbidden, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	app := getOAuth2Application(t, client, owner.Token, appID)
	if app.ID != appID {
		t.Fatalf("application should not have been deleted by unauthorized user")
	}
}
