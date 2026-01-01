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

// TestOAuth2ApplicationDeleteIdempotent validates that deleting a deleted application returns 404.
func TestOAuth2ApplicationDeleteIdempotent(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	name := fmt.Sprintf("Idempotent Test %d", time.Now().UnixNano())
	redirectURIs := []string{"https://example.com/callback"}
	scopes := []string{"identify"}

	var _ []string = scopes
	appID, _, _ := createOAuth2BotApplication(t, client, owner, name, redirectURIs)

	deleteOAuth2Application(t, client, owner, appID)

	sudoPayload := map[string]any{
		"password": owner.Password,
	}

	resp, err := client.deleteJSONWithAuth(fmt.Sprintf("/oauth2/applications/%s", appID), sudoPayload, owner.Token)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusNotFound {
		t.Fatalf("deleting already deleted application should return 404, got %d", resp.StatusCode)
	}
}
