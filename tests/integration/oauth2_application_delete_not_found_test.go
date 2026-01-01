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
)

// TestOAuth2ApplicationDeleteNotFound validates error handling for non-existent applications.
func TestOAuth2ApplicationDeleteNotFound(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	fakeAppID := "999999999999999999"
	sudoPayload := map[string]any{
		"password": owner.Password,
	}

	resp, err := client.deleteJSONWithAuth(fmt.Sprintf("/oauth2/applications/%s", fakeAppID), sudoPayload, owner.Token)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusNotFound {
		t.Fatalf("expected 404 Not Found, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}
}
