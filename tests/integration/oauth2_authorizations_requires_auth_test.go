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

// TestOAuth2AuthorizationsRequiresAuth verifies that the authorizations
// endpoints require authentication.
func TestOAuth2AuthorizationsRequiresAuth(t *testing.T) {
	client := newTestClient(t)

	req, _ := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/oauth2/@me/authorizations", client.baseURL), nil)
	client.applyCommonHeaders(req)
	resp, err := client.httpClient.Do(req)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	if resp.Body != nil {
		resp.Body.Close()
	}

	if resp.StatusCode != http.StatusUnauthorized {
		t.Fatalf("expected 401 for unauthenticated request, got %d", resp.StatusCode)
	}
}
