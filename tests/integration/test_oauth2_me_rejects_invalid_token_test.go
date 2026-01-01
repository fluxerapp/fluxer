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

// TestOAuth2MeRejectsInvalidToken ensures /oauth2/@me requires a valid bearer token.
func TestOAuth2MeRejectsInvalidToken(t *testing.T) {
	t.Parallel()

	client := newTestClient(t)

	req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/oauth2/@me", client.baseURL), nil)
	if err != nil {
		t.Fatalf("failed to build /oauth2/@me request: %v", err)
	}
	req.Header.Set("Authorization", "Bearer invalid-token-123")
	client.applyCommonHeaders(req)

	resp, err := client.httpClient.Do(req)
	if err != nil {
		t.Fatalf("oauth2/@me request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode == http.StatusOK {
		t.Fatalf("expected /oauth2/@me to reject invalid tokens")
	}
	if resp.StatusCode != http.StatusUnauthorized {
		t.Fatalf("expected 401 Unauthorized for invalid token, got %d", resp.StatusCode)
	}
}
