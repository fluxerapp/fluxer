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

// TestOAuth2AuthorizationsDeauthorizeNonexistent verifies that deauthorizing
// a non-existent application returns an appropriate error.
func TestOAuth2AuthorizationsDeauthorizeNonexistent(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)

	resp, err := client.delete("/oauth2/@me/authorizations/123456789012345678", user.Token)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}

	if resp.StatusCode == http.StatusNoContent || resp.StatusCode == http.StatusOK {
		t.Fatal("expected error for non-existent application")
	}
}
