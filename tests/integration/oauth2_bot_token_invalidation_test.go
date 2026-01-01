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

// TestBotTokenInvalidation verifies that invalid or malformed bot tokens are rejected.
func TestBotTokenInvalidation(t *testing.T) {
	client := newTestClient(t)

	testCases := []struct {
		name  string
		token string
	}{
		{"empty token", ""},
		{"invalid format", "not-a-real-token"},
		{"random string", "MTIzNDU2Nzg5MDEyMzQ1Njc4OTAuR0ZVa2Ry.dQw4w9WgXcQ"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me", client.baseURL), nil)
			if err != nil {
				t.Fatalf("failed to build request: %v", err)
			}
			req.Header.Set("Authorization", fmt.Sprintf("Bot %s", tc.token))
			client.applyCommonHeaders(req)

			resp, err := client.httpClient.Do(req)
			if err != nil {
				t.Fatalf("request failed: %v", err)
			}
			defer resp.Body.Close()

			if resp.StatusCode != http.StatusUnauthorized {
				t.Fatalf("expected 401 for invalid token %q, got %d", tc.name, resp.StatusCode)
			}
		})
	}

	t.Logf("All invalid bot tokens correctly rejected")
}
