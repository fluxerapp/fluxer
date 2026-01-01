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

// TestOAuth2ApplicationUpdateValidation validates input validation during updates.
func TestOAuth2ApplicationUpdateValidation(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	name := fmt.Sprintf("Validation Test %d", time.Now().UnixNano())
	redirectURIs := []string{"https://example.com/callback"}
	appID, _, _ := createOAuth2BotApplication(t, client, owner, name, redirectURIs)

	tests := []struct {
		name           string
		updates        map[string]any
		expectedStatus int
		description    string
	}{
		{
			name: "invalid redirect URI scheme",
			updates: map[string]any{
				"redirect_uris": []string{"http://example.com/callback"},
			},
			expectedStatus: http.StatusOK,
			description:    "http URIs are allowed",
		},
		{
			name: "empty redirect URIs array",
			updates: map[string]any{
				"redirect_uris": []string{},
			},
			expectedStatus: http.StatusOK,
			description:    "redirect_uris may be empty",
		},
		{
			name: "localhost redirect URI allowed",
			updates: map[string]any{
				"redirect_uris": []string{"http://localhost:8080/callback"},
			},
			expectedStatus: http.StatusOK,
			description:    "localhost with http should be allowed",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			resp, err := client.patchJSONWithAuth(fmt.Sprintf("/oauth2/applications/%s", appID), tt.updates, owner.Token)
			if err != nil {
				t.Fatalf("request failed: %v", err)
			}
			defer resp.Body.Close()

			if resp.StatusCode != tt.expectedStatus {
				t.Fatalf("%s: expected status %d, got %d: %s", tt.description, tt.expectedStatus, resp.StatusCode, readResponseBody(resp))
			}
		})
	}
}
