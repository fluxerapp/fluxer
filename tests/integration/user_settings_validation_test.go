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

// TestUserSettingsValidation checks type validation and value bounds on the settings endpoint
func TestUserSettingsValidation(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)

	testCases := []struct {
		name    string
		payload any
	}{
		{name: "boolean fields must be booleans", payload: map[string]any{"inline_attachment_media": "true"}},
		{name: "status must be known string", payload: map[string]any{"status": 42}},
		{name: "null theme not allowed", payload: map[string]any{"theme": nil}},
		{name: "mixed invalid shape", payload: map[string]any{"status": "offline", "gif_auto_play": "nope"}},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			resp, err := client.patchJSONWithAuth("/users/@me/settings", tc.payload, user.Token)
			if err != nil {
				t.Fatalf("failed to call settings endpoint: %v", err)
			}
			if resp.StatusCode == http.StatusOK {
				t.Fatalf("expected validation failure for %s, got 200", tc.name)
			}
			resp.Body.Close()
		})
	}

	validPayload := map[string]any{
		"status":                  "online",
		"inline_attachment_media": true,
		"gif_auto_play":           false,
	}
	resp, err := client.patchJSONWithAuth("/users/@me/settings", validPayload, user.Token)
	if err != nil {
		t.Fatalf("failed to apply valid settings update: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()
}
