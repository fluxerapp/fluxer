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

// TestInvalidIDFormats tests handling of malformed IDs
func TestInvalidIDFormats(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)

	testCases := []struct {
		name     string
		endpoint string
		method   string
	}{
		{"invalid guild ID letters", "/guilds/abcdef", "GET"},
		{"invalid guild ID negative", "/guilds/-123", "GET"},
		{"invalid guild ID overflow", "/guilds/99999999999999999999999999", "GET"},
		{"invalid channel ID", "/channels/invalid/messages", "GET"},
		{"invalid user ID", "/users/notanumber", "GET"},
		{"invalid message ID", "/channels/123/messages/xyz", "GET"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			var resp *http.Response
			var err error

			switch tc.method {
			case "GET":
				resp, err = client.getWithAuth(tc.endpoint, user.Token)
			case "POST":
				resp, err = client.postJSONWithAuth(tc.endpoint, nil, user.Token)
			}

			if err != nil {
				t.Fatalf("request failed: %v", err)
			}
			defer resp.Body.Close()

			if resp.StatusCode != http.StatusBadRequest && resp.StatusCode != http.StatusNotFound {
				t.Fatalf("expected 400 or 404 for invalid ID, got %d (endpoint: %s)", resp.StatusCode, tc.endpoint)
			}
		})
	}
}
