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

const (
	incomingCallFriendsOnly = 8
)

func TestRegisterIncomingCallSettingsDefault(t *testing.T) {
	client := newTestClient(t)

	testCases := []struct {
		name     string
		opts     []registerOption
		expected int
	}{
		{
			name:     "adult defaults to friends only",
			expected: incomingCallFriendsOnly,
		},
		{
			name:     "minor defaults to friends only",
			opts:     []registerOption{withDateOfBirth(minorDateOfBirth())},
			expected: incomingCallFriendsOnly,
		},
	}

	for _, tc := range testCases {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			account := createTestAccount(t, client, tc.opts...)

			resp, err := client.getWithAuth("/users/@me/settings", account.Token)
			if err != nil {
				t.Fatalf("failed to fetch settings: %v", err)
			}
			assertStatus(t, resp, http.StatusOK)

			var payload struct {
				IncomingCallFlags int `json:"incoming_call_flags"`
			}
			decodeJSONResponse(t, resp, &payload)

			if payload.IncomingCallFlags != tc.expected {
				t.Fatalf("unexpected incoming_call_flags: got %d want %d", payload.IncomingCallFlags, tc.expected)
			}
		})
	}
}
