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
	"testing"
	"time"
)

// TestOAuth2AuthorizeStateWithSpecialCharacters verifies that state
// parameters with special characters are preserved correctly.
func TestOAuth2AuthorizeStateWithSpecialCharacters(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/state/special"
	appID, _, _, _ := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Special State %d", time.Now().UnixNano()),
		[]string{redirectURI},
		[]string{"identify"},
	)

	testCases := []struct {
		name  string
		state string
	}{
		{"with dashes", "state-with-dashes-123"},
		{"with underscores", "state_with_underscores_456"},
		{"with periods", "state.with.periods.789"},
		{"with equals", "state=with=equals"},
		{"with encoded chars", "state%20with%20spaces"},
		{"base64-like", "c3RhdGUtYmFzZTY0LWxpa2U="},
		{"long state", "very-long-state-" + fmt.Sprintf("%d", time.Now().UnixNano()) + "-with-many-characters"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			_, returnedState := authorizeOAuth2(
				t,
				client,
				endUser.Token,
				appID,
				redirectURI,
				[]string{"identify"},
				tc.state,
				"",
				"",
			)

			if returnedState != tc.state {
				t.Fatalf("state not preserved: expected %q, got %q", tc.state, returnedState)
			}
		})
	}
}
