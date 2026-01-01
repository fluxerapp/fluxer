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

func TestAuthUsernameSuggestions(t *testing.T) {
	client := newTestClient(t)
	resp, err := client.postJSON("/auth/username-suggestions", map[string]string{"global_name": "Integration Tester"})
	if err != nil {
		t.Fatalf("failed to call username suggestions: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var suggestions usernameSuggestionsResponse
	decodeJSONResponse(t, resp, &suggestions)
	if len(suggestions.Suggestions) == 0 {
		t.Fatalf("expected username suggestions to be returned")
	}
}

type usernameSuggestionsResponse struct {
	Suggestions []string `json:"suggestions"`
}
