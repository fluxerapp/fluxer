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

// getTenorSuggestions gets autocomplete suggestions from Tenor
// Returns an array of suggestion strings
func getTenorSuggestions(t testing.TB, client *testClient, token, query, locale string) []string {
	t.Helper()

	url := fmt.Sprintf("/tenor/suggest?q=%s", query)
	if locale != "" {
		url += fmt.Sprintf("&locale=%s", locale)
	}

	resp, err := client.getWithAuth(url, token)
	if err != nil {
		t.Fatalf("failed to get tenor suggestions: %v", err)
	}
	defer resp.Body.Close()

	assertStatus(t, resp, http.StatusOK)

	var suggestions []string
	decodeJSONResponse(t, resp, &suggestions)

	return suggestions
}
