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

// getTenorFeatured gets featured GIFs from Tenor
// The actual response structure depends on the Tenor API, returning as-is
func getTenorFeatured(t testing.TB, client *testClient, token, locale string) map[string]any {
	t.Helper()

	url := "/tenor/featured"
	if locale != "" {
		url += fmt.Sprintf("?locale=%s", locale)
	}

	resp, err := client.getWithAuth(url, token)
	if err != nil {
		t.Fatalf("failed to get tenor featured: %v", err)
	}
	defer resp.Body.Close()

	assertStatus(t, resp, http.StatusOK)

	var featured map[string]any
	decodeJSONResponse(t, resp, &featured)

	return featured
}
