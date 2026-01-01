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

func TestTenorSuggest(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)

	t.Run("can get autocomplete suggestions", func(t *testing.T) {
		suggestResp := getTenorSuggestions(t, client, user.Token, "hap", "")

		if len(suggestResp) == 0 {
			t.Fatal("expected autocomplete suggestions, got none")
		}

		t.Logf("Received %d suggestions for query 'hap'", len(suggestResp))
	})

	t.Run("can get suggestions with locale", func(t *testing.T) {
		suggestResp := getTenorSuggestions(t, client, user.Token, "hel", "de")

		if len(suggestResp) == 0 {
			t.Fatal("expected autocomplete suggestions with locale, got none")
		}

		t.Logf("Received %d suggestions for query 'hel' with locale 'de'", len(suggestResp))
	})

	t.Run("requires authentication", func(t *testing.T) {
		resp, err := client.get("/tenor/suggest?q=hap")
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusUnauthorized {
			t.Fatalf("expected 401, got %d", resp.StatusCode)
		}
	})
}
