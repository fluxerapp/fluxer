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

func TestTenorSearch(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)

	t.Run("can search for GIFs", func(t *testing.T) {
		searchResp := searchTenorGIFs(t, client, user.Token, "happy", "")

		if len(searchResp) == 0 {
			t.Fatal("expected search results, got none")
		}

		t.Logf("Found %d GIFs for query 'happy'", len(searchResp))

		firstGIF := searchResp[0]
		if firstGIF.ID == "" {
			t.Fatal("GIF ID should not be empty")
		}

		t.Logf("First GIF: ID=%s, Title=%s", firstGIF.ID, firstGIF.Title)
	})

	t.Run("can search with locale", func(t *testing.T) {
		searchResp := searchTenorGIFs(t, client, user.Token, "hello", "en-US")

		if len(searchResp) == 0 {
			t.Fatal("expected search results with locale, got none")
		}

		t.Logf("Found %d GIFs for query 'hello' with locale 'en-US'", len(searchResp))
	})

	t.Run("returns results for nonexistent query", func(t *testing.T) {
		searchResp := searchTenorGIFs(t, client, user.Token, "xyznonexistentquery123", "")

		t.Logf("Search for nonexistent query returned %d results", len(searchResp))
	})

	t.Run("requires authentication", func(t *testing.T) {
		resp, err := client.get("/tenor/search?q=happy")
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusUnauthorized {
			t.Fatalf("expected 401, got %d", resp.StatusCode)
		}
	})
}
