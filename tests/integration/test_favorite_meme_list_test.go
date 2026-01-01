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

func TestFavoriteMeme_List(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	t.Run("can list memes for authenticated user", func(t *testing.T) {
		resp, err := client.getWithAuth("/users/@me/memes", user.Token)
		if err != nil {
			t.Fatalf("failed to list memes: %v", err)
		}
		defer resp.Body.Close()
		assertStatus(t, resp, http.StatusOK)

		var memes []favoriteMemeResponse
		decodeJSONResponse(t, resp, &memes)

		t.Logf("User has %d memes", len(memes))
	})

	t.Run("requires authentication", func(t *testing.T) {
		resp, err := client.get("/users/@me/memes")
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusUnauthorized {
			t.Errorf("expected 401, got %d", resp.StatusCode)
		}
	})
}
