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

// Meme limit constants
const (
	MaxFavoriteMemesNonPremium = 50
	MaxFavoriteMemesPremium    = 500
)

func TestFavoriteMeme_LimitEnforcement(t *testing.T) {

	t.Run("enforces limit at boundary", func(t *testing.T) {
		client := newTestClient(t)
		user := createTestAccount(t, client)
		ensureSessionStarted(t, client, user.Token)

		for i := 0; i < MaxFavoriteMemesNonPremium; i++ {
			payload := map[string]any{
				"url":  fmt.Sprintf("https://picsum.photos/id/%d/100", i),
				"name": fmt.Sprintf("Meme %d", i),
			}

			resp, err := client.postJSONWithAuth("/users/@me/memes", payload, user.Token)
			if err != nil {
				t.Fatalf("failed to create meme %d: %v", i, err)
			}
			resp.Body.Close()

			if resp.StatusCode != http.StatusOK && resp.StatusCode != http.StatusCreated {
				t.Fatalf("meme %d creation failed with status %d", i, resp.StatusCode)
			}
		}

		payload := map[string]any{
			"url":  favoriteMemeTestImageURL,
			"name": "Over Limit Meme",
		}

		resp, err := client.postJSONWithAuth("/users/@me/memes", payload, user.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusCreated {
			t.Error("expected meme creation to fail when at limit")
		}

		// Verify error response
		var errResp struct {
			Code             string `json:"code"`
			Message          string `json:"message"`
			MaxFavoriteMemes int    `json:"max_favorite_memes"`
			IsPremium        bool   `json:"is_premium"`
		}
		decodeJSONResponse(t, resp, &errResp)

		if errResp.Code != "MAX_FAVORITE_MEMES" {
			t.Errorf("expected error code MAX_FAVORITE_MEMES, got %s", errResp.Code)
		}

		if errResp.MaxFavoriteMemes != MaxFavoriteMemesNonPremium {
			t.Errorf("expected max_favorite_memes %d, got %d", MaxFavoriteMemesNonPremium, errResp.MaxFavoriteMemes)
		}

		if errResp.IsPremium {
			t.Error("expected is_premium to be false")
		}
	})
}
