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

func TestTenorRegisterShare(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)

	t.Run("can register GIF share", func(t *testing.T) {
		searchResp := searchTenorGIFs(t, client, user.Token, "happy", "")

		if len(searchResp) == 0 {
			t.Skip("no GIFs found to test share registration")
		}

		gifID := searchResp[0].ID
		query := "happy"

		registerTenorShare(t, client, user.Token, gifID, &query, "")

		t.Logf("Successfully registered share for GIF ID: %s", gifID)
	})

	t.Run("requires authentication", func(t *testing.T) {
		query := "test"
		req := registerTenorShareRequest{
			ID:     "test_id",
			Q:      &query,
			Locale: "",
		}

		resp, err := client.postJSON("/tenor/register-share", req)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusUnauthorized {
			t.Fatalf("expected 401, got %d", resp.StatusCode)
		}
	})
}
