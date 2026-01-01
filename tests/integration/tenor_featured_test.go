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

func TestTenorFeatured(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)

	t.Run("can get featured GIFs", func(t *testing.T) {
		featuredResp := getTenorFeatured(t, client, user.Token, "")

		if featuredResp == nil {
			t.Fatal("expected featured response, got nil")
		}

		t.Logf("Received featured response")
	})

	t.Run("can get featured with locale", func(t *testing.T) {
		featuredResp := getTenorFeatured(t, client, user.Token, "es-ES")

		if featuredResp == nil {
			t.Fatal("expected featured response with locale, got nil")
		}

		t.Logf("Received featured response for locale 'es-ES'")
	})

	t.Run("requires authentication", func(t *testing.T) {
		resp, err := client.get("/tenor/featured")
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusUnauthorized {
			t.Fatalf("expected 401, got %d", resp.StatusCode)
		}
	})
}
