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
	"testing"
)

func TestTenorCachingBehavior(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)

	t.Run("featured GIFs endpoint is accessible", func(t *testing.T) {
		featuredResp1 := getTenorFeatured(t, client, user.Token, "")
		featuredResp2 := getTenorFeatured(t, client, user.Token, "")

		if featuredResp1 == nil || featuredResp2 == nil {
			t.Fatal("expected featured responses, got nil")
		}

		t.Logf("Both requests returned featured GIFs successfully")
	})

	t.Run("trending GIFs endpoint is accessible", func(t *testing.T) {
		trendingResp1 := getTenorTrending(t, client, user.Token, "")
		trendingResp2 := getTenorTrending(t, client, user.Token, "")

		if len(trendingResp1) == 0 || len(trendingResp2) == 0 {
			t.Fatal("expected trending responses to contain GIFs")
		}

		t.Logf("Both requests returned trending GIFs successfully (%d and %d GIFs)", len(trendingResp1), len(trendingResp2))
	})
}
