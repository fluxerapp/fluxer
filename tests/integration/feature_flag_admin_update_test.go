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

func TestFeatureFlagAdminUpdate(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	guild := createGuild(t, client, owner.Token, "feature-flag-update")

	t.Run("success", func(t *testing.T) {
		adminToken := featureFlagAdminToken(t, client, []string{"feature_flag:manage"})
		payload := map[string]any{
			"flag":      "message_scheduling",
			"guild_ids": guild.ID,
		}
		resp, err := client.postJSONWithAuth("/admin/feature-flags/update", payload, adminToken)
		if err != nil {
			t.Fatalf("request failed: %v", err)
		}
		defer resp.Body.Close()
		assertStatus(t, resp, http.StatusOK)

		var body struct {
			FeatureFlags map[string][]string `json:"feature_flags"`
		}
		decodeJSONResponse(t, resp, &body)
		ids := body.FeatureFlags["message_scheduling"]
		if len(ids) == 0 || ids[0] != guild.ID {
			t.Fatalf("expected guild %s in response, got %+v", guild.ID, ids)
		}
	})

	t.Run("unauthorized", func(t *testing.T) {
		resp, err := client.postJSON("/admin/feature-flags/update", map[string]any{"flag": "expression_packs", "guild_ids": ""})
		if err != nil {
			t.Fatalf("request failed: %v", err)
		}
		defer resp.Body.Close()
		assertStatus(t, resp, http.StatusUnauthorized)
	})

	t.Run("missing-acl", func(t *testing.T) {
		token := featureFlagAdminToken(t, client, nil)
		resp, err := client.postJSONWithAuth("/admin/feature-flags/update", map[string]any{"flag": "expression_packs", "guild_ids": guild.ID}, token)
		if err != nil {
			t.Fatalf("request failed: %v", err)
		}
		defer resp.Body.Close()
		assertStatus(t, resp, http.StatusForbidden)
	})
}
