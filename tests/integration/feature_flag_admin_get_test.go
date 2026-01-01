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

func TestFeatureFlagAdminGet(t *testing.T) {
	client := newTestClient(t)

	t.Run("success", func(t *testing.T) {
		token := featureFlagAdminToken(t, client, []string{"feature_flag:view"})
		resp, err := client.postJSONWithAuth("/admin/feature-flags/get", map[string]any{}, token)
		if err != nil {
			t.Fatalf("request failed: %v", err)
		}
		defer resp.Body.Close()
		assertStatus(t, resp, http.StatusOK)

		var body struct {
			FeatureFlags map[string][]string `json:"feature_flags"`
		}
		decodeJSONResponse(t, resp, &body)
		if body.FeatureFlags == nil {
			t.Fatalf("expected feature_flags in response")
		}
	})

	t.Run("unauthorized", func(t *testing.T) {
		resp, err := client.postJSON("/admin/feature-flags/get", map[string]any{})
		if err != nil {
			t.Fatalf("request failed: %v", err)
		}
		defer resp.Body.Close()
		assertStatus(t, resp, http.StatusUnauthorized)
	})

	t.Run("missing-acl", func(t *testing.T) {
		token := featureFlagAdminToken(t, client, nil)
		resp, err := client.postJSONWithAuth("/admin/feature-flags/get", map[string]any{}, token)
		if err != nil {
			t.Fatalf("request failed: %v", err)
		}
		defer resp.Body.Close()
		assertStatus(t, resp, http.StatusForbidden)
	})
}
