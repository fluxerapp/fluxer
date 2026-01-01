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

func TestExpressionPackFeatureFlagGating(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	ensureSessionStarted(t, client, owner.Token)
	grantPremium(t, client, owner.UserID, PremiumTypeSubscription)
	guild := createGuild(t, client, owner.Token, "packs-flag")

	adminToken := featureFlagAdminToken(t, client, []string{"feature_flag:manage", "user:update:flags"})

	resp, err := client.postJSONWithAuth("/packs/emoji", map[string]any{"name": "feature-flag-pack"}, owner.Token)
	if err != nil {
		t.Fatalf("failed to call create pack endpoint: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusForbidden)

	updateFeatureFlagGuilds(t, client, adminToken, "expression_packs", []string{guild.ID})

	resp, err = client.postJSONWithAuth("/packs/emoji", map[string]any{"name": "feature-flag-pack"}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create pack after enabling flag: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusOK)
}
