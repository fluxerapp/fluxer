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

func TestRpcSessionIncludesFeatureFlags(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	guild := createGuild(t, client, owner.Token, "rpc-feature-flags")

	adminToken := featureFlagAdminToken(t, client, []string{"feature_flag:manage"})
	updateFeatureFlagGuilds(t, client, adminToken, "message_scheduling", []string{guild.ID})

	payload := map[string]any{
		"type":    "session",
		"token":   owner.Token,
		"version": 1,
	}

	resp, err := client.requestJSON(http.MethodPost, "/_rpc", payload, "Bearer test-rpc-secret")
	if err != nil {
		t.Fatalf("RPC request failed: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusOK)

	var body struct {
		Type string `json:"type"`
		Data struct {
			FeatureFlags map[string][]string `json:"feature_flags"`
		} `json:"data"`
	}
	decodeJSONResponse(t, resp, &body)

	if body.Type != "session" {
		t.Fatalf("expected session response, got %s", body.Type)
	}

	ids := body.Data.FeatureFlags["message_scheduling"]
	if len(ids) == 0 || ids[0] != guild.ID {
		t.Fatalf("expected guild %s in RPC feature flags, got %+v", guild.ID, ids)
	}
}
