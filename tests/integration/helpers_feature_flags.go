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
	"strings"
	"testing"
	"time"
)

func featureFlagAdminToken(t testing.TB, client *testClient, extraACLs []string) string {
	t.Helper()

	admin := createTestAccount(t, client)
	acls := append([]string{"admin:authenticate"}, extraACLs...)
	setUserACLs(t, client, admin.UserID, acls)

	redirectURI := "https://example.com/callback"
	appID, _, _, _ := createOAuth2Application(
		t,
		client,
		admin,
		fmt.Sprintf("Feature Flags Admin %d", time.Now().UnixNano()),
		[]string{redirectURI},
		nil,
	)

	authCode, _ := authorizeOAuth2(t, client, admin.Token, appID, redirectURI, []string{"identify"}, "", "", "")
	token := exchangeOAuth2AuthorizationCode(t, client, appID, "", authCode, redirectURI, "").AccessToken
	return token
}

func updateFeatureFlagGuilds(t testing.TB, client *testClient, adminToken string, flag string, guildIDs []string) {
	t.Helper()

	payload := map[string]any{
		"flag":      flag,
		"guild_ids": strings.Join(guildIDs, ","),
	}

	resp, err := client.postJSONWithAuth("/admin/feature-flags/update", payload, adminToken)
	if err != nil {
		t.Fatalf("failed to update feature flag %s: %v", flag, err)
	}
	assertStatus(t, resp, http.StatusOK)
}
