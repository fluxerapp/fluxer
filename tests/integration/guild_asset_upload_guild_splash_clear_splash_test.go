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

func TestGuildSplash_ClearSplash(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	guild := createGuild(t, client, user.Token, "Splash Test Guild")
	guildID := guild.ID

	grantGuildInviteSplashFeature(t, client, guildID)

	payload := map[string]any{
		"splash": "data:image/png;base64," + getValidPNGBase64(),
	}
	resp, err := client.patchJSONWithAuth(fmt.Sprintf("/guilds/%s", guildID), payload, user.Token)
	if err != nil {
		t.Fatalf("failed to set splash: %v", err)
	}
	resp.Body.Close()

	payload = map[string]any{
		"splash": nil,
	}
	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%s", guildID), payload, user.Token)
	if err != nil {
		t.Fatalf("failed to clear splash: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusOK)

	var result struct {
		Splash *string `json:"splash"`
	}
	decodeJSONResponse(t, resp, &result)

	if result.Splash != nil {
		t.Error("expected guild splash to be cleared")
	}
}
