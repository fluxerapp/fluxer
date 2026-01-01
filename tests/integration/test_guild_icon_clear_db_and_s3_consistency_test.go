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

// TestGuildIconClear_DBAndS3Consistency verifies clearing a guild icon
func TestGuildIconClear_DBAndS3Consistency(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	guild := createGuild(t, client, user.Token, "Clear Icon Test Guild")
	guildID := guild.ID

	pngDataURL := "data:image/png;base64," + getValidPNGBase64()
	resp, err := client.patchJSONWithAuth(fmt.Sprintf("/guilds/%s", guildID), map[string]any{
		"icon": pngDataURL,
	}, user.Token)
	if err != nil {
		t.Fatalf("failed to upload guild icon: %v", err)
	}
	resp.Body.Close()
	assertStatus(t, resp, http.StatusOK)

	verify := verifyGuildIconInS3(t, client, guildID)
	if verify.ExistsInS3 == nil || !*verify.ExistsInS3 {
		t.Fatal("icon should exist in S3 before clearing")
	}

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%s", guildID), map[string]any{
		"icon": nil,
	}, user.Token)
	if err != nil {
		t.Fatalf("failed to clear guild icon: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusOK)

	var result struct {
		Icon *string `json:"icon"`
	}
	decodeJSONResponse(t, resp, &result)

	if result.Icon != nil && *result.Icon != "" {
		t.Errorf("expected icon to be null after clearing, got %v", result.Icon)
	}

	verify = verifyGuildIconInS3(t, client, guildID)
	if verify.Hash != nil && *verify.Hash != "" {
		t.Errorf("expected no icon hash in DB after clearing, got %s", *verify.Hash)
	}
}
