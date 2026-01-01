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

// TestGuildIconUpload_DBAndS3Consistency verifies guild icon upload consistency
func TestGuildIconUpload_DBAndS3Consistency(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	guild := createGuild(t, client, user.Token, "Icon Test Guild")
	guildID := guild.ID

	verify := verifyGuildIconInS3(t, client, guildID)

	pngDataURL := "data:image/png;base64," + getValidPNGBase64()
	resp, err := client.patchJSONWithAuth(fmt.Sprintf("/guilds/%s", guildID), map[string]any{
		"icon": pngDataURL,
	}, user.Token)
	if err != nil {
		t.Fatalf("failed to upload guild icon: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusOK)

	var result struct {
		Icon string `json:"icon"`
	}
	decodeJSONResponse(t, resp, &result)

	if result.Icon == "" {
		t.Fatal("expected icon hash to be set after upload")
	}

	verify = verifyGuildIconInS3(t, client, guildID)
	if verify.Hash == nil {
		t.Fatal("expected hash to be set in verification response")
	}
	if *verify.Hash != result.Icon {
		t.Errorf("hash mismatch: API returned %s, verification returned %s", result.Icon, *verify.Hash)
	}
	if verify.ExistsInS3 == nil || !*verify.ExistsInS3 {
		t.Error("icon hash exists in DB but asset NOT found in S3 - data inconsistency!")
	}
}
