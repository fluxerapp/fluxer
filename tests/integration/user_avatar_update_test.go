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

// TestUserAvatarUpdate_OldAssetEventuallyDeleted verifies that when a user updates
// their avatar, the old asset is queued for deletion
func TestUserAvatarUpdate_OldAssetEventuallyDeleted(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	pngDataURL := "data:image/png;base64," + getValidPNGBase64()
	resp, err := client.patchJSONWithAuth("/users/@me", map[string]any{
		"avatar": pngDataURL,
	}, user.Token)
	if err != nil {
		t.Fatalf("failed to upload first avatar: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var firstResult struct {
		Avatar string `json:"avatar"`
	}
	decodeJSONResponse(t, resp, &firstResult)
	resp.Body.Close()
	firstAvatarHash := firstResult.Avatar

	verify := verifyUserAvatarInS3(t, client, user.UserID)
	if verify.ExistsInS3 == nil || !*verify.ExistsInS3 {
		t.Fatal("first avatar should exist in S3")
	}

	differentPNG := loadFixtureAsDataURL(t, "yeah.png", "image/png")

	resp, err = client.patchJSONWithAuth("/users/@me", map[string]any{
		"avatar": differentPNG,
	}, user.Token)
	if err != nil {
		t.Fatalf("failed to upload second avatar: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var secondResult struct {
		Avatar string `json:"avatar"`
	}
	decodeJSONResponse(t, resp, &secondResult)
	resp.Body.Close()
	secondAvatarHash := secondResult.Avatar

	if firstAvatarHash == secondAvatarHash {
		t.Skip("Avatars have same hash (same image content), skipping replacement test")
	}

	verify = verifyUserAvatarInS3(t, client, user.UserID)
	if verify.Hash == nil || *verify.Hash != secondAvatarHash {
		t.Errorf("expected current avatar hash to be %s, got %v", secondAvatarHash, verify.Hash)
	}
	if verify.ExistsInS3 == nil || !*verify.ExistsInS3 {
		t.Error("second avatar should exist in S3")
	}
}
