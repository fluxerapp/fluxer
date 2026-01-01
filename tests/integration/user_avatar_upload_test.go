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

// TestUserAvatarUpload_DBAndS3Consistency verifies that when a user uploads an avatar,
// both the database hash and S3 object are consistent
func TestUserAvatarUpload_DBAndS3Consistency(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	verify := verifyUserAvatarInS3(t, client, user.UserID)
	if verify.Hash != nil {
		t.Errorf("expected no avatar initially, got hash: %s", *verify.Hash)
	}

	pngDataURL := "data:image/png;base64," + getValidPNGBase64()
	resp, err := client.patchJSONWithAuth("/users/@me", map[string]any{
		"avatar": pngDataURL,
	}, user.Token)
	if err != nil {
		t.Fatalf("failed to upload avatar: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusOK)

	var result struct {
		Avatar string `json:"avatar"`
	}
	decodeJSONResponse(t, resp, &result)

	if result.Avatar == "" {
		t.Fatal("expected avatar hash to be set after upload")
	}

	verify = verifyUserAvatarInS3(t, client, user.UserID)
	if verify.Hash == nil {
		t.Fatal("expected hash to be set in verification response")
	}
	if *verify.Hash != result.Avatar {
		t.Errorf("hash mismatch: API returned %s, verification returned %s", result.Avatar, *verify.Hash)
	}
	if verify.ExistsInS3 == nil || !*verify.ExistsInS3 {
		t.Error("avatar hash exists in DB but asset NOT found in S3 - data inconsistency!")
	}
}
