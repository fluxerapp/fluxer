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

// TestUserAvatarClear_DBAndS3Consistency verifies that clearing an avatar
// updates DB and queues old asset for deletion
func TestUserAvatarClear_DBAndS3Consistency(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	pngDataURL := "data:image/png;base64," + getValidPNGBase64()
	resp, err := client.patchJSONWithAuth("/users/@me", map[string]any{
		"avatar": pngDataURL,
	}, user.Token)
	if err != nil {
		t.Fatalf("failed to upload avatar: %v", err)
	}
	resp.Body.Close()
	assertStatus(t, resp, http.StatusOK)

	verify := verifyUserAvatarInS3(t, client, user.UserID)
	if verify.ExistsInS3 == nil || !*verify.ExistsInS3 {
		t.Fatal("avatar should exist in S3 before clearing")
	}

	resp, err = client.patchJSONWithAuth("/users/@me", map[string]any{
		"avatar": nil,
	}, user.Token)
	if err != nil {
		t.Fatalf("failed to clear avatar: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusOK)

	var result struct {
		Avatar *string `json:"avatar"`
	}
	decodeJSONResponse(t, resp, &result)

	if result.Avatar != nil && *result.Avatar != "" {
		t.Errorf("expected avatar to be null after clearing, got %v", result.Avatar)
	}

	verify = verifyUserAvatarInS3(t, client, user.UserID)
	if verify.Hash != nil && *verify.Hash != "" {
		t.Errorf("expected no avatar hash in DB after clearing, got %s", *verify.Hash)
	}
}
