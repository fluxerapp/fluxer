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
	"testing"
)

// TestMultipleAssetUpdates_Consistency verifies consistency across multiple rapid updates
func TestMultipleAssetUpdates_Consistency(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	for i := range 3 {
		pngDataURL := "data:image/png;base64," + getValidPNGBase64()
		resp, err := client.patchJSONWithAuth("/users/@me", map[string]any{
			"avatar": pngDataURL,
		}, user.Token)
		if err != nil {
			t.Fatalf("failed to upload avatar iteration %d: %v", i, err)
		}

		var result struct {
			Avatar string `json:"avatar"`
		}
		decodeJSONResponse(t, resp, &result)
		resp.Body.Close()

		verify := verifyUserAvatarInS3(t, client, user.UserID)
		if verify.Hash == nil || *verify.Hash != result.Avatar {
			t.Errorf("iteration %d: hash mismatch between API response and DB", i)
		}
		if verify.ExistsInS3 == nil || !*verify.ExistsInS3 {
			t.Errorf("iteration %d: avatar not found in S3 after successful update", i)
		}
	}
}
