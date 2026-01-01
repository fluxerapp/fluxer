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

func TestEmoji_BioSanitization_NoPermissionCheck(t *testing.T) {

	client := newTestClient(t)
	premiumUser := createTestAccount(t, client)
	ensureSessionStarted(t, client, premiumUser.Token)
	grantPremium(t, client, premiumUser.UserID, PremiumTypeSubscription)

	t.Run("no permission check for bio emojis", func(t *testing.T) {

		payload := map[string]any{
			"bio": "No permission needed <:test:123456789012345678>",
		}

		resp, err := client.patchJSONWithAuth("/users/@me", payload, premiumUser.Token)
		if err != nil {
			t.Fatalf("failed to update profile: %v", err)
		}
		defer resp.Body.Close()
		assertStatus(t, resp, http.StatusOK)

		t.Log("Bio emoji update succeeded - no permission check required")
	})
}
