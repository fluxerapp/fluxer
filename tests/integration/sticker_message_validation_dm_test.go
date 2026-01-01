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

func TestSticker_MessageValidation_DM(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	user2 := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	createFriendship(t, client, user, user2)
	dmChannel := createDmChannel(t, client, user.Token, parseSnowflake(t, user2.UserID))
	dmChannelID := parseSnowflake(t, dmChannel.ID)

	t.Run("non-premium user cannot use sticker in DM", func(t *testing.T) {
		payload := map[string]any{
			"content":     "Hello",
			"sticker_ids": []string{"999999999999999999"},
		}

		resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages", dmChannelID), payload, user.Token)
		if err != nil {
			t.Fatalf("failed to send message: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK {
			t.Fatal("expected sticker message to fail for non-premium user in DM")
		}
		if resp.StatusCode != http.StatusBadRequest && resp.StatusCode != http.StatusForbidden {
			t.Errorf("expected 400 or 403 for sticker in DM without premium, got %d", resp.StatusCode)
		}
	})

	t.Run("non-existent sticker returns error", func(t *testing.T) {
		premiumUser := createTestAccount(t, client)
		ensureSessionStarted(t, client, premiumUser.Token)
		grantPremium(t, client, premiumUser.UserID, PremiumTypeSubscription)

		createFriendship(t, client, premiumUser, user2)
		premiumDM := createDmChannel(t, client, premiumUser.Token, parseSnowflake(t, user2.UserID))
		premiumDMID := parseSnowflake(t, premiumDM.ID)

		payload := map[string]any{
			"content":     "Premium sticker test",
			"sticker_ids": []string{"999999999999999998"},
		}

		resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages", premiumDMID), payload, premiumUser.Token)
		if err != nil {
			t.Fatalf("failed to send message: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK {
			t.Error("expected non-existent sticker to fail")
		}
	})

	t.Run("multiple invalid stickers all cause failure", func(t *testing.T) {
		premiumUser := createTestAccount(t, client)
		ensureSessionStarted(t, client, premiumUser.Token)
		grantPremium(t, client, premiumUser.UserID, PremiumTypeSubscription)

		createFriendship(t, client, premiumUser, user2)
		premiumDM := createDmChannel(t, client, premiumUser.Token, parseSnowflake(t, user2.UserID))
		premiumDMID := parseSnowflake(t, premiumDM.ID)

		payload := map[string]any{
			"content":     "Multi-sticker test",
			"sticker_ids": []string{"111111111111111111", "222222222222222222", "333333333333333333"},
		}

		resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages", premiumDMID), payload, premiumUser.Token)
		if err != nil {
			t.Fatalf("failed to send message: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK {
			t.Error("expected message to fail with invalid stickers")
		}
	})
}
