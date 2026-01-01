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

func TestPackCreationAndInstallationRequirePremium(t *testing.T) {
	client := newTestClient(t)

	freeUser := createTestAccount(t, client)
	createPayload := map[string]string{
		"name":        "free pack",
		"description": "should fail",
	}
	resp, err := client.postJSONWithAuth("/packs/emoji", createPayload, freeUser.Token)
	if err != nil {
		t.Fatalf("failed to hit create pack endpoint: %v", err)
	}
	assertStatus(t, resp, http.StatusForbidden)
	resp.Body.Close()

	creator := createTestAccount(t, client)
	grantPremium(t, client, creator.UserID, PremiumTypeSubscription)
	pack := createPack(t, client, creator.Token, "emoji", "Premium Pack", "premium-only pack")
	packID := parseSnowflake(t, pack.ID)

	installResp, err := client.postJSONWithAuth(fmt.Sprintf("/packs/%d/install", packID), nil, freeUser.Token)
	if err != nil {
		t.Fatalf("failed to hit install pack endpoint: %v", err)
	}
	assertStatus(t, installResp, http.StatusForbidden)
	installResp.Body.Close()
}

func TestPackExpressionAdditionRequiresPremium(t *testing.T) {
	client := newTestClient(t)

	creator := createTestAccount(t, client)
	grantPremium(t, client, creator.UserID, PremiumTypeSubscription)
	pack := createPack(t, client, creator.Token, "emoji", "Premium Expression Pack", "initial")
	packID := parseSnowflake(t, pack.ID)
	createPackEmoji(t, client, creator.Token, packID, "initial_emoji")

	revokePremium(t, client, creator.UserID)

	payload := map[string]string{
		"name":  "blocked_emoji",
		"image": loadFixtureAsDataURL(t, "yeah.png", "image/png"),
	}
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/packs/emojis/%d", packID), payload, creator.Token)
	if err != nil {
		t.Fatalf("failed to hit create pack emoji endpoint: %v", err)
	}
	assertStatus(t, resp, http.StatusForbidden)
	resp.Body.Close()

	updatePayload := map[string]string{"name": "still-renamable"}
	updateResp, err := client.patchJSONWithAuth(fmt.Sprintf("/packs/%d", packID), updatePayload, creator.Token)
	if err != nil {
		t.Fatalf("failed to hit update pack endpoint: %v", err)
	}
	assertStatus(t, updateResp, http.StatusOK)
	updateResp.Body.Close()
}
