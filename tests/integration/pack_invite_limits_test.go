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

func TestPackInviteMaxUsesLimit(t *testing.T) {
	client := newTestClient(t)

	creator := createTestAccount(t, client)
	grantPremium(t, client, creator.UserID, PremiumTypeSubscription)
	pack := createPack(t, client, creator.Token, "emoji", "Test Pack", "Pack with max-uses invite")
	packID := parseSnowflake(t, pack.ID)

	createPackEmoji(t, client, creator.Token, packID, "test_emoji")

	invite := createPackInvite(t, client, creator.Token, packID, 1, 0, true)

	recipient1 := createTestAccount(t, client)
	grantPremium(t, client, recipient1.UserID, PremiumTypeSubscription)
	acceptPackInvite(t, client, invite.Code, recipient1.Token)

	recipient2 := createTestAccount(t, client)
	grantPremium(t, client, recipient2.UserID, PremiumTypeSubscription)
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, recipient2.Token)
	if err != nil {
		t.Fatalf("failed to attempt second invite accept: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusNotFound)
}

func TestPackInviteNonOwnerCannotCreate(t *testing.T) {
	client := newTestClient(t)

	creator := createTestAccount(t, client)
	grantPremium(t, client, creator.UserID, PremiumTypeSubscription)
	pack := createPack(t, client, creator.Token, "sticker", "Test Sticker Pack", "Pack for access denial test")
	packID := parseSnowflake(t, pack.ID)

	otherUser := createTestAccount(t, client)
	grantPremium(t, client, otherUser.UserID, PremiumTypeSubscription)

	payload := map[string]any{
		"max_uses": 0,
		"max_age":  0,
		"unique":   false,
	}
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/packs/%d/invites", packID), payload, otherUser.Token)
	if err != nil {
		t.Fatalf("failed to hit create pack invite endpoint: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusForbidden)
}

func TestStickerPackInviteFlow(t *testing.T) {
	client := newTestClient(t)

	creator := createTestAccount(t, client)
	grantPremium(t, client, creator.UserID, PremiumTypeSubscription)
	pack := createPack(t, client, creator.Token, "sticker", "Test Sticker Pack", "Sticker pack invite test")
	packID := parseSnowflake(t, pack.ID)

	createPackSticker(t, client, creator.Token, packID, "test_sticker")

	invite := createPackInvite(t, client, creator.Token, packID, 0, 0, false)

	recipient := createTestAccount(t, client)
	grantPremium(t, client, recipient.UserID, PremiumTypeSubscription)
	acceptPackInvite(t, client, invite.Code, recipient.Token)

	packs := listPacks(t, client, recipient.Token)
	found := false
	for _, installedPack := range packs.Sticker.Installed {
		if installedPack.ID == pack.ID {
			found = true
			break
		}
	}
	if !found {
		t.Fatalf("expected sticker pack %s to be in installed list", pack.ID)
	}
}

func TestPackInviteDelete(t *testing.T) {
	client := newTestClient(t)

	creator := createTestAccount(t, client)
	grantPremium(t, client, creator.UserID, PremiumTypeSubscription)
	pack := createPack(t, client, creator.Token, "emoji", "Delete Test Pack", "Pack for invite deletion test")
	packID := parseSnowflake(t, pack.ID)

	invite := createPackInvite(t, client, creator.Token, packID, 0, 0, true)

	resp, err := client.delete(fmt.Sprintf("/invites/%s", invite.Code), creator.Token)
	if err != nil {
		t.Fatalf("failed to delete pack invite: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusNoContent)

	fetchResp, err := client.get(fmt.Sprintf("/invites/%s", invite.Code))
	if err != nil {
		t.Fatalf("failed to fetch deleted invite: %v", err)
	}
	defer fetchResp.Body.Close()
	assertStatus(t, fetchResp, http.StatusNotFound)
}

func TestPackInviteListOnlyOwner(t *testing.T) {
	client := newTestClient(t)

	creator := createTestAccount(t, client)
	grantPremium(t, client, creator.UserID, PremiumTypeSubscription)
	pack := createPack(t, client, creator.Token, "emoji", "List Test Pack", "Pack for invite list test")
	packID := parseSnowflake(t, pack.ID)

	createPackInvite(t, client, creator.Token, packID, 0, 0, true)

	resp, err := client.getWithAuth(fmt.Sprintf("/packs/%d/invites", packID), creator.Token)
	if err != nil {
		t.Fatalf("failed to list pack invites: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusOK)

	var invites []packInviteMetadataResponse
	decodeJSONResponse(t, resp, &invites)
	if len(invites) != 1 {
		t.Fatalf("expected 1 invite, got %d", len(invites))
	}

	otherUser := createTestAccount(t, client)
	grantPremium(t, client, otherUser.UserID, PremiumTypeSubscription)

	otherResp, err := client.getWithAuth(fmt.Sprintf("/packs/%d/invites", packID), otherUser.Token)
	if err != nil {
		t.Fatalf("failed to attempt list pack invites as non-owner: %v", err)
	}
	defer otherResp.Body.Close()
	assertStatus(t, otherResp, http.StatusForbidden)
}

func createPackSticker(t testing.TB, client *testClient, token string, packID int64, name string) {
	t.Helper()
	payload := map[string]any{
		"name":  name,
		"tags":  []string{"test"},
		"image": loadFixtureAsDataURL(t, "thisisfine.gif", "image/gif"),
	}
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/packs/stickers/%d", packID), payload, token)
	if err != nil {
		t.Fatalf("failed to create pack sticker: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusOK)
}
