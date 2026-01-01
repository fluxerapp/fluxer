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

type packSummary struct {
	ID          string  `json:"id"`
	Name        string  `json:"name"`
	Description *string `json:"description"`
	Type        string  `json:"type"`
}

type packDashboardSection struct {
	InstalledLimit int           `json:"installed_limit"`
	CreatedLimit   int           `json:"created_limit"`
	Installed      []packSummary `json:"installed"`
	Created        []packSummary `json:"created"`
}

type packDashboardResponse struct {
	Emoji   packDashboardSection `json:"emoji"`
	Sticker packDashboardSection `json:"sticker"`
}

type packInviteMetadataResponse struct {
	Code string `json:"code"`
}

func TestPackInviteInstallationAndExpressionUse(t *testing.T) {
	client := newTestClient(t)
	creator := createTestAccount(t, client)
	grantPremium(t, client, creator.UserID, PremiumTypeSubscription)
	recipient := createTestAccount(t, client)
	grantPremium(t, client, recipient.UserID, PremiumTypeSubscription)

	pack := createPack(t, client, creator.Token, "emoji", "Test Emoji Pack", "Test pack description")
	packSnowflake := parseSnowflake(t, pack.ID)

	emoji := createPackEmoji(t, client, creator.Token, packSnowflake, "pack_emoji")

	invite := createPackInvite(t, client, creator.Token, packSnowflake, 5, 0, true)
	acceptPackInvite(t, client, invite.Code, recipient.Token)

	packs := listPacks(t, client, recipient.Token)
	found := false
	for _, cached := range packs.Emoji.Installed {
		if cached.ID == pack.ID {
			found = true
			break
		}
	}
	if !found {
		t.Fatalf("expected installed pack list to include %s", pack.ID)
	}

	dmChannel := createDmChannel(t, client, recipient.Token, parseSnowflake(t, creator.UserID))
	msg := sendChannelMessage(
		t,
		client,
		recipient.Token,
		parseSnowflake(t, dmChannel.ID),
		fmt.Sprintf("Hi <:pack_emoji:%s>", emoji.ID),
	)

	if msg.ChannelID != dmChannel.ID {
		t.Fatalf("expected message channel %s, got %s", dmChannel.ID, msg.ChannelID)
	}
}

func createPack(t testing.TB, client *testClient, token, packType, name, description string) packSummary {
	t.Helper()
	payload := map[string]string{
		"name":        name,
		"description": description,
	}
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/packs/%s", packType), payload, token)
	if err != nil {
		t.Fatalf("failed to create pack: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusOK)

	var pack packSummary
	decodeJSONResponse(t, resp, &pack)
	if pack.ID == "" {
		t.Fatalf("pack response missing id")
	}
	return pack
}

func createPackEmoji(t testing.TB, client *testClient, token string, packID int64, name string) emojiResponse {
	t.Helper()
	payload := map[string]string{
		"name":  name,
		"image": loadFixtureAsDataURL(t, "yeah.png", "image/png"),
	}
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/packs/emojis/%d", packID), payload, token)
	if err != nil {
		t.Fatalf("failed to create pack emoji: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusOK)

	var emoji emojiResponse
	decodeJSONResponse(t, resp, &emoji)
	return emoji
}

func createPackInvite(t testing.TB, client *testClient, token string, packID int64, maxUses, maxAge int, unique bool) packInviteMetadataResponse {
	t.Helper()
	payload := map[string]any{
		"max_uses": maxUses,
		"max_age":  maxAge,
		"unique":   unique,
	}
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/packs/%d/invites", packID), payload, token)
	if err != nil {
		t.Fatalf("failed to create pack invite: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusOK)

	var invite packInviteMetadataResponse
	decodeJSONResponse(t, resp, &invite)
	if invite.Code == "" {
		t.Fatalf("invite response missing code")
	}
	return invite
}

func acceptPackInvite(t testing.TB, client *testClient, code, token string) {
	t.Helper()
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", code), nil, token)
	if err != nil {
		t.Fatalf("failed to accept pack invite: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusOK)
}

func listPacks(t testing.TB, client *testClient, token string) packDashboardResponse {
	t.Helper()
	resp, err := client.getWithAuth("/packs", token)
	if err != nil {
		t.Fatalf("failed to list packs: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusOK)

	var dashboard packDashboardResponse
	decodeJSONResponse(t, resp, &dashboard)
	return dashboard
}
