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
	"time"
)

func TestBotAuthorizeWithGuildSelectionRedirects(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	guildOwner := createTestAccount(t, client)

	appID, _, _, _ := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Bot Guild Selection %d", time.Now().UnixNano()),
		nil,
		[]string{"bot"},
	)

	guild := createGuild(t, client, guildOwner.Token, fmt.Sprintf("Bot Guild %d", time.Now().UnixNano()))

	payload := map[string]any{
		"client_id": appID,
		"scope":     "bot",
		"guild_id":  guild.ID,
	}

	resp, err := client.postJSONWithAuth("/oauth2/authorize/consent", payload, guildOwner.Token)
	if err != nil {
		t.Fatalf("failed to authorize: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Fatalf("expected 200 when selecting guild, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var body struct {
		RedirectTo string `json:"redirect_to"`
	}
	decodeJSONResponse(t, resp, &body)
	if body.RedirectTo == "" {
		t.Fatal("missing redirect_to on consent response")
	}
}
