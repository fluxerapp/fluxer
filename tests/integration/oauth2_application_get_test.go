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
	"testing"
	"time"
)

// TestOAuth2ApplicationGet validates retrieving a single application's details.
func TestOAuth2ApplicationGet(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	name := fmt.Sprintf("Get Test App %d", time.Now().UnixNano())
	redirectURIs := []string{"https://example.com/callback"}
	scopes := []string{"identify", "email", "guilds"}

	appID, _, _, _ := createOAuth2Application(t, client, owner, name, redirectURIs, scopes)

	app := getOAuth2Application(t, client, owner.Token, appID)

	if app.ID != appID {
		t.Fatalf("expected id %s, got %s", appID, app.ID)
	}
	if app.Name != name {
		t.Fatalf("expected name %q, got %q", name, app.Name)
	}
	if len(app.RedirectURIs) != len(redirectURIs) || app.RedirectURIs[0] != redirectURIs[0] {
		t.Fatalf("expected redirect_uris %v, got %v", redirectURIs, app.RedirectURIs)
	}
	if app.RedirectURIs == nil {
		t.Fatalf("expected redirect_uris to be present")
	}

	if app.Bot == nil {
		t.Fatalf("expected bot object in response")
	}
	if app.Bot.ID == "" {
		t.Fatalf("bot response missing id")
	}
	if app.Bot.Username == "" {
		t.Fatalf("bot response missing username")
	}
	if app.Bot.Discriminator == "" {
		t.Fatalf("bot response missing discriminator")
	}

	if app.Bot.Token != "" {
		t.Fatalf("bot token should not be returned in GET requests for security")
	}

	if app.ClientSecret != "" {
		t.Fatalf("client_secret should not be returned in GET requests")
	}
}
