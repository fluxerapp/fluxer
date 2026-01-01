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

// TestOAuth2ApplicationUpdateKeepsBot validates updating an application retains bot and hides secrets.
func TestOAuth2ApplicationUpdateKeepsBot(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	originalName := fmt.Sprintf("Conf Original %d", time.Now().UnixNano())
	originalURIs := []string{"https://example.com/callback"}

	appID, botID, _, _ := createOAuth2Application(t, client, owner, originalName, originalURIs, nil)

	newName := fmt.Sprintf("Conf Updated %d", time.Now().UnixNano())
	newURIs := []string{"https://example.com/new-callback"}

	updates := map[string]any{
		"name":          newName,
		"redirect_uris": newURIs,
	}

	updated := updateOAuth2Application(t, client, owner.Token, appID, updates)

	if updated.Name != newName {
		t.Fatalf("expected name %q, got %q", newName, updated.Name)
	}
	if len(updated.RedirectURIs) != len(newURIs) || updated.RedirectURIs[0] != newURIs[0] {
		t.Fatalf("redirect_uris not updated correctly")
	}

	if updated.Bot == nil || updated.Bot.ID != botID {
		t.Fatalf("application should retain its bot user after update")
	}
	if updated.Bot.Token != "" {
		t.Fatalf("bot token should not be returned in update response")
	}
	if updated.ClientSecret != "" {
		t.Fatalf("client_secret should not be returned in update response")
	}
}
