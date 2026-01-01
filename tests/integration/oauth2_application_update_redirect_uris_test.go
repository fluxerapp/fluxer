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

// TestOAuth2ApplicationUpdateRedirectURIs validates updating redirect URIs.
func TestOAuth2ApplicationUpdateRedirectURIs(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	name := fmt.Sprintf("Update URIs Test %d", time.Now().UnixNano())
	originalURIs := []string{"https://example.com/callback"}

	appID, _, _ := createOAuth2BotApplication(t, client, owner, name, originalURIs)

	newURIs := []string{"https://example.com/new-callback", "https://example.com/other-callback"}
	updates := map[string]any{
		"redirect_uris": newURIs,
	}

	updated := updateOAuth2Application(t, client, owner.Token, appID, updates)

	if len(updated.RedirectURIs) != len(newURIs) {
		t.Fatalf("expected %d redirect URIs, got %d", len(newURIs), len(updated.RedirectURIs))
	}
	for i, uri := range newURIs {
		if updated.RedirectURIs[i] != uri {
			t.Fatalf("redirect_uri[%d] mismatch: expected %q, got %q", i, uri, updated.RedirectURIs[i])
		}
	}

	if updated.Name != name {
		t.Fatalf("name should remain unchanged")
	}
}
