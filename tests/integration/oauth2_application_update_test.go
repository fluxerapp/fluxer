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

// TestOAuth2ApplicationUpdate validates updating an application's name.
func TestOAuth2ApplicationUpdate(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	originalName := fmt.Sprintf("Original Name %d", time.Now().UnixNano())
	redirectURIs := []string{"https://example.com/callback"}

	appID, _, _ := createOAuth2BotApplication(t, client, owner, originalName, redirectURIs)

	newName := fmt.Sprintf("Updated Name %d", time.Now().UnixNano())
	updates := map[string]any{
		"name": newName,
	}

	updated := updateOAuth2Application(t, client, owner.Token, appID, updates)

	if updated.Name != newName {
		t.Fatalf("expected name %q, got %q", newName, updated.Name)
	}
	if updated.ID != appID {
		t.Fatalf("application id should not change")
	}

	if len(updated.RedirectURIs) != len(redirectURIs) || updated.RedirectURIs[0] != redirectURIs[0] {
		t.Fatalf("redirect_uris should remain unchanged")
	}
}
