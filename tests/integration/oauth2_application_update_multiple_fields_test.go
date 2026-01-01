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

// TestOAuth2ApplicationUpdateMultipleFields validates updating multiple fields at once.
func TestOAuth2ApplicationUpdateMultipleFields(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	originalName := fmt.Sprintf("Original %d", time.Now().UnixNano())
	originalURIs := []string{"https://example.com/old"}

	appID, _, _ := createOAuth2BotApplication(t, client, owner, originalName, originalURIs)

	newName := fmt.Sprintf("New Name %d", time.Now().UnixNano())
	newURIs := []string{"https://example.com/new1", "https://example.com/new2"}

	updates := map[string]any{
		"name":          newName,
		"redirect_uris": newURIs,
	}

	updated := updateOAuth2Application(t, client, owner.Token, appID, updates)

	if updated.Name != newName {
		t.Fatalf("expected name %q, got %q", newName, updated.Name)
	}
	if len(updated.RedirectURIs) != len(newURIs) {
		t.Fatalf("expected %d redirect URIs, got %d", len(newURIs), len(updated.RedirectURIs))
	}
}
