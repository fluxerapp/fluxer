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

// TestOAuth2ApplicationUpdatePartial validates that partial updates work correctly.
func TestOAuth2ApplicationUpdatePartial(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	originalName := fmt.Sprintf("Partial Original %d", time.Now().UnixNano())
	originalURIs := []string{"https://example.com/old"}

	appID, _, _ := createOAuth2BotApplication(t, client, owner, originalName, originalURIs)

	newName := fmt.Sprintf("Partial Updated %d", time.Now().UnixNano())
	updates := map[string]any{
		"name": newName,
	}

	updated := updateOAuth2Application(t, client, owner.Token, appID, updates)

	if updated.Name != newName {
		t.Fatalf("expected name %q, got %q", newName, updated.Name)
	}

	if len(updated.RedirectURIs) != len(originalURIs) || updated.RedirectURIs[0] != originalURIs[0] {
		t.Fatalf("redirect_uris should remain unchanged: expected %v, got %v", originalURIs, updated.RedirectURIs)
	}
}
