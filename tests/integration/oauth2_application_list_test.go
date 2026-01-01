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

// TestOAuth2ApplicationList validates listing all applications owned by the current user.
func TestOAuth2ApplicationList(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	initialApps := listOAuth2Applications(t, client, owner.Token)
	initialCount := len(initialApps)

	app1Name := fmt.Sprintf("List Test App 1 %d", time.Now().UnixNano())
	app1ID, _, _ := createOAuth2BotApplication(t, client, owner, app1Name, []string{"https://example.com/app1"})

	app2Name := fmt.Sprintf("List Test App 2 %d", time.Now().UnixNano())
	app2ID, _, _ := createOAuth2BotApplication(t, client, owner, app2Name, []string{"https://example.com/app2"})

	apps := listOAuth2Applications(t, client, owner.Token)

	if len(apps) != initialCount+2 {
		t.Fatalf("expected %d applications, got %d", initialCount+2, len(apps))
	}

	// Find the created applications
	var foundApp1, foundApp2 bool
	for _, app := range apps {
		if app.ID == app1ID {
			foundApp1 = true
			if app.Name != app1Name {
				t.Fatalf("app1 name mismatch: expected %q, got %q", app1Name, app.Name)
			}
			if app.Bot == nil {
				t.Fatalf("app1 should have a bot user")
			}
			if app.Bot.Token != "" {
				t.Fatalf("bot token should not be returned in list responses")
			}
			if app.ClientSecret != "" {
				t.Fatalf("client_secret should not be returned in list responses")
			}
		}
		if app.ID == app2ID {
			foundApp2 = true
			if app.Name != app2Name {
				t.Fatalf("app2 name mismatch: expected %q, got %q", app2Name, app.Name)
			}
			if app.Bot == nil {
				t.Fatalf("app2 should have a bot user")
			}
			if app.Bot.Token != "" {
				t.Fatalf("bot token should not be returned in list responses")
			}
			if app.ClientSecret != "" {
				t.Fatalf("client_secret should not be returned in list responses")
			}
		}
	}

	if !foundApp1 {
		t.Fatalf("app1 not found in list response")
	}
	if !foundApp2 {
		t.Fatalf("app2 not found in list response")
	}
}
