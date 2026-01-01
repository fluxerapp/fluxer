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

// TestBotProfileUpdate verifies that bot profiles can be updated via the OAuth2 application API.
// This tests updating username, avatar, and bio fields. Discriminator changes are only
// allowed for lifetime premium users to keep discriminator allocation stable.
func TestBotProfileUpdate(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	appName := fmt.Sprintf("Test Bot %d", time.Now().UnixNano())
	redirectURI := "https://example.com/callback"
	appID, botUserID, botToken := createOAuth2BotApplication(t, client, owner, appName, []string{redirectURI})

	if appID == "" || botUserID == "" || botToken == "" {
		t.Fatalf("bot application creation failed")
	}

	newUsername := fmt.Sprintf("UpdatedBot%d", time.Now().UnixNano()%10000)
	updatedProfile := updateBotProfile(t, client, owner.Token, appID, map[string]any{
		"username": newUsername,
	})

	if username, ok := updatedProfile["username"].(string); !ok || username != newUsername {
		t.Fatalf("expected username %s, got %v", newUsername, updatedProfile["username"])
	}

	newBio := "This is a test bot for integration testing"
	updatedProfile = updateBotProfile(t, client, owner.Token, appID, map[string]any{
		"bio": newBio,
	})

	if bio, ok := updatedProfile["bio"].(string); !ok || bio != newBio {
		t.Fatalf("expected bio %s, got %v", newBio, updatedProfile["bio"])
	}

	avatarDataURI := "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg=="
	updatedProfile = updateBotProfile(t, client, owner.Token, appID, map[string]any{
		"avatar": avatarDataURI,
	})

	if avatar, ok := updatedProfile["avatar"].(string); !ok || avatar == "" {
		t.Fatalf("expected avatar hash, got %v", updatedProfile["avatar"])
	}

	finalUsername := fmt.Sprintf("FinalBot%d", time.Now().UnixNano()%10000)
	finalBio := "Updated bio and username together"
	updatedProfile = updateBotProfile(t, client, owner.Token, appID, map[string]any{
		"username": finalUsername,
		"bio":      finalBio,
	})

	if username, ok := updatedProfile["username"].(string); !ok || username != finalUsername {
		t.Fatalf("expected username %s, got %v", finalUsername, updatedProfile["username"])
	}
	if bio, ok := updatedProfile["bio"].(string); !ok || bio != finalBio {
		t.Fatalf("expected bio %s, got %v", finalBio, updatedProfile["bio"])
	}

	if discriminator, ok := updatedProfile["discriminator"].(string); !ok || discriminator == "" {
		t.Fatalf("bot profile should include discriminator field, got %v", updatedProfile["discriminator"])
	}

	resp, err := client.patchJSONWithAuth(
		fmt.Sprintf("/oauth2/applications/%s/bot", appID),
		map[string]any{"discriminator": "1234"},
		owner.Token,
	)
	if err != nil {
		t.Fatalf("failed to attempt discriminator update: %v", err)
	}

	if resp.StatusCode != http.StatusOK && resp.StatusCode != http.StatusBadRequest && resp.StatusCode != http.StatusForbidden {
		t.Fatalf("unexpected status code for discriminator update: %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	app := getOAuth2Application(t, client, owner.Token, appID)
	if app.Bot == nil {
		t.Fatalf("application should have bot field")
	}

	if app.Bot.ID != botUserID {
		t.Fatalf("expected bot id %s, got %s", botUserID, app.Bot.ID)
	}

	t.Logf("Successfully updated bot profile - username: %s, bio: %s", finalUsername, finalBio)
}
