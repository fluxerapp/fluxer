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
	"strings"
	"testing"
	"time"
)

// TestBotUsernameCaseChange verifies that changing only the case of a bot's username
// preserves the discriminator. Case-only changes should not require discriminator
// reassignment because the normalized username remains the same.
func TestBotUsernameCaseChange(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	initialUsername := fmt.Sprintf("testbot%d", time.Now().UnixNano()%10000)
	redirectURI := "https://example.com/callback"
	appID, botUserID, _ := createOAuth2BotApplication(t, client, owner, initialUsername, []string{redirectURI})

	initialApp := getOAuth2Application(t, client, owner.Token, appID)
	if initialApp.Bot == nil {
		t.Fatalf("application should have bot field")
	}

	initialDiscriminator := initialApp.Bot.Discriminator
	if initialDiscriminator == "" {
		t.Fatalf("bot should have a discriminator, got empty string")
	}

	initialUsernameReturned := initialApp.Bot.Username
	if initialUsernameReturned == "" {
		t.Fatalf("bot should have a username, got empty string")
	}

	t.Logf("Initial bot - ID: %s, Username: %s, Discriminator: %s", botUserID, initialUsernameReturned, initialDiscriminator)

	uppercaseUsername := strings.ToUpper(initialUsernameReturned)
	if uppercaseUsername == initialUsernameReturned {
		uppercaseUsername = strings.ToLower(initialUsernameReturned)
	}

	updatedProfile := updateBotProfile(t, client, owner.Token, appID, map[string]any{
		"username": uppercaseUsername,
	})

	updatedUsername, ok := updatedProfile["username"].(string)
	if !ok || updatedUsername != uppercaseUsername {
		t.Fatalf("expected username %s, got %v", uppercaseUsername, updatedProfile["username"])
	}

	updatedDiscriminator, ok := updatedProfile["discriminator"].(string)
	if !ok {
		t.Fatalf("updated profile should include discriminator, got %v", updatedProfile["discriminator"])
	}

	if updatedDiscriminator != initialDiscriminator {
		t.Fatalf("discriminator should be preserved on case-only change, expected %s, got %s", initialDiscriminator, updatedDiscriminator)
	}

	t.Logf("After case change - Username: %s, Discriminator: %s (preserved)", updatedUsername, updatedDiscriminator)

	mixedCaseUsername := ""
	for i, char := range updatedUsername {
		if i%2 == 0 {
			mixedCaseUsername += strings.ToUpper(string(char))
		} else {
			mixedCaseUsername += strings.ToLower(string(char))
		}
	}

	updatedProfile = updateBotProfile(t, client, owner.Token, appID, map[string]any{
		"username": mixedCaseUsername,
	})

	finalUsername, ok := updatedProfile["username"].(string)
	if !ok || finalUsername != mixedCaseUsername {
		t.Fatalf("expected username %s, got %v", mixedCaseUsername, updatedProfile["username"])
	}

	finalDiscriminator, ok := updatedProfile["discriminator"].(string)
	if !ok {
		t.Fatalf("final profile should include discriminator, got %v", updatedProfile["discriminator"])
	}

	if finalDiscriminator != initialDiscriminator {
		t.Fatalf("discriminator should be preserved on case-only change, expected %s, got %s", initialDiscriminator, finalDiscriminator)
	}

	t.Logf("After mixed case change - Username: %s, Discriminator: %s (preserved)", finalUsername, finalDiscriminator)

	differentUsername := fmt.Sprintf("differentbot%d", time.Now().UnixNano()%10000)
	updatedProfile = updateBotProfile(t, client, owner.Token, appID, map[string]any{
		"username": differentUsername,
	})

	newUsername, ok := updatedProfile["username"].(string)
	if !ok || newUsername != differentUsername {
		t.Fatalf("expected username %s, got %v", differentUsername, updatedProfile["username"])
	}

	newDiscriminator, ok := updatedProfile["discriminator"].(string)
	if !ok {
		t.Fatalf("new profile should include discriminator, got %v", updatedProfile["discriminator"])
	}

	t.Logf("After full name change - Username: %s, Discriminator: %s", newUsername, newDiscriminator)

	uppercaseDifferent := strings.ToUpper(differentUsername)
	updatedProfile = updateBotProfile(t, client, owner.Token, appID, map[string]any{
		"username": uppercaseDifferent,
	})

	verifyUsername, ok := updatedProfile["username"].(string)
	if !ok || verifyUsername != uppercaseDifferent {
		t.Fatalf("expected username %s, got %v", uppercaseDifferent, updatedProfile["username"])
	}

	verifyDiscriminator, ok := updatedProfile["discriminator"].(string)
	if !ok {
		t.Fatalf("verify profile should include discriminator, got %v", updatedProfile["discriminator"])
	}

	if verifyDiscriminator != newDiscriminator {
		t.Fatalf("discriminator should be preserved on case-only change, expected %s, got %s", newDiscriminator, verifyDiscriminator)
	}

	t.Logf("Case-only username changes preserve discriminators correctly")
}
