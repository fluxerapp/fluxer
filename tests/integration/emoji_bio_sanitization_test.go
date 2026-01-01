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
	"strings"
	"testing"
)

func TestEmoji_BioSanitization(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	t.Run("non-premium user has external emoji replaced in bio", func(t *testing.T) {
		payload := map[string]any{
			"bio": "My bio <:custom:999999999999999999>",
		}

		resp, err := client.patchJSONWithAuth("/users/@me", payload, user.Token)
		if err != nil {
			t.Fatalf("failed to update profile: %v", err)
		}
		defer resp.Body.Close()
		assertStatus(t, resp, http.StatusOK)

		fetchResp, err := client.getWithAuth("/users/@me", user.Token)
		if err != nil {
			t.Fatalf("failed to fetch user: %v", err)
		}
		defer fetchResp.Body.Close()

		var result struct {
			Bio string `json:"bio"`
		}
		decodeJSONResponse(t, fetchResp, &result)

		if strings.Contains(result.Bio, "<:custom:") {
			t.Error("expected external emoji to be sanitized in bio")
		}

		t.Logf("Sanitized bio: %s", result.Bio)
	})

	t.Run("non-premium user animated emoji replaced in bio", func(t *testing.T) {
		payload := map[string]any{
			"bio": "Animated <a:party:999999999999999998>",
		}

		resp, err := client.patchJSONWithAuth("/users/@me", payload, user.Token)
		if err != nil {
			t.Fatalf("failed to update profile: %v", err)
		}
		defer resp.Body.Close()
		assertStatus(t, resp, http.StatusOK)

		fetchResp, err := client.getWithAuth("/users/@me", user.Token)
		if err != nil {
			t.Fatalf("failed to fetch user: %v", err)
		}
		defer fetchResp.Body.Close()

		var result struct {
			Bio string `json:"bio"`
		}
		decodeJSONResponse(t, fetchResp, &result)

		if strings.Contains(result.Bio, "<a:party:") {
			t.Error("expected animated emoji to be sanitized in bio")
		}
	})

	t.Run("unicode emoji allowed in bio", func(t *testing.T) {
		payload := map[string]any{
			"bio": "Hello 👋 I love coding 💻",
		}

		resp, err := client.patchJSONWithAuth("/users/@me", payload, user.Token)
		if err != nil {
			t.Fatalf("failed to update profile: %v", err)
		}
		defer resp.Body.Close()
		assertStatus(t, resp, http.StatusOK)

		fetchResp, err := client.getWithAuth("/users/@me", user.Token)
		if err != nil {
			t.Fatalf("failed to fetch user: %v", err)
		}
		defer fetchResp.Body.Close()

		var result struct {
			Bio string `json:"bio"`
		}
		decodeJSONResponse(t, fetchResp, &result)

		if !strings.Contains(result.Bio, "👋") {
			t.Error("expected unicode emoji to be preserved in bio")
		}

		if !strings.Contains(result.Bio, "💻") {
			t.Error("expected unicode emoji to be preserved in bio")
		}
	})

	t.Run("premium user can use external emoji in bio", func(t *testing.T) {
		premiumUser := createTestAccount(t, client)
		ensureSessionStarted(t, client, premiumUser.Token)
		grantPremium(t, client, premiumUser.UserID, PremiumTypeSubscription)

		guild := createGuild(t, client, premiumUser.Token, "Emoji Guild")
		guildID := parseSnowflake(t, guild.ID)
		emoji := createGuildEmoji(t, client, premiumUser.Token, guildID, "premium_emoji")

		payload := map[string]any{
			"bio": fmt.Sprintf("Premium bio <:premium_emoji:%s>", emoji.ID),
		}

		resp, err := client.patchJSONWithAuth("/users/@me", payload, premiumUser.Token)
		if err != nil {
			t.Fatalf("failed to update profile: %v", err)
		}
		defer resp.Body.Close()
		assertStatus(t, resp, http.StatusOK)

		fetchResp, err := client.getWithAuth("/users/@me", premiumUser.Token)
		if err != nil {
			t.Fatalf("failed to fetch user: %v", err)
		}
		defer fetchResp.Body.Close()

		var result struct {
			Bio string `json:"bio"`
		}
		decodeJSONResponse(t, fetchResp, &result)

		if !strings.Contains(result.Bio, "<:premium_emoji:") {
			t.Errorf("expected premium user bio to contain custom emoji, got: %s", result.Bio)
		}
	})

	t.Run("multiple emojis sanitized individually", func(t *testing.T) {
		payload := map[string]any{
			"bio": "Start <:one:111> and <:two:222> end",
		}

		resp, err := client.patchJSONWithAuth("/users/@me", payload, user.Token)
		if err != nil {
			t.Fatalf("failed to update profile: %v", err)
		}
		defer resp.Body.Close()
		assertStatus(t, resp, http.StatusOK)

		fetchResp, err := client.getWithAuth("/users/@me", user.Token)
		if err != nil {
			t.Fatalf("failed to fetch user: %v", err)
		}
		defer fetchResp.Body.Close()

		var result struct {
			Bio string `json:"bio"`
		}
		decodeJSONResponse(t, fetchResp, &result)

		if strings.Contains(result.Bio, "<:one:") || strings.Contains(result.Bio, "<:two:") {
			t.Error("expected all emojis to be sanitized")
		}

		t.Logf("Multi-emoji bio: %s", result.Bio)
	})

	t.Run("non-premium bio limited to 160 chars", func(t *testing.T) {
		longBio := make([]byte, 200)
		for i := range longBio {
			longBio[i] = 'a'
		}

		payload := map[string]any{
			"bio": string(longBio),
		}

		resp, err := client.patchJSONWithAuth("/users/@me", payload, user.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK {
			t.Error("expected long bio to fail for non-premium user")
		}
	})

	t.Run("premium user can have longer bio", func(t *testing.T) {
		premiumUser := createTestAccount(t, client)
		ensureSessionStarted(t, client, premiumUser.Token)
		grantPremium(t, client, premiumUser.UserID, PremiumTypeSubscription)

		longBio := make([]byte, 200)
		for i := range longBio {
			longBio[i] = 'a'
		}

		payload := map[string]any{
			"bio": string(longBio),
		}

		resp, err := client.patchJSONWithAuth("/users/@me", payload, premiumUser.Token)
		if err != nil {
			t.Fatalf("failed to update profile: %v", err)
		}
		defer resp.Body.Close()
		assertStatus(t, resp, http.StatusOK)
	})
}
