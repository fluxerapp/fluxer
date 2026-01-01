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
	"net/http"
	"testing"
)

func TestFavoriteMeme_CreateFromURL(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	t.Run("can create meme from valid image URL", func(t *testing.T) {
		payload := map[string]any{
			"url":  favoriteMemeTestImageURL,
			"name": "My Test Meme",
		}

		resp, err := client.postJSONWithAuth("/users/@me/memes", payload, user.Token)
		if err != nil {
			t.Fatalf("failed to create meme: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusOK && resp.StatusCode != http.StatusCreated {
			t.Fatalf("expected successful meme creation, got status: %d", resp.StatusCode)
		}
	})

	t.Run("can create meme with all metadata", func(t *testing.T) {
		payload := map[string]any{
			"url":      favoriteMemeTestImageURL,
			"name":     "Full Metadata Meme",
			"alt_text": "A funny meme for testing purposes",
			"tags":     []string{"funny", "test", "meme"},
		}

		resp, err := client.postJSONWithAuth("/users/@me/memes", payload, user.Token)
		if err != nil {
			t.Fatalf("failed to create meme: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusCreated {
			var result favoriteMemeResponse
			decodeJSONResponse(t, resp, &result)

			if result.Name != "Full Metadata Meme" {
				t.Errorf("expected name 'Full Metadata Meme', got '%s'", result.Name)
			}
		}
	})

	t.Run("rejects name over 100 chars", func(t *testing.T) {
		longName := make([]byte, 101)
		for i := range longName {
			longName[i] = 'a'
		}

		payload := map[string]any{
			"url":  favoriteMemeTestImageURL,
			"name": string(longName),
		}

		resp, err := client.postJSONWithAuth("/users/@me/memes", payload, user.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusCreated {
			t.Error("expected request to fail for name over 100 chars")
		}
	})

	t.Run("rejects alt_text over 500 chars", func(t *testing.T) {
		longAltText := make([]byte, 501)
		for i := range longAltText {
			longAltText[i] = 'a'
		}

		payload := map[string]any{
			"url":      favoriteMemeTestImageURL,
			"name":     "Test Meme",
			"alt_text": string(longAltText),
		}

		resp, err := client.postJSONWithAuth("/users/@me/memes", payload, user.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusCreated {
			t.Error("expected request to fail for alt_text over 500 chars")
		}
	})

	t.Run("rejects more than 10 tags", func(t *testing.T) {
		tags := make([]string, 11)
		for i := range tags {
			tags[i] = "tag"
		}

		payload := map[string]any{
			"url":  favoriteMemeTestImageURL,
			"name": "Test Meme",
			"tags": tags,
		}

		resp, err := client.postJSONWithAuth("/users/@me/memes", payload, user.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusCreated {
			t.Error("expected request to fail for more than 10 tags")
		}
	})

	t.Run("rejects tag over 30 chars", func(t *testing.T) {
		longTag := make([]byte, 31)
		for i := range longTag {
			longTag[i] = 'a'
		}

		payload := map[string]any{
			"url":  favoriteMemeTestImageURL,
			"name": "Test Meme",
			"tags": []string{string(longTag)},
		}

		resp, err := client.postJSONWithAuth("/users/@me/memes", payload, user.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusCreated {
			t.Error("expected request to fail for tag over 30 chars")
		}
	})

	t.Run("rejects invalid URL", func(t *testing.T) {
		payload := map[string]any{
			"url":  "not-a-valid-url",
			"name": "Test Meme",
		}

		resp, err := client.postJSONWithAuth("/users/@me/memes", payload, user.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusCreated {
			t.Error("expected request to fail for invalid URL")
		}
	})

	t.Run("rejects missing URL", func(t *testing.T) {
		payload := map[string]any{
			"name": "Test Meme",
		}

		resp, err := client.postJSONWithAuth("/users/@me/memes", payload, user.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusCreated {
			t.Error("expected request to fail for missing URL")
		}
	})

	t.Run("requires authentication", func(t *testing.T) {
		payload := map[string]any{
			"url":  favoriteMemeTestImageURL,
			"name": "Test Meme",
		}

		resp, err := client.postJSON("/users/@me/memes", payload)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusUnauthorized {
			t.Errorf("expected 401, got %d", resp.StatusCode)
		}
	})
}
