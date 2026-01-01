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

func TestFavoriteMeme_Update(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	t.Run("returns 404 for nonexistent meme", func(t *testing.T) {
		payload := map[string]any{
			"name": "Updated Name",
		}

		resp, err := client.patchJSONWithAuth("/users/@me/memes/999999999999999999", payload, user.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusNotFound {
			t.Errorf("expected 404, got %d", resp.StatusCode)
		}
	})

	t.Run("rejects name over 100 chars", func(t *testing.T) {
		longName := make([]byte, 101)
		for i := range longName {
			longName[i] = 'a'
		}

		payload := map[string]any{
			"name": string(longName),
		}

		resp, err := client.patchJSONWithAuth("/users/@me/memes/123456789", payload, user.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK {
			t.Error("expected request to fail for name over 100 chars")
		}
	})

	t.Run("rejects alt_text over 500 chars", func(t *testing.T) {
		longAltText := make([]byte, 501)
		for i := range longAltText {
			longAltText[i] = 'a'
		}

		payload := map[string]any{
			"alt_text": string(longAltText),
		}

		resp, err := client.patchJSONWithAuth("/users/@me/memes/123456789", payload, user.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK {
			t.Error("expected request to fail for alt_text over 500 chars")
		}
	})

	t.Run("can clear alt_text with null", func(t *testing.T) {
		payload := map[string]any{
			"alt_text": nil,
		}

		resp, err := client.patchJSONWithAuth("/users/@me/memes/123456789", payload, user.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusOK && resp.StatusCode != http.StatusNotFound {
			t.Errorf("expected 200 or 404, got %d", resp.StatusCode)
		}
	})

	t.Run("requires authentication", func(t *testing.T) {
		payload := map[string]any{
			"name": "Updated Name",
		}

		resp, err := client.patchJSONWithAuth("/users/@me/memes/123456789", payload, "")
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusUnauthorized {
			t.Errorf("expected 401, got %d", resp.StatusCode)
		}
	})
}
