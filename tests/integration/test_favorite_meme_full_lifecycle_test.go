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
)

func TestFavoriteMeme_FullLifecycle(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	createPayload := map[string]any{
		"url":      favoriteMemeTestImageURL,
		"name":     "Lifecycle Test Meme",
		"alt_text": "Original alt text",
		"tags":     []string{"test", "lifecycle"},
	}

	createResp, err := client.postJSONWithAuth("/users/@me/memes", createPayload, user.Token)
	if err != nil {
		t.Fatalf("failed to create meme: %v", err)
	}
	defer createResp.Body.Close()
	assertStatus(t, createResp, http.StatusCreated)

	var createdMeme favoriteMemeResponse
	decodeJSONResponse(t, createResp, &createdMeme)

	if createdMeme.ID == "" {
		t.Fatal("created meme should have an ID")
	}

	getResp, err := client.getWithAuth(fmt.Sprintf("/users/@me/memes/%s", createdMeme.ID), user.Token)
	if err != nil {
		t.Fatalf("failed to get meme: %v", err)
	}
	defer getResp.Body.Close()
	assertStatus(t, getResp, http.StatusOK)

	var gotMeme favoriteMemeResponse
	decodeJSONResponse(t, getResp, &gotMeme)

	if gotMeme.Name != "Lifecycle Test Meme" {
		t.Errorf("expected name 'Lifecycle Test Meme', got '%s'", gotMeme.Name)
	}

	updatePayload := map[string]any{
		"name":     "Updated Meme Name",
		"alt_text": "Updated alt text",
		"tags":     []string{"updated", "test"},
	}

	updateResp, err := client.patchJSONWithAuth(fmt.Sprintf("/users/@me/memes/%s", createdMeme.ID), updatePayload, user.Token)
	if err != nil {
		t.Fatalf("failed to update meme: %v", err)
	}
	defer updateResp.Body.Close()
	assertStatus(t, updateResp, http.StatusOK)

	var updatedMeme favoriteMemeResponse
	decodeJSONResponse(t, updateResp, &updatedMeme)

	if updatedMeme.Name != "Updated Meme Name" {
		t.Errorf("expected updated name 'Updated Meme Name', got '%s'", updatedMeme.Name)
	}

	listResp, err := client.getWithAuth("/users/@me/memes", user.Token)
	if err != nil {
		t.Fatalf("failed to list memes: %v", err)
	}
	defer listResp.Body.Close()
	assertStatus(t, listResp, http.StatusOK)

	var memes []favoriteMemeResponse
	decodeJSONResponse(t, listResp, &memes)

	found := false
	for _, m := range memes {
		if m.ID == createdMeme.ID {
			found = true
			break
		}
	}
	if !found {
		t.Error("created meme not found in list")
	}

	deleteResp, err := client.delete(fmt.Sprintf("/users/@me/memes/%s", createdMeme.ID), user.Token)
	if err != nil {
		t.Fatalf("failed to delete meme: %v", err)
	}
	defer deleteResp.Body.Close()

	if deleteResp.StatusCode != http.StatusOK && deleteResp.StatusCode != http.StatusNoContent {
		t.Errorf("expected 200 or 204, got %d", deleteResp.StatusCode)
	}

	verifyResp, err := client.getWithAuth(fmt.Sprintf("/users/@me/memes/%s", createdMeme.ID), user.Token)
	if err != nil {
		t.Fatalf("failed to verify deletion: %v", err)
	}
	defer verifyResp.Body.Close()

	if verifyResp.StatusCode != http.StatusNotFound {
		t.Errorf("expected 404 after deletion, got %d", verifyResp.StatusCode)
	}
}
