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
	"net/url"
	"testing"
	"time"
)

func TestUserAccountAndSettingsEndpoints(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	newGlobal := fmt.Sprintf("Integration %d", time.Now().UnixNano())
	newBio := "Integration tests ensure user endpoints behave"
	resp, err := client.patchJSONWithAuth("/users/@me", map[string]any{
		"global_name": newGlobal,
		"bio":         newBio,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to update profile: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var updated userPrivateResponse
	decodeJSONResponse(t, resp, &updated)
	if updated.GlobalName != newGlobal {
		t.Fatalf("expected global name %q, got %q", newGlobal, updated.GlobalName)
	}
	if updated.Bio != newBio {
		t.Fatalf("expected bio to update")
	}

	tagPath := fmt.Sprintf("/users/check-tag?username=%s&discriminator=%s",
		url.QueryEscape(updated.Username),
		url.QueryEscape(updated.Discriminator),
	)
	resp, err = client.getWithAuth(tagPath, account.Token)
	if err != nil {
		t.Fatalf("failed to call check-tag: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var tagResp tagCheckResponse
	decodeJSONResponse(t, resp, &tagResp)
	if tagResp.Taken {
		t.Fatalf("expected own username/discriminator to be available (not taken)")
	}

	resp, err = client.getWithAuth(fmt.Sprintf("/users/%s", account.UserID), account.Token)
	if err != nil {
		t.Fatalf("failed to fetch user by id: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var partial userPartial
	decodeJSONResponse(t, resp, &partial)
	if partial.ID != account.UserID {
		t.Fatalf("expected user id %s, got %s", account.UserID, partial.ID)
	}

	resp, err = client.getWithAuth(fmt.Sprintf("/users/%s/profile", account.UserID), account.Token)
	if err != nil {
		t.Fatalf("failed to fetch profile: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var profile userProfileEnvelope
	decodeJSONResponse(t, resp, &profile)
	if profile.User.ID != account.UserID {
		t.Fatalf("expected profile user id %s, got %s", account.UserID, profile.User.ID)
	}

	resp, err = client.patchJSONWithAuth("/users/@me/guilds/@me/settings", map[string]any{
		"suppress_everyone": true,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to update default guild settings: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var guildSettings userGuildSettingsResponse
	decodeJSONResponse(t, resp, &guildSettings)
	if !guildSettings.SuppressEveryone {
		t.Fatalf("expected suppress_everyone to be true")
	}

	target := createTestAccount(t, client)
	resp, err = client.putJSONWithAuth(fmt.Sprintf("/users/@me/notes/%s", target.UserID), map[string]string{"note": "Great tester"}, account.Token)
	if err != nil {
		t.Fatalf("failed to set user note: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.postJSONWithAuth(preloadMessagesEndpoint, map[string]any{"channels": []int{}}, account.Token)
	if err != nil {
		t.Fatalf("failed to preload messages: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var preload map[string]any
	decodeJSONResponse(t, resp, &preload)
	if len(preload) != 0 {
		t.Fatalf("expected empty preload response, got %d entries", len(preload))
	}

	deletePayload := map[string]string{
		"password": account.Password,
	}
	resp, err = client.postJSONWithAuth(messagesDeleteEndpoint, deletePayload, account.Token)
	if err != nil {
		t.Fatalf("failed to request message deletion: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	t.Run("reject getting nonexistent user", func(t *testing.T) {
		resp, err := client.getWithAuth("/users/999999999999999999", account.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusNotFound)
		resp.Body.Close()
	})

	t.Run("reject getting nonexistent user profile", func(t *testing.T) {
		resp, err := client.getWithAuth("/users/999999999999999999/profile", account.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusNotFound)
		resp.Body.Close()
	})

	t.Run("check-tag with missing username returns 400", func(t *testing.T) {
		resp, err := client.getWithAuth("/users/check-tag?discriminator=1234", account.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusBadRequest)
		resp.Body.Close()
	})

	t.Run("check-tag with missing discriminator returns 400", func(t *testing.T) {
		resp, err := client.getWithAuth("/users/check-tag?username=testuser", account.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusBadRequest)
		resp.Body.Close()
	})

	t.Run("check-tag with invalid discriminator returns 400", func(t *testing.T) {
		resp, err := client.getWithAuth("/users/check-tag?username=test&discriminator=invalid", account.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusBadRequest)
		resp.Body.Close()
	})
}
