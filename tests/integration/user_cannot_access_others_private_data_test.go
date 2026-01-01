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

// TestUserCannotAccessOthersPrivateData tests privacy of user data
func TestUserCannotAccessOthersPrivateData(t *testing.T) {
	client := newTestClient(t)
	_ = createTestAccount(t, client)
	user2 := createTestAccount(t, client)

	resp, err := client.getWithAuth("/users/@me/settings", user2.Token)
	if err != nil {
		t.Fatalf("failed to get own settings: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var settings struct {
		Status string `json:"status"`
	}
	decodeJSONResponse(t, resp, &settings)

	resp, err = client.getWithAuth("/users/@me/notes", user2.Token)
	if err != nil {
		t.Fatalf("failed to get notes: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.getWithAuth("/users/@me/relationships", user2.Token)
	if err != nil {
		t.Fatalf("failed to get relationships: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.getWithAuth("/users/@me/mentions?limit=10", user2.Token)
	if err != nil {
		t.Fatalf("failed to get mentions: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.getWithAuth("/users/@me/saved-messages?limit=10", user2.Token)
	if err != nil {
		t.Fatalf("failed to get saved messages: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

}
