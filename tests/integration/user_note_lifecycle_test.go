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
	"encoding/json"
	"fmt"
	"net/http"
	"testing"
	"time"
)

func TestUserNoteLifecycle(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	target := createTestAccount(t, client)

	socket := newGatewayClient(t, client, user.Token)
	defer socket.Close()

	noteContent := "This is a test note"
	notePayload := map[string]string{
		"note": noteContent,
	}

	resp, err := client.putJSONWithAuth(fmt.Sprintf("/users/@me/notes/%s", target.UserID), notePayload, user.Token)
	if err != nil {
		t.Fatalf("failed to set user note: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	// Verify USER_NOTE_UPDATE event
	socket.WaitForEvent(t, "USER_NOTE_UPDATE", 10*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			ID   string `json:"id"`
			Note string `json:"note"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			return false
		}
		return payload.ID == target.UserID && payload.Note == noteContent
	})

	resp, err = client.getWithAuth(fmt.Sprintf("/users/@me/notes/%s", target.UserID), user.Token)
	if err == nil && resp.StatusCode == http.StatusOK {
		var noteResp struct {
			Note string `json:"note"`
		}
		decodeJSONResponse(t, resp, &noteResp)
		if noteResp.Note != noteContent {
			t.Fatalf("expected note %q, got %q", noteContent, noteResp.Note)
		}
	} else if resp != nil {
		resp.Body.Close()
	}
}
