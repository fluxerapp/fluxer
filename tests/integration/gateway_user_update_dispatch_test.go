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

func TestGatewayUserUpdateDispatch(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	socket := newGatewayClient(t, client, account.Token)
	t.Cleanup(socket.Close)

	newGlobal := fmt.Sprintf("Gateway User %d", time.Now().UnixNano())
	newBio := fmt.Sprintf("Gateway Bio %d", time.Now().UnixNano())
	resp, err := client.patchJSONWithAuth("/users/@me", map[string]any{
		"global_name": newGlobal,
		"bio":         newBio,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to update profile: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	socket.WaitForEvent(t, "USER_UPDATE", 30*time.Second, func(raw json.RawMessage) bool {
		var payload userPrivateResponse
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode user update payload: %v", err)
		}
		return payload.GlobalName == newGlobal && payload.Bio == newBio
	})
}
