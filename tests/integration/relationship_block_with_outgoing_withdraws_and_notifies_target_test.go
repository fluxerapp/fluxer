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

func TestRelationshipBlockWithOutgoingWithdrawsAndNotifiesTarget(t *testing.T) {
	client := newTestClient(t)
	alice := createTestAccount(t, client)
	bob := createTestAccount(t, client)

	aliceSocket := newGatewayClient(t, client, alice.Token)
	t.Cleanup(aliceSocket.Close)
	bobSocket := newGatewayClient(t, client, bob.Token)
	t.Cleanup(bobSocket.Close)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/users/@me/relationships/%s", bob.UserID), nil, alice.Token)
	if err != nil {
		t.Fatalf("failed to send friend request: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	aliceSocket.WaitForEvent(t, "RELATIONSHIP_ADD", 30*time.Second, func(raw json.RawMessage) bool {
		var payload relationshipResponse
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode relationship add: %v", err)
		}
		return payload.ID == bob.UserID && payload.Type == relationshipOutgoing
	})
	bobSocket.WaitForEvent(t, "RELATIONSHIP_ADD", 30*time.Second, func(raw json.RawMessage) bool {
		var payload relationshipResponse
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode relationship add: %v", err)
		}
		return payload.ID == alice.UserID && payload.Type == relationshipIncoming
	})

	resp, err = client.putJSONWithAuth(
		fmt.Sprintf("/users/@me/relationships/%s", bob.UserID),
		map[string]int{"type": relationshipBlocked},
		alice.Token,
	)
	if err != nil {
		t.Fatalf("failed to block user: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	aliceSocket.WaitForEvent(t, "RELATIONSHIP_ADD", 30*time.Second, func(raw json.RawMessage) bool {
		var payload relationshipResponse
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode relationship add: %v", err)
		}
		return payload.ID == bob.UserID && payload.Type == relationshipBlocked
	})

	bobSocket.WaitForEvent(t, "RELATIONSHIP_REMOVE", 30*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			ID string `json:"id"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode relationship remove: %v", err)
		}
		return payload.ID == alice.UserID
	})

	resp, err = client.getWithAuth("/users/@me/relationships", bob.Token)
	if err != nil {
		t.Fatalf("failed to list bob relationships: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var bobRels []relationshipResponse
	decodeJSONResponse(t, resp, &bobRels)
	if len(bobRels) != 0 {
		t.Fatalf("expected bob to have no relationship with alice after alice blocks, got %d", len(bobRels))
	}
}
