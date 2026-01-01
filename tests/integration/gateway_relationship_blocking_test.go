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

func TestGatewayRelationshipBlocking(t *testing.T) {
	client := newTestClient(t)
	blocker := createTestAccount(t, client)
	blocked := createTestAccount(t, client)

	blockerSocket := newGatewayClient(t, client, blocker.Token)
	t.Cleanup(blockerSocket.Close)
	blockedSocket := newGatewayClient(t, client, blocked.Token)
	t.Cleanup(blockedSocket.Close)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/users/@me/relationships/%s", blocked.UserID), nil, blocker.Token)
	if err != nil {
		t.Fatalf("failed to send friend request: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	blockerSocket.WaitForEvent(t, "RELATIONSHIP_ADD", 30*time.Second, func(raw json.RawMessage) bool {
		var payload relationshipResponse
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode blocker friend request: %v", err)
		}
		return payload.ID == blocked.UserID && payload.Type == relationshipOutgoing
	})

	blockedSocket.WaitForEvent(t, "RELATIONSHIP_ADD", 30*time.Second, func(raw json.RawMessage) bool {
		var payload relationshipResponse
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode blocked friend request: %v", err)
		}
		return payload.ID == blocker.UserID && payload.Type == relationshipIncoming
	})

	resp, err = client.putJSONWithAuth(fmt.Sprintf("/users/@me/relationships/%s", blocker.UserID), nil, blocked.Token)
	if err != nil {
		t.Fatalf("failed to accept friend request: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	blockerSocket.WaitForEvent(t, "RELATIONSHIP_UPDATE", 30*time.Second, func(raw json.RawMessage) bool {
		var payload relationshipResponse
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode blocker friendship: %v", err)
		}
		return payload.ID == blocked.UserID && payload.Type == relationshipFriend
	})

	blockedSocket.WaitForEvent(t, "RELATIONSHIP_UPDATE", 30*time.Second, func(raw json.RawMessage) bool {
		var payload relationshipResponse
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode blocked friendship: %v", err)
		}
		return payload.ID == blocker.UserID && payload.Type == relationshipFriend
	})

	resp, err = client.putJSONWithAuth(fmt.Sprintf("/users/@me/relationships/%s", blocked.UserID), map[string]int{"type": relationshipBlocked}, blocker.Token)
	if err != nil {
		t.Fatalf("failed to block user: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	blockerSocket.WaitForEvent(t, "RELATIONSHIP_ADD", 30*time.Second, func(raw json.RawMessage) bool {
		var payload relationshipResponse
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode blocker relationship add: %v", err)
		}
		return payload.ID == blocked.UserID && payload.Type == relationshipBlocked
	})

	blockedSocket.WaitForEvent(t, "RELATIONSHIP_REMOVE", 30*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			ID string `json:"id"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode blocked relationship remove: %v", err)
		}
		return payload.ID == blocker.UserID
	})

	resp, err = client.delete(fmt.Sprintf("/users/@me/relationships/%s", blocked.UserID), blocker.Token)
	if err != nil {
		t.Fatalf("failed to unblock user: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	blockerSocket.WaitForEvent(t, "RELATIONSHIP_REMOVE", 30*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			ID string `json:"id"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode unblock relationship remove: %v", err)
		}
		return payload.ID == blocked.UserID
	})
}
