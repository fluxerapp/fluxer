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

func TestUserRelationshipEndpoints(t *testing.T) {
	client := newTestClient(t)
	requester := createTestAccount(t, client)
	target := createTestAccount(t, client)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/users/@me/relationships/%s", target.UserID), nil, requester.Token)
	if err != nil {
		t.Fatalf("failed to send friend request: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var outgoing relationshipResponse
	decodeJSONResponse(t, resp, &outgoing)
	if outgoing.Type != relationshipOutgoing {
		t.Fatalf("expected outgoing relationship type, got %d", outgoing.Type)
	}

	resp, err = client.getWithAuth("/users/@me/relationships", target.Token)
	if err != nil {
		t.Fatalf("failed to list relationships for target: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var incoming []relationshipResponse
	decodeJSONResponse(t, resp, &incoming)
	if len(incoming) != 1 || incoming[0].Type != relationshipIncoming {
		t.Fatalf("expected one incoming request")
	}

	resp, err = client.putJSONWithAuth(fmt.Sprintf("/users/@me/relationships/%s", requester.UserID), nil, target.Token)
	if err != nil {
		t.Fatalf("failed to accept friend request: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var accepted relationshipResponse
	decodeJSONResponse(t, resp, &accepted)
	if accepted.Type != relationshipFriend {
		t.Fatalf("expected friendship after acceptance, got %d", accepted.Type)
	}

	resp, err = client.getWithAuth("/users/@me/relationships", requester.Token)
	if err != nil {
		t.Fatalf("failed to list requester relationships: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var requesterRels []relationshipResponse
	decodeJSONResponse(t, resp, &requesterRels)
	if len(requesterRels) != 1 || requesterRels[0].Type != relationshipFriend {
		t.Fatalf("expected requester to have one friend")
	}

	resp, err = client.delete(fmt.Sprintf("/users/@me/relationships/%s", target.UserID), requester.Token)
	if err != nil {
		t.Fatalf("failed to delete relationship: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.getWithAuth("/users/@me/relationships", target.Token)
	if err != nil {
		t.Fatalf("failed to list target relationships after delete: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var afterDelete []relationshipResponse
	decodeJSONResponse(t, resp, &afterDelete)
	if len(afterDelete) != 0 {
		t.Fatalf("expected no relationships after deletion, got %d", len(afterDelete))
	}

	resp, err = client.putJSONWithAuth(fmt.Sprintf("/users/@me/relationships/%s", requester.UserID), map[string]int{"type": relationshipBlocked}, target.Token)
	if err != nil {
		t.Fatalf("failed to block requester: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var blocked relationshipResponse
	decodeJSONResponse(t, resp, &blocked)
	if blocked.Type != relationshipBlocked {
		t.Fatalf("expected blocked relationship, got %d", blocked.Type)
	}
}
