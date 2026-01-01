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

func TestRelationshipAcceptAfterPrivacyChangeAllowsExistingRequest(t *testing.T) {
	client := newTestClient(t)
	alice := createTestAccount(t, client)
	bob := createTestAccount(t, client)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/users/@me/relationships/%s", bob.UserID), nil, alice.Token)
	if err != nil {
		t.Fatalf("failed to send friend request: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.patchJSONWithAuth("/users/@me/settings", map[string]int{"friend_source_flags": 3}, bob.Token)
	if err != nil {
		t.Fatalf("failed to update friend_source_flags: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.putJSONWithAuth(fmt.Sprintf("/users/@me/relationships/%s", alice.UserID), nil, bob.Token)
	if err != nil {
		t.Fatalf("failed to accept friend request: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var accepted relationshipResponse
	decodeJSONResponse(t, resp, &accepted)
	if accepted.ID != alice.UserID || accepted.Type != relationshipFriend {
		t.Fatalf("expected bob to have friend relationship with alice after acceptance")
	}

	resp, err = client.getWithAuth("/users/@me/relationships", alice.Token)
	if err != nil {
		t.Fatalf("failed to list alice relationships: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var aliceRels []relationshipResponse
	decodeJSONResponse(t, resp, &aliceRels)
	if len(aliceRels) != 1 || aliceRels[0].ID != bob.UserID || aliceRels[0].Type != relationshipFriend {
		t.Fatalf("expected alice to have a friendship with bob after acceptance")
	}
}
