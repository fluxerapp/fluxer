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

func TestUserRelationshipByTagEndpoint(t *testing.T) {
	client := newTestClient(t)
	requester := createTestAccount(t, client)
	target := createTestAccount(t, client)

	resp, err := client.getWithAuth("/users/@me", target.Token)
	if err != nil {
		t.Fatalf("failed to fetch target profile: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var targetProfile userPrivateResponse
	decodeJSONResponse(t, resp, &targetProfile)

	payload := map[string]string{
		"username":      targetProfile.Username,
		"discriminator": targetProfile.Discriminator,
	}
	resp, err = client.postJSONWithAuth("/users/@me/relationships", payload, requester.Token)
	if err != nil {
		t.Fatalf("failed to send friend request by tag: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var rel relationshipResponse
	decodeJSONResponse(t, resp, &rel)
	if rel.User.ID != target.UserID || rel.Type != relationshipOutgoing {
		t.Fatalf("expected outgoing relationship for %s, got %+v", target.UserID, rel)
	}
}
