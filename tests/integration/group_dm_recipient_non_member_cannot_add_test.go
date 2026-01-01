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

// TestGroupDMRecipientNonMemberCannotAdd ensures only channel participants can add recipients and friendship rules are enforced
func TestGroupDMRecipientNonMemberCannotAdd(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)
	third := createTestAccount(t, client)
	outsider := createTestAccount(t, client)
	ownerFriend := createTestAccount(t, client)

	createFriendship(t, client, owner, member)
	createFriendship(t, client, owner, third)
	createFriendship(t, client, member, third)
	createFriendship(t, client, outsider, third)
	createFriendship(t, client, owner, ownerFriend)

	groupChannel := createGroupDmChannel(t, client, owner.Token, member.UserID, third.UserID)
	groupID := parseSnowflake(t, groupChannel.ID)

	resp, err := client.putJSONWithAuth(fmt.Sprintf("/channels/%d/recipients/%s", groupID, third.UserID), nil, outsider.Token)
	if err != nil {
		t.Fatalf("outsider failed to attempt add: %v", err)
	}
	if resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusNoContent {
		t.Fatalf("expected non-participant to be blocked from adding recipients")
	}
	resp.Body.Close()

	resp, err = client.putJSONWithAuth(fmt.Sprintf("/channels/%d/recipients/%s", groupID, ownerFriend.UserID), nil, owner.Token)
	if err != nil {
		t.Fatalf("owner failed to add recipient: %v", err)
	}
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("expected owner add to succeed with 204, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.putJSONWithAuth(fmt.Sprintf("/channels/%d/recipients/%s", groupID, outsider.UserID), nil, member.Token)
	if err != nil {
		t.Fatalf("member failed to attempt add outsider after conversion: %v", err)
	}
	if resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusNoContent {
		t.Fatalf("expected member to be unable to add non-friend outsider")
	}
	resp.Body.Close()
}
