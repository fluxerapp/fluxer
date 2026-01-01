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

func TestInviteGroupDMRecipientsSerialization(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)
	recipient := createTestAccount(t, client)

	ensureSessionStarted(t, client, owner.Token)
	ensureSessionStarted(t, client, member.Token)
	ensureSessionStarted(t, client, recipient.Token)

	createFriendship(t, client, owner, member)
	createFriendship(t, client, owner, recipient)

	groupChannel := createGroupDmChannel(t, client, owner.Token, member.UserID, recipient.UserID)
	channelID := parseSnowflake(t, groupChannel.ID)

	invite := createChannelInvite(t, client, owner.Token, channelID)

	resp, err := client.get(fmt.Sprintf("/invites/%s", invite.Code))
	if err != nil {
		t.Fatalf("failed to fetch invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var invitePayload map[string]any
	decodeJSONResponse(t, resp, &invitePayload)

	channelPayload, ok := invitePayload["channel"].(map[string]any)
	if !ok {
		t.Fatalf("expected channel payload in invite response")
	}

	recipientsRaw, ok := channelPayload["recipients"].([]any)
	if !ok {
		t.Fatalf("expected recipients array in channel payload")
	}
	if len(recipientsRaw) < 2 {
		t.Fatalf("expected group DM invite to expose at least two recipients, got %d", len(recipientsRaw))
	}

	for _, raw := range recipientsRaw {
		recipientEntry, ok := raw.(map[string]any)
		if !ok {
			t.Fatalf("recipient entry must be an object, got %#v", raw)
		}
		if len(recipientEntry) != 1 || recipientEntry["username"] == nil {
			t.Fatalf("recipient entry contains unexpected fields: %#v", recipientEntry)
		}
		if _, ok := recipientEntry["username"].(string); !ok {
			t.Fatalf("recipient username must be a string, got %#v", recipientEntry["username"])
		}
	}
}
