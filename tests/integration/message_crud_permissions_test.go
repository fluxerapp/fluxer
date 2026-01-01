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

func TestMessageCRUDPermissions(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	ensureSessionStarted(t, client, owner.Token)
	ensureSessionStarted(t, client, member.Token)

	guild := createGuild(t, client, owner.Token, "Message Perms Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	memberMsgPayload := map[string]string{"content": "Member message"}
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages", channelID), memberMsgPayload, member.Token)
	if err != nil {
		t.Fatalf("failed to send message: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var memberMsg struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &memberMsg)
	memberMsgID := parseSnowflake(t, memberMsg.ID)

	ownerMsgPayload := map[string]string{"content": "Owner message"}
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages", channelID), ownerMsgPayload, owner.Token)
	if err != nil {
		t.Fatalf("failed to send message: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var ownerMsg struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &ownerMsg)
	ownerMsgID := parseSnowflake(t, ownerMsg.ID)

	t.Run("member can read messages", func(t *testing.T) {
		resp, err := client.getWithAuth(fmt.Sprintf("/channels/%d/messages", channelID), member.Token)
		if err != nil {
			t.Fatalf("failed to get messages: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("member can send messages", func(t *testing.T) {
		payload := map[string]string{"content": "Another message"}
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages", channelID), payload, member.Token)
		if err != nil {
			t.Fatalf("failed to send message: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("member can edit own message", func(t *testing.T) {
		payload := map[string]string{"content": "Edited by member"}
		resp, err := client.patchJSONWithAuth(fmt.Sprintf("/channels/%d/messages/%d", channelID, memberMsgID), payload, member.Token)
		if err != nil {
			t.Fatalf("failed to edit message: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("member cannot edit others' message", func(t *testing.T) {
		payload := map[string]string{"content": "Hacked"}
		resp, err := client.patchJSONWithAuth(fmt.Sprintf("/channels/%d/messages/%d", channelID, ownerMsgID), payload, member.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusForbidden)
		resp.Body.Close()
	})

	t.Run("member can delete own message", func(t *testing.T) {
		resp, err := client.delete(fmt.Sprintf("/channels/%d/messages/%d", channelID, memberMsgID), member.Token)
		if err != nil {
			t.Fatalf("failed to delete message: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})

	t.Run("owner with MANAGE_MESSAGES can delete any message", func(t *testing.T) {
		payload := map[string]string{"content": "To be deleted"}
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages", channelID), payload, member.Token)
		if err != nil {
			t.Fatalf("failed to send message: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		var msg struct {
			ID string `json:"id"`
		}
		decodeJSONResponse(t, resp, &msg)
		msgID := parseSnowflake(t, msg.ID)

		resp, err = client.delete(fmt.Sprintf("/channels/%d/messages/%d", channelID, msgID), owner.Token)
		if err != nil {
			t.Fatalf("failed to delete message: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})
}
