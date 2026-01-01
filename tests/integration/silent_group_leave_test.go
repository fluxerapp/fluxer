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
	"time"
)

func TestSilentGroupLeaveViaDeleteChannel(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)
	user3 := createTestAccount(t, client)

	user1Socket := newGatewayClient(t, client, user1.Token)
	t.Cleanup(user1Socket.Close)
	user2Socket := newGatewayClient(t, client, user2.Token)
	t.Cleanup(user2Socket.Close)
	user3Socket := newGatewayClient(t, client, user3.Token)
	t.Cleanup(user3Socket.Close)

	createFriendship(t, client, user1, user2)
	createFriendship(t, client, user1, user3)

	drainRelationshipEvents(t, user1Socket)
	drainRelationshipEvents(t, user2Socket)
	drainRelationshipEvents(t, user3Socket)

	groupDmChannel := createGroupDmChannel(t, client, user1.Token, user2.UserID, user3.UserID)
	waitForChannelEvent(t, user2Socket, "CHANNEL_CREATE", groupDmChannel.ID)
	waitForChannelEvent(t, user3Socket, "CHANNEL_CREATE", groupDmChannel.ID)

	waitForMessageEventByChannel(t, user1Socket, "MESSAGE_CREATE", groupDmChannel.ID)
	waitForMessageEventByChannel(t, user2Socket, "MESSAGE_CREATE", groupDmChannel.ID)
	waitForMessageEventByChannel(t, user3Socket, "MESSAGE_CREATE", groupDmChannel.ID)

	t.Run("silent leave does not create system message", func(t *testing.T) {
		resp, err := client.getWithAuth(fmt.Sprintf("/channels/%d/messages?limit=50", parseSnowflake(t, groupDmChannel.ID)), user2.Token)
		if err != nil {
			t.Fatalf("failed to get messages before leave: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)

		type messageResponse struct {
			ID string `json:"id"`
		}
		var messagesBefore []messageResponse
		decodeJSONResponse(t, resp, &messagesBefore)
		resp.Body.Close()

		resp, err = client.delete(fmt.Sprintf("/channels/%d?silent=true", parseSnowflake(t, groupDmChannel.ID)), user1.Token)
		if err != nil {
			t.Fatalf("failed to leave group silently: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()

		resp, err = client.getWithAuth(fmt.Sprintf("/channels/%d", parseSnowflake(t, groupDmChannel.ID)), user1.Token)
		if err != nil {
			t.Fatalf("failed to get channel request: %v", err)
		}
		if resp.StatusCode != http.StatusNotFound && resp.StatusCode != http.StatusForbidden {
			t.Fatalf("expected user1 to no longer have access to channel, got status %d", resp.StatusCode)
		}
		resp.Body.Close()

		time.Sleep(500 * time.Millisecond)

		resp, err = client.getWithAuth(fmt.Sprintf("/channels/%d/messages?limit=50", parseSnowflake(t, groupDmChannel.ID)), user2.Token)
		if err != nil {
			t.Fatalf("failed to get messages after leave: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)

		var messagesAfter []messageResponse
		decodeJSONResponse(t, resp, &messagesAfter)
		resp.Body.Close()

		if len(messagesAfter) != len(messagesBefore) {
			t.Fatalf("expected same number of messages before and after silent leave, got %d before, %d after", len(messagesBefore), len(messagesAfter))
		}

		resp, err = client.getWithAuth(fmt.Sprintf("/channels/%d", parseSnowflake(t, groupDmChannel.ID)), user2.Token)
		if err != nil {
			t.Fatalf("failed to get channel after leave: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)

		var channelResponse struct {
			Recipients []struct {
				ID string `json:"id"`
			} `json:"recipients"`
		}
		decodeJSONResponse(t, resp, &channelResponse)
		resp.Body.Close()

		user1Found := false
		for _, recipient := range channelResponse.Recipients {
			if recipient.ID == user1.UserID {
				user1Found = true
				break
			}
		}
		if user1Found {
			t.Fatalf("expected user1 to be removed from channel recipients")
		}
	})
}

func TestSilentGroupLeaveViaRemoveRecipient(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)
	user3 := createTestAccount(t, client)

	user1Socket := newGatewayClient(t, client, user1.Token)
	t.Cleanup(user1Socket.Close)
	user2Socket := newGatewayClient(t, client, user2.Token)
	t.Cleanup(user2Socket.Close)
	user3Socket := newGatewayClient(t, client, user3.Token)
	t.Cleanup(user3Socket.Close)

	createFriendship(t, client, user1, user2)
	createFriendship(t, client, user1, user3)

	drainRelationshipEvents(t, user1Socket)
	drainRelationshipEvents(t, user2Socket)
	drainRelationshipEvents(t, user3Socket)

	groupDmChannel := createGroupDmChannel(t, client, user1.Token, user2.UserID, user3.UserID)
	waitForChannelEvent(t, user1Socket, "CHANNEL_CREATE", groupDmChannel.ID)
	waitForChannelEvent(t, user2Socket, "CHANNEL_CREATE", groupDmChannel.ID)
	waitForChannelEvent(t, user3Socket, "CHANNEL_CREATE", groupDmChannel.ID)

	waitForMessageEventByChannel(t, user1Socket, "MESSAGE_CREATE", groupDmChannel.ID)
	waitForMessageEventByChannel(t, user2Socket, "MESSAGE_CREATE", groupDmChannel.ID)
	waitForMessageEventByChannel(t, user3Socket, "MESSAGE_CREATE", groupDmChannel.ID)

	t.Run("silent remove via recipients endpoint", func(t *testing.T) {
		resp, err := client.getWithAuth(fmt.Sprintf("/channels/%d/messages?limit=50", parseSnowflake(t, groupDmChannel.ID)), user1.Token)
		if err != nil {
			t.Fatalf("failed to get messages before remove: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)

		type messageResponse struct {
			ID string `json:"id"`
		}
		var messagesBefore []messageResponse
		decodeJSONResponse(t, resp, &messagesBefore)
		resp.Body.Close()

		resp, err = client.delete(fmt.Sprintf("/channels/%d/recipients/%s?silent=true", parseSnowflake(t, groupDmChannel.ID), user3.UserID), user1.Token)
		if err != nil {
			t.Fatalf("failed to remove recipient silently: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()

		resp, err = client.getWithAuth(fmt.Sprintf("/channels/%d", parseSnowflake(t, groupDmChannel.ID)), user3.Token)
		if err != nil {
			t.Fatalf("failed to get channel request: %v", err)
		}
		if resp.StatusCode != http.StatusNotFound && resp.StatusCode != http.StatusForbidden {
			t.Fatalf("expected user3 to no longer have access to channel, got status %d", resp.StatusCode)
		}
		resp.Body.Close()

		time.Sleep(500 * time.Millisecond)

		resp, err = client.getWithAuth(fmt.Sprintf("/channels/%d/messages?limit=50", parseSnowflake(t, groupDmChannel.ID)), user1.Token)
		if err != nil {
			t.Fatalf("failed to get messages after remove: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)

		var messagesAfter []messageResponse
		decodeJSONResponse(t, resp, &messagesAfter)
		resp.Body.Close()

		if len(messagesAfter) != len(messagesBefore) {
			t.Fatalf("expected same number of messages before and after silent remove, got %d before, %d after", len(messagesBefore), len(messagesAfter))
		}

		resp, err = client.getWithAuth(fmt.Sprintf("/channels/%d", parseSnowflake(t, groupDmChannel.ID)), user1.Token)
		if err != nil {
			t.Fatalf("failed to get channel after remove: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)

		var channelResponse struct {
			Recipients []struct {
				ID string `json:"id"`
			} `json:"recipients"`
		}
		decodeJSONResponse(t, resp, &channelResponse)
		resp.Body.Close()

		user3Found := false
		for _, recipient := range channelResponse.Recipients {
			if recipient.ID == user3.UserID {
				user3Found = true
				break
			}
		}
		if user3Found {
			t.Fatalf("expected user3 to be removed from channel recipients")
		}
	})
}
