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
	"net/url"
	"testing"
	"time"
)

func TestGatewayMessageReactions(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	ownerSocket := newGatewayClient(t, client, owner.Token)
	t.Cleanup(ownerSocket.Close)
	memberSocket := newGatewayClient(t, client, member.Token)
	t.Cleanup(memberSocket.Close)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Reactions Guild %d", time.Now().UnixNano()))
	channelID := parseSnowflake(t, guild.SystemChannel)
	invite := createChannelInvite(t, client, owner.Token, channelID)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	memberSocket.WaitForEvent(t, "GUILD_CREATE", 60*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			ID string `json:"id"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			return false
		}
		return payload.ID == guild.ID
	})

	message := sendChannelMessage(t, client, owner.Token, channelID, "react to me")
	emoji := "👍"

	resp, err = client.putWithAuth(fmt.Sprintf("/channels/%d/messages/%d/reactions/%s/@me", channelID, parseSnowflake(t, message.ID), url.PathEscape(emoji)), member.Token)
	if err != nil {
		t.Fatalf("failed to add reaction: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	waitForReactionAdd := func(socket *gatewayClient, userID string) {
		socket.WaitForEvent(t, "MESSAGE_REACTION_ADD", 30*time.Second, func(raw json.RawMessage) bool {
			var payload struct {
				MessageID string `json:"message_id"`
				ChannelID string `json:"channel_id"`
				UserID    string `json:"user_id"`
				Emoji     struct {
					Name string `json:"name"`
				} `json:"emoji"`
			}
			if err := json.Unmarshal(raw, &payload); err != nil {
				t.Fatalf("failed to decode reaction add: %v", err)
			}
			return payload.MessageID == message.ID && payload.ChannelID == guild.SystemChannel && payload.UserID == userID && payload.Emoji.Name == emoji
		})
	}

	waitForReactionAdd(ownerSocket, member.UserID)
	waitForReactionAdd(memberSocket, member.UserID)

	resp, err = client.putWithAuth(fmt.Sprintf("/channels/%d/messages/%d/reactions/%s/@me", channelID, parseSnowflake(t, message.ID), url.PathEscape(emoji)), owner.Token)
	if err != nil {
		t.Fatalf("failed to add owner reaction: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	waitForReactionAdd(ownerSocket, owner.UserID)
	waitForReactionAdd(memberSocket, owner.UserID)

	resp, err = client.delete(fmt.Sprintf("/channels/%d/messages/%d/reactions/%s/@me", channelID, parseSnowflake(t, message.ID), url.PathEscape(emoji)), member.Token)
	if err != nil {
		t.Fatalf("failed to remove reaction: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	waitForReactionRemove := func(socket *gatewayClient, userID string) {
		socket.WaitForEvent(t, "MESSAGE_REACTION_REMOVE", 30*time.Second, func(raw json.RawMessage) bool {
			var payload struct {
				MessageID string `json:"message_id"`
				ChannelID string `json:"channel_id"`
				UserID    string `json:"user_id"`
				Emoji     struct {
					Name string `json:"name"`
				} `json:"emoji"`
			}
			if err := json.Unmarshal(raw, &payload); err != nil {
				t.Fatalf("failed to decode reaction remove: %v", err)
			}
			return payload.MessageID == message.ID && payload.UserID == userID && payload.Emoji.Name == emoji
		})
	}

	waitForReactionRemove(ownerSocket, member.UserID)
	waitForReactionRemove(memberSocket, member.UserID)

	resp, err = client.delete(fmt.Sprintf("/channels/%d/messages/%d/reactions/%s", channelID, parseSnowflake(t, message.ID), url.PathEscape(emoji)), owner.Token)
	if err != nil {
		t.Fatalf("failed to remove all reactions for emoji: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	ownerSocket.WaitForEvent(t, "MESSAGE_REACTION_REMOVE_EMOJI", 30*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			MessageID string `json:"message_id"`
			ChannelID string `json:"channel_id"`
			Emoji     struct {
				Name string `json:"name"`
			} `json:"emoji"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode reaction remove emoji: %v", err)
		}
		return payload.MessageID == message.ID && payload.Emoji.Name == emoji
	})

	memberSocket.WaitForEvent(t, "MESSAGE_REACTION_REMOVE_EMOJI", 30*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			MessageID string `json:"message_id"`
			Emoji     struct {
				Name string `json:"name"`
			} `json:"emoji"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode reaction remove emoji: %v", err)
		}
		return payload.MessageID == message.ID && payload.Emoji.Name == emoji
	})

	resp, err = client.putWithAuth(fmt.Sprintf("/channels/%d/messages/%d/reactions/🎉/@me", channelID, parseSnowflake(t, message.ID)), owner.Token)
	if err != nil {
		t.Fatalf("failed to add another reaction: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	ownerSocket.WaitForEvent(t, "MESSAGE_REACTION_ADD", 30*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			MessageID string `json:"message_id"`
			Emoji     struct {
				Name string `json:"name"`
			} `json:"emoji"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			return false
		}
		return payload.MessageID == message.ID && payload.Emoji.Name == "🎉"
	})

	resp, err = client.delete(fmt.Sprintf("/channels/%d/messages/%d/reactions", channelID, parseSnowflake(t, message.ID)), owner.Token)
	if err != nil {
		t.Fatalf("failed to remove all reactions: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	ownerSocket.WaitForEvent(t, "MESSAGE_REACTION_REMOVE_ALL", 30*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			MessageID string `json:"message_id"`
			ChannelID string `json:"channel_id"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode reaction remove all: %v", err)
		}
		return payload.MessageID == message.ID && payload.ChannelID == guild.SystemChannel
	})

	memberSocket.WaitForEvent(t, "MESSAGE_REACTION_REMOVE_ALL", 30*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			MessageID string `json:"message_id"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode reaction remove all: %v", err)
		}
		return payload.MessageID == message.ID
	})
}
