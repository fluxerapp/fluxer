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

func TestGatewayWebhooks(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	ownerSocket := newGatewayClient(t, client, owner.Token)
	t.Cleanup(ownerSocket.Close)
	memberSocket := newGatewayClient(t, client, member.Token)
	t.Cleanup(memberSocket.Close)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Webhook Guild %d", time.Now().UnixNano()))
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

	webhookPayload := map[string]string{"name": "Test Webhook"}
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/channels/%d/webhooks", channelID), webhookPayload, owner.Token)
	if err != nil {
		t.Fatalf("failed to create webhook: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var webhook struct {
		ID   string `json:"id"`
		Name string `json:"name"`
	}
	decodeJSONResponse(t, resp, &webhook)

	waitForWebhooksUpdate := func(socket *gatewayClient) {
		socket.WaitForEvent(t, "WEBHOOKS_UPDATE", 30*time.Second, func(raw json.RawMessage) bool {
			var payload struct {
				ChannelID string `json:"channel_id"`
				GuildID   string `json:"guild_id"`
			}
			if err := json.Unmarshal(raw, &payload); err != nil {
				t.Fatalf("failed to decode webhooks update: %v", err)
			}
			return payload.ChannelID == guild.SystemChannel && payload.GuildID == guild.ID
		})
	}

	waitForWebhooksUpdate(ownerSocket)
	waitForWebhooksUpdate(memberSocket)

	updatePayload := map[string]string{"name": "Updated Webhook"}
	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/webhooks/%s", webhook.ID), updatePayload, owner.Token)
	if err != nil {
		t.Fatalf("failed to update webhook: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	waitForWebhooksUpdate(ownerSocket)
	waitForWebhooksUpdate(memberSocket)

	resp, err = client.delete(fmt.Sprintf("/webhooks/%s", webhook.ID), owner.Token)
	if err != nil {
		t.Fatalf("failed to delete webhook: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	waitForWebhooksUpdate(ownerSocket)
	waitForWebhooksUpdate(memberSocket)
}
