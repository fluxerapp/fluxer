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

// TestWebhookPermissions tests webhook security
func TestWebhookPermissions(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Webhook Security %d", time.Now().UnixNano()))
	channelID := parseSnowflake(t, guild.SystemChannel)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	webhookPayload := map[string]string{"name": "Test Webhook"}
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/channels/%d/webhooks", channelID), webhookPayload, owner.Token)
	if err != nil {
		t.Fatalf("failed to create webhook: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var webhook struct {
		ID    string `json:"id"`
		Token string `json:"token"`
		Name  string `json:"name"`
	}
	decodeJSONResponse(t, resp, &webhook)

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/channels/%d/webhooks", channelID), webhookPayload, member.Token)
	if err != nil {
		t.Fatalf("failed to attempt webhook create: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 for webhook create without permission, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.getWithAuth(fmt.Sprintf("/channels/%d/webhooks", channelID), member.Token)
	if err != nil {
		t.Fatalf("failed to attempt webhook list: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 for webhook list without permission, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/webhooks/%s", webhook.ID), map[string]string{"name": "Hacked"}, member.Token)
	if err != nil {
		t.Fatalf("failed to attempt webhook modify: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden && resp.StatusCode != http.StatusNotFound {
		t.Fatalf("expected 403/404 for webhook modify without permission, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.delete(fmt.Sprintf("/webhooks/%s", webhook.ID), member.Token)
	if err != nil {
		t.Fatalf("failed to attempt webhook delete: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden && resp.StatusCode != http.StatusNotFound {
		t.Fatalf("expected 403/404 for webhook delete without permission, got %d", resp.StatusCode)
	}
	resp.Body.Close()
}
