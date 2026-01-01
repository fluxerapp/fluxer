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

func TestWebhookExecution(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	ensureSessionStarted(t, client, owner.Token)

	guild := createGuild(t, client, owner.Token, "Webhook Exec Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	webhookPayload := map[string]string{"name": "Test Webhook"}
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/webhooks", channelID), webhookPayload, owner.Token)
	if err != nil {
		t.Fatalf("failed to create webhook: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var webhook struct {
		ID    string `json:"id"`
		Token string `json:"token"`
	}
	decodeJSONResponse(t, resp, &webhook)

	t.Run("execute webhook without wait returns 204", func(t *testing.T) {
		payload := map[string]string{"content": "Hello from webhook!"}
		resp, err := client.postJSON(fmt.Sprintf("/webhooks/%s/%s", webhook.ID, webhook.Token), payload)
		if err != nil {
			t.Fatalf("failed to execute webhook: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})

	t.Run("execute webhook with wait=true returns message", func(t *testing.T) {
		payload := map[string]any{
			"content":  "Custom user",
			"username": "Custom Bot",
		}
		resp, err := client.postJSON(fmt.Sprintf("/webhooks/%s/%s?wait=true", webhook.ID, webhook.Token), payload)
		if err != nil {
			t.Fatalf("failed to execute webhook: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		var message struct {
			ID      string `json:"id"`
			Content string `json:"content"`
		}
		decodeJSONResponse(t, resp, &message)
		if message.Content != "Custom user" {
			t.Errorf("expected content 'Custom user', got '%s'", message.Content)
		}
	})

	t.Run("reject webhook execution without content", func(t *testing.T) {
		payload := map[string]string{}
		resp, err := client.postJSON(fmt.Sprintf("/webhooks/%s/%s", webhook.ID, webhook.Token), payload)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusBadRequest)
		resp.Body.Close()
	})

	t.Run("reject webhook execution with invalid token", func(t *testing.T) {
		payload := map[string]string{"content": "Test"}
		resp, err := client.postJSON(fmt.Sprintf("/webhooks/%s/invalid_token", webhook.ID), payload)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusNotFound)
		resp.Body.Close()
	})

	resp, err = client.delete(fmt.Sprintf("/webhooks/%s/%s", webhook.ID, webhook.Token), "")
	if err != nil {
		t.Fatalf("failed to delete webhook: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()
}
