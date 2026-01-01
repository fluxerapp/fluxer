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

func TestWebhookValidation(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	ensureSessionStarted(t, client, owner.Token)

	guild := createGuild(t, client, owner.Token, "Webhook Validation Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	t.Run("reject webhook creation without name", func(t *testing.T) {
		payload := map[string]string{}
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/webhooks", channelID), payload, owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusBadRequest)
		resp.Body.Close()
	})

	t.Run("reject webhook creation with invalid avatar", func(t *testing.T) {
		payload := map[string]string{
			"name":   "Test",
			"avatar": "invalid-base64",
		}
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/webhooks", channelID), payload, owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusBadRequest)
		resp.Body.Close()
	})

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

	t.Run("reject getting nonexistent webhook", func(t *testing.T) {
		resp, err := client.getWithAuth("/webhooks/999999999999999999", owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusNotFound)
		resp.Body.Close()
	})

	t.Run("reject updating nonexistent webhook", func(t *testing.T) {
		payload := map[string]string{"name": "New Name"}
		resp, err := client.patchJSONWithAuth("/webhooks/999999999999999999", payload, owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusNotFound)
		resp.Body.Close()
	})

	t.Run("reject deleting nonexistent webhook", func(t *testing.T) {
		resp, err := client.delete("/webhooks/999999999999999999", owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusNotFound)
		resp.Body.Close()
	})

	t.Run("accept webhook operations by token", func(t *testing.T) {
		resp, err := client.get(fmt.Sprintf("/webhooks/%s/%s", webhook.ID, webhook.Token))
		if err != nil {
			t.Fatalf("failed to get webhook by token: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()

		updatePayload := map[string]string{"name": "Updated by Token"}
		resp, err = client.patchJSONWithAuth(fmt.Sprintf("/webhooks/%s/%s", webhook.ID, webhook.Token), updatePayload, "")
		if err != nil {
			t.Fatalf("failed to update webhook by token: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()

		resp, err = client.delete(fmt.Sprintf("/webhooks/%s/%s", webhook.ID, webhook.Token), "")
		if err != nil {
			t.Fatalf("failed to delete webhook by token: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})
}
