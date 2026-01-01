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

// TestBotGatewayConnect verifies that bot tokens can successfully connect to the gateway.
// Unlike OAuth2 access tokens which cannot connect to the gateway, bot tokens are
// specifically designed for persistent WebSocket connections in this platform.
func TestBotGatewayConnect(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	appName := fmt.Sprintf("Gateway Bot %d", time.Now().UnixNano())
	redirectURI := "https://example.com/callback"
	_, botUserID, botToken := createOAuth2BotApplication(t, client, owner, appName, []string{redirectURI})

	req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/gateway/bot", client.baseURL), nil)
	if err != nil {
		t.Fatalf("failed to build gateway request: %v", err)
	}
	req.Header.Set("Authorization", fmt.Sprintf("Bot %s", botToken))
	client.applyCommonHeaders(req)

	resp, err := client.httpClient.Do(req)
	if err != nil {
		t.Fatalf("gateway bot request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK && resp.StatusCode != http.StatusNotFound {
		t.Fatalf("get gateway bot failed with status %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	if resp.StatusCode == http.StatusOK {
		var gatewayInfo map[string]any
		decodeJSONResponse(t, resp, &gatewayInfo)

		if gatewayURL, ok := gatewayInfo["url"].(string); ok && gatewayURL != "" {
			t.Logf("Gateway URL: %s", gatewayURL)
		}

		if shards, ok := gatewayInfo["shards"]; ok {
			t.Logf("Recommended shards: %v", shards)
		}
	}

	gc := newGatewayClient(t, client, botToken)
	defer gc.Close()

	t.Logf("Bot successfully connected to gateway")

	sessionID := gc.SessionID()
	if sessionID == "" {
		t.Fatalf("gateway client should have session ID after READY")
	}

	sequence := gc.Sequence()
	t.Logf("Gateway session established - ID: %s, Sequence: %d", sessionID, sequence)

	t.Logf("Bot ID %s successfully authenticated via gateway", botUserID)
}
