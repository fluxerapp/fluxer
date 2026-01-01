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

// resetBotToken resets the bot token for an OAuth2 application.
func resetBotToken(t testing.TB, client *testClient, owner testAccount, applicationID string) string {
	t.Helper()
	payload := map[string]any{
		"password": owner.Password,
	}
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/oauth2/applications/%s/bot/reset-token", applicationID), payload, owner.Token)
	if err != nil {
		t.Fatalf("failed to reset bot token: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("reset bot token failed: %s", readResponseBody(resp))
	}
	var result struct {
		Token string `json:"token"`
	}
	decodeJSONResponse(t, resp, &result)
	if result.Token == "" {
		t.Fatalf("reset bot token response missing token")
	}
	return result.Token
}
