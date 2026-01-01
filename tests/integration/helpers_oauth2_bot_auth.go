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

// authenticateWithBotToken authenticates using a bot token and returns the account details.
func authenticateWithBotToken(t testing.TB, client *testClient, botToken string) testAccount {
	t.Helper()
	req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me", client.baseURL), nil)
	if err != nil {
		t.Fatalf("failed to build bot auth request: %v", err)
	}
	req.Header.Set("Authorization", fmt.Sprintf("Bot %s", botToken))
	client.applyCommonHeaders(req)

	resp, err := client.httpClient.Do(req)
	if err != nil {
		t.Fatalf("bot auth request failed: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("bot auth failed with status %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var user struct {
		ID    string `json:"id"`
		Email string `json:"email"`
	}
	decodeJSONResponse(t, resp, &user)

	return testAccount{
		UserID: user.ID,
		Token:  botToken,
		Email:  user.Email,
	}
}
