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

// setBotFlag sets or clears the bot flag for a user
func setBotFlag(t testing.TB, client *testClient, userID string, isBot bool) {
	t.Helper()

	payload := map[string]any{
		"is_bot": isBot,
	}

	resp, err := client.postJSON(
		fmt.Sprintf("/test/users/%s/set-bot-flag", userID),
		payload,
	)
	if err != nil {
		t.Fatalf("failed to set bot flag: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Fatalf("expected status 200 when setting bot flag, got %d", resp.StatusCode)
	}
}
