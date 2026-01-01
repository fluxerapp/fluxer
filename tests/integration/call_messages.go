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
	"testing"
)

// Helper function to get channel messages
func getChannelMessages(t *testing.T, client *testClient, token string, channelID int64, limit int) []callMessageResponse {
	t.Helper()

	resp, err := client.getWithAuth(fmt.Sprintf("/channels/%d/messages?limit=%d", channelID, limit), token)
	if err != nil {
		t.Fatalf("failed to get channel messages: %v", err)
	}

	var messages []callMessageResponse
	decodeJSONResponse(t, resp, &messages)
	return messages
}
