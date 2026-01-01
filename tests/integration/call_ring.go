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

// Helper function to ring call recipients
func ringCall(t *testing.T, client *testClient, token string, channelID int64, recipients []int64) {
	t.Helper()

	body := map[string]any{}
	if len(recipients) > 0 {
		recipientStrs := make([]string, len(recipients))
		for i, r := range recipients {
			recipientStrs[i] = fmt.Sprintf("%d", r)
		}
		body["recipients"] = recipientStrs
	}

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/call/ring", channelID), body, token)
	if err != nil {
		t.Fatalf("failed to ring call: %v", err)
	}
	if resp.StatusCode != 204 {
		t.Fatalf("expected status 204 for ring call, got %d", resp.StatusCode)
	}
}
