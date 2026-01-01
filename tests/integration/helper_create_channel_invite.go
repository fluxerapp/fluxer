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

func createChannelInvite(t testing.TB, client *testClient, token string, channelID int64) inviteResponse {
	t.Helper()
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/invites", channelID), map[string]any{}, token)
	if err != nil {
		t.Fatalf("failed to create invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var invite inviteResponse
	decodeJSONResponse(t, resp, &invite)
	if invite.Code == "" {
		t.Fatalf("expected invite code in response")
	}
	return invite
}
