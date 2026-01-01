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

type privateChannelEntry struct {
	ChannelID     string `json:"channel_id"`
	LastMessageID string `json:"last_message_id"`
}

type privateChannelSeedResult struct {
	DMs      []privateChannelEntry `json:"dms"`
	GroupDMs []privateChannelEntry `json:"group_dms"`
}

func seedPrivateChannels(t testing.TB, client *testClient, user testAccount, payload map[string]any) privateChannelSeedResult {
	t.Helper()

	resp, err := client.postJSON(fmt.Sprintf("/test/users/%s/private-channels", user.UserID), payload)
	if err != nil {
		t.Fatalf("failed to seed private channels: %v", err)
	}
	defer resp.Body.Close()

	assertStatus(t, resp, http.StatusOK)
	var result privateChannelSeedResult
	decodeJSONResponse(t, resp, &result)
	return result
}
