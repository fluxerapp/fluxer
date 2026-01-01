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

const (
	guildChannelTypeText     = 0
	guildChannelTypeVoice    = 2
	guildChannelTypeCategory = 4
)

func createGuildChannelOfType(
	t testing.TB,
	client *testClient,
	token string,
	guildID int64,
	name string,
	channelType int,
) minimalChannelResponse {
	t.Helper()
	payload := map[string]any{
		"name": name,
		"type": channelType,
	}
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), payload, token)
	if err != nil {
		t.Fatalf("failed to create channel: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var channel minimalChannelResponse
	decodeJSONResponse(t, resp, &channel)
	return channel
}
