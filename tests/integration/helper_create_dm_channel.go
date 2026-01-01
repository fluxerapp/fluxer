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
	"net/http"
	"testing"
)

func createDmChannel(t testing.TB, client *testClient, token string, recipientID int64) minimalChannelResponse {
	t.Helper()
	payload := map[string]string{"recipient_id": formatSnowflake(recipientID)}
	resp, err := client.postJSONWithAuth("/users/@me/channels", payload, token)
	if err != nil {
		t.Fatalf("failed to create DM channel: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var channel minimalChannelResponse
	decodeJSONResponse(t, resp, &channel)
	if channel.ID == "" {
		t.Fatalf("dm channel response missing id")
	}
	return channel
}

func createGroupDmChannel(t testing.TB, client *testClient, token string, recipientIDs ...string) minimalChannelResponse {
	t.Helper()
	if len(recipientIDs) == 0 {
		t.Fatalf("createGroupDmChannel requires at least one recipient")
	}

	payload := map[string]any{"recipients": recipientIDs}
	resp, err := client.postJSONWithAuth("/users/@me/channels", payload, token)
	if err != nil {
		t.Fatalf("failed to create group DM channel: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var channel minimalChannelResponse
	decodeJSONResponse(t, resp, &channel)
	if channel.ID == "" {
		t.Fatalf("group DM channel response missing id")
	}
	return channel
}
