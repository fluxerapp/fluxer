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

func TestUserChannelLifecycleEndpoints(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	recipient := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	createFriendship(t, client, user, recipient)

	channel := createDmChannel(t, client, user.Token, parseSnowflake(t, recipient.UserID))

	resp, err := client.getWithAuth("/users/@me/channels", user.Token)
	if err != nil {
		t.Fatalf("failed to list DM channels: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var channels []minimalChannelResponse
	decodeJSONResponse(t, resp, &channels)
	found := false
	for _, ch := range channels {
		if ch.ID == channel.ID {
			found = true
			break
		}
	}
	if !found {
		t.Fatalf("expected created DM channel %s in list", channel.ID)
	}

	resp, err = client.putJSONWithAuth(fmt.Sprintf("/users/@me/channels/%d/pin", parseSnowflake(t, channel.ID)), nil, user.Token)
	if err != nil {
		t.Fatalf("failed to pin dm channel: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.delete(fmt.Sprintf("/users/@me/channels/%d/pin", parseSnowflake(t, channel.ID)), user.Token)
	if err != nil {
		t.Fatalf("failed to unpin dm channel: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	channelSnowflake := formatSnowflake(parseSnowflake(t, channel.ID))
	preloadPayload := map[string]any{"channels": []string{channelSnowflake}}
	resp, err = client.postJSONWithAuth("/users/@me/preload-messages", preloadPayload, user.Token)
	if err != nil {
		t.Fatalf("failed to preload messages: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var preload map[string]map[string]any
	decodeJSONResponse(t, resp, &preload)
	if _, ok := preload[channel.ID]; !ok {
		t.Fatalf("expected preload response for channel %s", channel.ID)
	}

	deletePayload := map[string]any{
		"channel_ids": []string{channelSnowflake},
		"password":    user.Password,
	}
	resp, err = client.postJSONWithAuth("/users/@me/messages/delete", deletePayload, user.Token)
	if err != nil {
		t.Fatalf("failed to request messages delete: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()
}
