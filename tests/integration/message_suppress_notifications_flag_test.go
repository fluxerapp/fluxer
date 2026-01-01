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

func TestMessageSendSuppressNotificationsFlag(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	ensureSessionStarted(t, client, owner.Token)

	guild := createGuild(t, client, owner.Token, "Suppress Notifications Flag Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	const suppressNotificationsFlag = 1 << 12

	payload := map[string]any{
		"content": "Quiet message",
		"flags":   suppressNotificationsFlag,
	}

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages", channelID), payload, owner.Token)
	if err != nil {
		t.Fatalf("failed to send message: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	defer resp.Body.Close()

	var message struct {
		Flags int `json:"flags"`
	}
	decodeJSONResponse(t, resp, &message)

	if message.Flags&suppressNotificationsFlag == 0 {
		t.Fatalf("expected suppress notifications flag to be set, got %d", message.Flags)
	}
}
