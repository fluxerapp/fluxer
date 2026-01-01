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
	"time"
)

func TestLegacyHealDoesNotRegressLastMessage(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	guild := createGuild(t, client, account.Token, "Legacy Heal State Test")
	channelID := parseSnowflake(t, guild.SystemChannel)
	clearChannelMessages(t, client, channelID, account.Token)

	timestamps := []time.Time{
		time.Now().Add(-time.Hour * 24 * 40),
		time.Now().Add(-time.Hour * 24 * 30),
		time.Now().Add(-time.Hour * 24 * 20),
		time.Now().Add(-time.Hour * 24 * 10),
		time.Now(),
	}

	seed := seedMessagesRaw(t, client, channelID, timestamps, account.UserID)

	resp, err := client.getWithAuth(
		fmt.Sprintf("/channels/%d/messages?limit=50", channelID),
		account.Token,
	)
	if err != nil {
		t.Fatalf("fetch failed: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	state := getChannelState(t, client, channelID, account.Token)
	if state.LastMessageID == nil {
		t.Fatalf("expected last_message_id to be set")
	}

	if *state.LastMessageID != seed.Messages[len(seed.Messages)-1].MessageID {
		t.Fatalf("last_message_id regressed during legacy heal")
	}
}
