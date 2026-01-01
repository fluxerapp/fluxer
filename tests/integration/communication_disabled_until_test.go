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
	"strings"
	"testing"
	"time"
)

func TestCommunicationDisabledUntilReactionsAndVoice(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)
	ensureSessionStarted(t, client, owner.Token)
	ensureSessionStarted(t, client, member.Token)

	guild := createGuild(t, client, owner.Token, "Communication Disabled Guild")
	guildID := parseSnowflake(t, guild.ID)
	channelID := parseSnowflake(t, guild.SystemChannel)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	message := sendChannelMessage(t, client, owner.Token, channelID, "timed out reaction test")

	utcTimeout := time.Now().UTC().Add(time.Hour).Format(time.RFC3339)
	resp, err = client.patchJSONWithAuth(
		fmt.Sprintf("/guilds/%d/members/%s", guildID, member.UserID),
		map[string]any{"communication_disabled_until": utcTimeout},
		owner.Token,
	)
	if err != nil {
		t.Fatalf("failed to set communication timeout: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	messageID := parseSnowflake(t, message.ID)

	resp, err = client.putWithAuth(
		fmt.Sprintf("/channels/%d/messages/%d/reactions/👍/@me", channelID, messageID),
		member.Token,
	)
	if err != nil {
		t.Fatalf("failed to add reaction as timed-out member: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 when adding reaction while timed out, got %d", resp.StatusCode)
	}
	var errorBody struct {
		Code string `json:"code"`
	}
	decodeJSONResponse(t, resp, &errorBody)
	if errorBody.Code != "COMMUNICATION_DISABLED" {
		t.Fatalf("expected COMMUNICATION_DISABLED error, got %q", errorBody.Code)
	}

	resp, err = client.putWithAuth(
		fmt.Sprintf("/channels/%d/messages/%d/reactions/👍/@me", channelID, messageID),
		owner.Token,
	)
	if err != nil {
		t.Fatalf("owner failed to add base reaction: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.putWithAuth(
		fmt.Sprintf("/channels/%d/messages/%d/reactions/👍/@me", channelID, messageID),
		member.Token,
	)
	if err != nil {
		t.Fatalf("timed-out member failed to stack existing reaction: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	var voiceChannel minimalChannelResponse
	resp, err = client.postJSONWithAuth(
		fmt.Sprintf("/guilds/%d/channels", guildID),
		map[string]any{"name": "timeout-voice", "type": 2},
		owner.Token,
	)
	if err != nil {
		t.Fatalf("failed to create voice channel: %v", err)
	}
	decodeJSONResponse(t, resp, &voiceChannel)

	memberGateway := newGatewayClient(t, client, member.Token)
	defer memberGateway.Close()

	memberGateway.SendVoiceStateUpdate(&guild.ID, &voiceChannel.ID, nil, false, false, false, false)

	select {
	case err := <-memberGateway.errCh:
		if !strings.Contains(err.Error(), "VOICE_MEMBER_TIMED_OUT") {
			t.Fatalf("expected VOICE_MEMBER_TIMED_OUT error, got %v", err)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("timed out waiting for gateway error while joining voice")
	}
}
