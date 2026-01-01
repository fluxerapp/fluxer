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
	"encoding/json"
	"fmt"
	"net/http"
	"testing"
	"time"
)

func TestGatewayReadyUsersPartialAfterUserUpdate(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	guild := createGuild(t, client, owner.Token, "Ready Partial Leak Guild")
	invite := createChannelInvite(t, client, owner.Token, parseSnowflake(t, guild.SystemChannel))

	member := createTestAccount(t, client)
	joinGuild(t, client, member.Token, invite.Code)

	newBio := fmt.Sprintf("Private bio %d", time.Now().UnixNano())
	resp, err := client.patchJSONWithAuth("/users/@me", map[string]any{"bio": newBio}, owner.Token)
	if err != nil {
		t.Fatalf("failed to update owner bio: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	socket := newGatewayClient(t, client, member.Token)
	t.Cleanup(socket.Close)

	ready := socket.WaitForEvent(t, "READY", 30*time.Second, nil)

	var readyPayload struct {
		Users []map[string]any `json:"users"`
	}
	if err := json.Unmarshal(ready.Data, &readyPayload); err != nil {
		t.Fatalf("failed to decode READY payload: %v", err)
	}
	if len(readyPayload.Users) == 0 {
		t.Fatal("expected READY.users to include at least one user")
	}

	privateKeys := []string{
		"bio",
		"email",
		"phone",
		"mfa_enabled",
		"authenticator_types",
		"premium_type",
		"premium_since",
		"premium_until",
		"premium_will_cancel",
		"premium_billing_cycle",
		"premium_badge_hidden",
		"premium_badge_masked",
		"pending_bulk_message_deletion",
	}
	for _, user := range readyPayload.Users {
		userID, _ := user["id"].(string)
		for _, key := range privateKeys {
			if _, ok := user[key]; ok {
				t.Fatalf("READY user %s unexpectedly contains private key %s", userID, key)
			}
		}
	}
}
