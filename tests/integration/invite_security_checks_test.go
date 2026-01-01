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

// TestInviteSecurityChecks tests invite system security
func TestInviteSecurityChecks(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	attacker := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Invite Security %d", time.Now().UnixNano()))
	channelID := parseSnowflake(t, guild.SystemChannel)

	invite := createChannelInvite(t, client, owner.Token, channelID)

	resp, err := client.get(fmt.Sprintf("/invites/%s", invite.Code))
	if err != nil {
		t.Fatalf("failed to get invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.delete(fmt.Sprintf("/invites/%s", invite.Code), attacker.Token)
	if err != nil {
		t.Fatalf("failed to attempt invite delete: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden && resp.StatusCode != http.StatusNotFound {
		t.Fatalf("expected 403/404 for deleting others' invite, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.delete(fmt.Sprintf("/invites/%s", invite.Code), owner.Token)
	if err != nil {
		t.Fatalf("failed to delete own invite: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.get(fmt.Sprintf("/invites/%s", invite.Code))
	if err != nil {
		t.Fatalf("failed to check deleted invite: %v", err)
	}
	if resp.StatusCode != http.StatusNotFound {
		t.Fatalf("expected 404 for deleted invite, got %d", resp.StatusCode)
	}
	resp.Body.Close()
}
