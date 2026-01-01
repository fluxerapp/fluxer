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

// TestUnauthorizedGuildAccess attempts to access guilds without permission
func TestUnauthorizedGuildAccess(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	attacker := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Private Guild %d", time.Now().UnixNano()))
	guildID := parseSnowflake(t, guild.ID)

	resp, err := client.getWithAuth(fmt.Sprintf("/guilds/%d", guildID), attacker.Token)
	if err != nil {
		t.Fatalf("failed to attempt guild access: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden && resp.StatusCode != http.StatusNotFound {
		t.Fatalf("expected 403/404 for unauthorized guild access, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.getWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), attacker.Token)
	if err != nil {
		t.Fatalf("failed to attempt channel list: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden && resp.StatusCode != http.StatusNotFound {
		t.Fatalf("expected 403/404 for unauthorized channel list, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.getWithAuth(fmt.Sprintf("/guilds/%d/members", guildID), attacker.Token)
	if err != nil {
		t.Fatalf("failed to attempt member list: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden && resp.StatusCode != http.StatusNotFound {
		t.Fatalf("expected 403/404 for unauthorized member list, got %d", resp.StatusCode)
	}
	resp.Body.Close()
}
