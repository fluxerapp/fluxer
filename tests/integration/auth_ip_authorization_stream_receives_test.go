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
	"bufio"
	"fmt"
	"net/http"
	"strings"
	"testing"
	"time"
)

// Ensures stream delivers one event then closes when publish happens.
func TestAuthIPAuthorizationStreamReceivesAndCloses(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	ticket := fmt.Sprintf("stream-%d", time.Now().UnixNano())
	token := fmt.Sprintf("token-%d", time.Now().UnixNano())

	seedIPAuthorizationTicket(t, client, ipAuthSeedPayload{
		Ticket:         ticket,
		Token:          token,
		UserID:         account.UserID,
		Email:          account.Email,
		Username:       "stream-user",
		ClientIP:       "192.0.2.10",
		UserAgent:      "IntegrationTest/1.0",
		ClientLocation: "Testland",
		CreatedAt:      time.Now().Add(-1 * time.Minute),
		TTLSeconds:     900,
	})

	resp, err := client.get(fmt.Sprintf("/auth/ip-authorization/stream?ticket=%s", ticket))
	if err != nil {
		t.Fatalf("failed to open stream: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("expected 200 from stream, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	time.Sleep(100 * time.Millisecond)

	_, err = client.postJSON("/test/auth/ip-authorization/publish", map[string]string{
		"ticket":  ticket,
		"token":   token,
		"user_id": account.UserID,
	})
	if err != nil {
		t.Fatalf("failed to publish ip auth event: %v", err)
	}

	reader := bufio.NewReader(resp.Body)
	foundData := false

	for {
		line, readErr := reader.ReadString('\n')
		if readErr != nil {
			break
		}
		if strings.HasPrefix(line, "data:") {
			if !strings.Contains(line, token) || !strings.Contains(line, account.UserID) {
				t.Fatalf("data line missing expected payload: %s", line)
			}
			foundData = true
		}
		if line == "\n" && foundData {
			break
		}
	}

	if !foundData {
		t.Fatalf("did not receive data event from stream")
	}

	resp.Body.Close()
}
