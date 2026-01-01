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

// Ensures resend fails when resendUsed flag is already set on the ticket.
func TestAuthIPAuthorizationResendAlreadyUsed(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	ticket := fmt.Sprintf("ticket-used-%d", time.Now().UnixNano())
	token := fmt.Sprintf("token-used-%d", time.Now().UnixNano())

	seedIPAuthorizationTicket(t, client, ipAuthSeedPayload{
		Ticket:         ticket,
		Token:          token,
		UserID:         account.UserID,
		Email:          account.Email,
		Username:       "resend-used-user",
		ClientIP:       "203.0.113.10",
		UserAgent:      "IntegrationTest/1.0",
		ClientLocation: "Testland",
		ResendUsed:     true,
		CreatedAt:      time.Now().Add(-2 * time.Minute),
		TTLSeconds:     900,
	})

	resp, err := client.postJSON("/auth/ip-authorization/resend", map[string]string{"ticket": ticket})
	if err != nil {
		t.Fatalf("failed to call ip-authorization resend: %v", err)
	}
	if resp.StatusCode != http.StatusTooManyRequests {
		t.Fatalf("expected resend to fail when already used, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()
}
