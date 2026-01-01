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

func seedIPAuthorizationTicket(t testing.TB, client *testClient, payload ipAuthSeedPayload) {
	t.Helper()

	body := map[string]any{
		"ticket":          payload.Ticket,
		"token":           payload.Token,
		"user_id":         payload.UserID,
		"email":           payload.Email,
		"username":        payload.Username,
		"client_ip":       payload.ClientIP,
		"user_agent":      payload.UserAgent,
		"client_location": payload.ClientLocation,
		"resend_used":     payload.ResendUsed,
		"created_at":      payload.CreatedAt.UnixMilli(),
		"ttl_seconds":     payload.TTLSeconds,
	}

	if payload.Platform != nil {
		body["platform"] = *payload.Platform
	}
	if payload.InviteCode != nil {
		body["invite_code"] = *payload.InviteCode
	}

	resp, err := client.postJSON("/test/auth/ip-authorization", body)
	if err != nil {
		t.Fatalf("failed to seed ip authorization ticket: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("seed ip authorization returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()
}
