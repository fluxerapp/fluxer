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

const (
	requireVerifiedEmail = 1 << 0
	requireVerifiedPhone = 1 << 2
)

type suspiciousActivityErrorResponse struct {
	errorResponse
	Data struct {
		SuspiciousActivityFlags int `json:"suspicious_activity_flags"`
	} `json:"data"`
}

func TestAuthEmailVerificationOnlyClearsEmailRelatedSuspiciousFlags(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)
	clearTestEmails(t, client)

	updateUserSecurityFlags(t, client, account.UserID, userSecurityFlagsPayload{
		SuspiciousActivityFlagNames: []string{"REQUIRE_VERIFIED_EMAIL", "REQUIRE_VERIFIED_PHONE"},
	})

	checkSuspiciousFlags := func(expected int) {
		resp, err := client.getWithAuth("/users/@me", account.Token)
		if err != nil {
			t.Fatalf("failed to fetch /users/@me: %v", err)
		}
		assertStatus(t, resp, http.StatusForbidden)
		var errBody suspiciousActivityErrorResponse
		decodeJSONResponse(t, resp, &errBody)
		if errBody.Data.SuspiciousActivityFlags != expected {
			t.Fatalf("expected suspicious flags %d, got %d", expected, errBody.Data.SuspiciousActivityFlags)
		}
	}

	checkSuspiciousFlags(requireVerifiedEmail | requireVerifiedPhone)

	resp, err := client.postJSONWithAuth("/auth/verify/resend", nil, account.Token)
	if err != nil {
		t.Fatalf("failed to request verification email: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	email := waitForEmail(t, client, "email_verification", account.Email)
	token, ok := email.Metadata["token"]
	if !ok || token == "" {
		t.Fatalf("expected verification token in email metadata")
	}

	resp, err = client.postJSON("/auth/verify", map[string]string{"token": token})
	if err != nil {
		t.Fatalf("failed to verify email: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	checkSuspiciousFlags(requireVerifiedPhone)
}
