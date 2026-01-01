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

func TestAuthEmailChangeResendCooldown(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)
	clearTestEmails(t, client)

	startResp, err := client.postJSONWithAuth("/users/@me/email-change/start", map[string]any{
		"password": account.Password,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to start email change: %v", err)
	}
	assertStatus(t, startResp, http.StatusOK)
	var start emailChangeStartResponse
	decodeJSONResponse(t, startResp, &start)
	startResp.Body.Close()

	// Get original proof - either from start response or by verifying original email
	var originalProof string
	if start.RequireOriginal {
		originalEmail := waitForEmail(t, client, "email_change_original", account.Email)
		originalCode := originalEmail.Metadata["code"]
		originalProof = verifyOriginalEmailChange(t, client, account, start.Ticket, originalCode, account.Password)
	} else {
		if start.OriginalProof == nil || *start.OriginalProof == "" {
			t.Fatalf("expected original_proof in start response")
		}
		originalProof = *start.OriginalProof
	}

	newEmail := fmt.Sprintf("cooldown-%d@example.com", time.Now().UnixNano())
	reqNewResp, err := client.postJSONWithAuth("/users/@me/email-change/request-new", map[string]any{
		"ticket":         start.Ticket,
		"new_email":      newEmail,
		"original_proof": originalProof,
		"password":       account.Password,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to request new email: %v", err)
	}
	assertStatus(t, reqNewResp, http.StatusOK)
	reqNewResp.Body.Close()

	resendResp, err := client.postJSONWithAuth("/users/@me/email-change/resend-new", map[string]any{
		"ticket": start.Ticket,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to attempt immediate resend: %v", err)
	}
	if resendResp.StatusCode != http.StatusTooManyRequests && resendResp.StatusCode != http.StatusBadRequest {
		t.Fatalf("expected resend to be rate limited, got %d: %s", resendResp.StatusCode, readResponseBody(resendResp))
	}
	resendResp.Body.Close()

	time.Sleep(31 * time.Second)

	resendResp, err = client.postJSONWithAuth("/users/@me/email-change/resend-new", map[string]any{
		"ticket": start.Ticket,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to resend after cooldown: %v", err)
	}
	assertStatus(t, resendResp, http.StatusNoContent)
	resendResp.Body.Close()
}
