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

func TestAuthUnclaimedClaimFlow(t *testing.T) {
	client := newTestClient(t)

	account := createTestAccount(t, client)
	clearTestEmails(t, client)
	unclaimAccount(t, client, account.UserID)

	t.Run("unclaimed users cannot change tag or settings", func(t *testing.T) {
		resp, err := client.patchJSONWithAuth("/users/@me", map[string]any{
			"username": fmt.Sprintf("forbidden-%d", time.Now().UnixNano()),
		}, account.Token)
		if err != nil {
			t.Fatalf("failed to attempt username change: %v", err)
		}
		defer resp.Body.Close()
		if resp.StatusCode == http.StatusOK {
			t.Fatalf("expected username change to be rejected for unclaimed user")
		}
	})

	t.Run("unclaimed users claim via email code and password", func(t *testing.T) {
		startResp, err := client.postJSONWithAuth("/users/@me/email-change/start", map[string]any{}, account.Token)
		if err != nil {
			t.Fatalf("failed to start email change: %v", err)
		}
		assertStatus(t, startResp, http.StatusOK)
		var start emailChangeStartResponse
		decodeJSONResponse(t, startResp, &start)
		startResp.Body.Close()

		if start.RequireOriginal {
			t.Fatalf("unclaimed users should not require original email verification")
		}
		if start.OriginalProof == nil || *start.OriginalProof == "" {
			t.Fatalf("expected original_proof in start response for unclaimed user")
		}
		originalProof := *start.OriginalProof

		newEmail := fmt.Sprintf("integration-claim-%d@example.com", time.Now().UnixNano())
		reqNewResp, err := client.postJSONWithAuth("/users/@me/email-change/request-new", map[string]any{
			"ticket":         start.Ticket,
			"new_email":      newEmail,
			"original_proof": originalProof,
		}, account.Token)
		if err != nil {
			t.Fatalf("failed to request new email: %v", err)
		}
		assertStatus(t, reqNewResp, http.StatusOK)
		var reqNew emailChangeRequestNewResponse
		decodeJSONResponse(t, reqNewResp, &reqNew)
		reqNewResp.Body.Close()

		newEmailData := waitForEmail(t, client, "email_change_new", newEmail)
		newCode := newEmailData.Metadata["code"]
		if newCode == "" {
			t.Fatalf("expected verification code in email metadata")
		}

		verifyNewResp, err := client.postJSONWithAuth("/users/@me/email-change/verify-new", map[string]any{
			"ticket":         start.Ticket,
			"code":           newCode,
			"original_proof": originalProof,
		}, account.Token)
		if err != nil {
			t.Fatalf("failed to verify new email: %v", err)
		}
		assertStatus(t, verifyNewResp, http.StatusOK)
		var verify emailChangeVerifyNewResponse
		decodeJSONResponse(t, verifyNewResp, &verify)
		verifyNewResp.Body.Close()

		newPassword := uniquePassword()
		finalResp, err := client.patchJSONWithAuth("/users/@me", map[string]any{
			"email_token":  verify.EmailToken,
			"new_password": newPassword,
		}, account.Token)
		if err != nil {
			t.Fatalf("failed to finalize claim: %v", err)
		}
		assertStatus(t, finalResp, http.StatusOK)
		var updated struct {
			Email                 string  `json:"email"`
			PasswordLastChangedAt *string `json:"password_last_changed_at"`
		}
		decodeJSONResponse(t, finalResp, &updated)
		finalResp.Body.Close()

		if updated.Email != newEmail {
			t.Fatalf("expected email to update to %s, got %s", newEmail, updated.Email)
		}
		if updated.PasswordLastChangedAt == nil || *updated.PasswordLastChangedAt == "" {
			t.Fatalf("expected password to be set during claim")
		}

		getResp, err := client.getWithAuth("/users/@me", account.Token)
		if err != nil {
			t.Fatalf("failed to check session after claim: %v", err)
		}
		assertStatus(t, getResp, http.StatusOK)
		var me userPrivateResponse
		decodeJSONResponse(t, getResp, &me)
		getResp.Body.Close()

		if me.Email != newEmail {
			t.Fatalf("expected /users/@me to reflect new email, got %s", me.Email)
		}
		if !me.Verified {
			t.Fatalf("expected email to be verified after claim flow")
		}
	})
}
