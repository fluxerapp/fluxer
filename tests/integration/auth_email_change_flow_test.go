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

func TestAuthEmailChangeFlow(t *testing.T) {
	client := newTestClient(t)

	t.Run("email change uses ticketed dual-code flow with sudo and proof token", func(t *testing.T) {
		account := createTestAccount(t, client)
		clearTestEmails(t, client)

		startResp := startEmailChange(t, client, account, account.Password)

		var originalProof string
		if startResp.RequireOriginal {
			originalEmail := waitForEmail(t, client, "email_change_original", account.Email)
			originalCode := originalEmail.Metadata["code"]
			if originalCode == "" {
				t.Fatalf("expected original verification code in email metadata")
			}
			originalProof = verifyOriginalEmailChange(t, client, account, startResp.Ticket, originalCode, account.Password)
		} else {
			if startResp.OriginalProof == nil || *startResp.OriginalProof == "" {
				t.Fatalf("expected original_proof in start response for unverified account")
			}
			originalProof = *startResp.OriginalProof
		}

		newEmail := fmt.Sprintf("integration-new-%d@example.com", time.Now().UnixNano())
		newReq := requestNewEmailChange(t, client, account, startResp.Ticket, newEmail, originalProof, account.Password)
		if newReq.NewEmail != newEmail {
			t.Fatalf("expected new email %s, got %s", newEmail, newReq.NewEmail)
		}

		newEmailData := waitForEmail(t, client, "email_change_new", newEmail)
		newCode := newEmailData.Metadata["code"]
		if newCode == "" {
			t.Fatalf("expected new email verification code in metadata")
		}

		token := verifyNewEmailChange(t, client, account, startResp.Ticket, newCode, originalProof, account.Password)

		resp, err := client.patchJSONWithAuth("/users/@me", map[string]any{
			"email_token": token,
			"password":    account.Password,
		}, account.Token)
		if err != nil {
			t.Fatalf("failed to finalize email change: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		var updated userPrivateResponse
		decodeJSONResponse(t, resp, &updated)
		resp.Body.Close()

		if updated.Email != newEmail {
			t.Fatalf("expected email to be updated to %s, got %s", newEmail, updated.Email)
		}
	})

	t.Run("direct email field is rejected", func(t *testing.T) {
		account := createTestAccount(t, client)
		clearTestEmails(t, client)

		newEmail := fmt.Sprintf("integration-direct-%d@example.com", time.Now().UnixNano())
		resp, err := client.patchJSONWithAuth("/users/@me", map[string]any{
			"email":    newEmail,
			"password": account.Password,
		}, account.Token)
		if err != nil {
			t.Fatalf("failed to attempt direct email change: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK {
			t.Fatalf("expected direct email change to be rejected, got 200 OK")
		}
	})

	t.Run("request-new fails without original_proof", func(t *testing.T) {
		account := createTestAccount(t, client)
		clearTestEmails(t, client)

		startResp := startEmailChange(t, client, account, account.Password)

		newEmail := fmt.Sprintf("integration-no-proof-%d@example.com", time.Now().UnixNano())
		resp, err := client.postJSONWithAuth("/users/@me/email-change/request-new", map[string]any{
			"ticket":         startResp.Ticket,
			"new_email":      newEmail,
			"original_proof": "invalid-proof-token",
			"password":       account.Password,
		}, account.Token)
		if err != nil {
			t.Fatalf("failed to send request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK {
			t.Fatalf("expected request-new to fail with invalid original_proof")
		}
	})

	t.Run("verify-new fails without original_proof", func(t *testing.T) {
		account := createTestAccount(t, client)
		clearTestEmails(t, client)

		startResp := startEmailChange(t, client, account, account.Password)

		var originalProof string
		if startResp.RequireOriginal {
			originalEmail := waitForEmail(t, client, "email_change_original", account.Email)
			originalCode := originalEmail.Metadata["code"]
			originalProof = verifyOriginalEmailChange(t, client, account, startResp.Ticket, originalCode, account.Password)
		} else {
			originalProof = *startResp.OriginalProof
		}

		newEmail := fmt.Sprintf("integration-verify-no-proof-%d@example.com", time.Now().UnixNano())
		requestNewEmailChange(t, client, account, startResp.Ticket, newEmail, originalProof, account.Password)

		newEmailData := waitForEmail(t, client, "email_change_new", newEmail)
		newCode := newEmailData.Metadata["code"]

		resp, err := client.postJSONWithAuth("/users/@me/email-change/verify-new", map[string]any{
			"ticket":         startResp.Ticket,
			"code":           newCode,
			"original_proof": "invalid-proof-token",
			"password":       account.Password,
		}, account.Token)
		if err != nil {
			t.Fatalf("failed to send request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK {
			t.Fatalf("expected verify-new to fail with invalid original_proof")
		}
	})

	t.Run("original_proof returned from start when require_original is false", func(t *testing.T) {
		account := createTestAccount(t, client)
		clearTestEmails(t, client)
		unclaimAccount(t, client, account.UserID)

		resp, err := client.postJSONWithAuth("/users/@me/email-change/start", map[string]any{}, account.Token)
		if err != nil {
			t.Fatalf("failed to start email change: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		var startResp emailChangeStartResponse
		decodeJSONResponse(t, resp, &startResp)
		resp.Body.Close()

		if startResp.RequireOriginal {
			t.Fatalf("expected require_original=false for unclaimed/unverified account")
		}

		if startResp.OriginalProof == nil || *startResp.OriginalProof == "" {
			t.Fatalf("expected original_proof in start response when require_original=false")
		}
	})

	t.Run("verify-original returns original_proof for verified email accounts", func(t *testing.T) {
		account := createTestAccount(t, client)
		clearTestEmails(t, client)

		startResp := startEmailChange(t, client, account, account.Password)

		if !startResp.RequireOriginal {
			t.Logf("account did not require original verification; treating as already verified path")
			if startResp.OriginalProof == nil || *startResp.OriginalProof == "" {
				t.Fatalf("expected original_proof when original verification is not required")
			}
			return
		}

		originalEmail := waitForEmail(t, client, "email_change_original", account.Email)
		originalCode := originalEmail.Metadata["code"]
		if originalCode == "" {
			t.Fatalf("expected original verification code in email metadata")
		}

		originalProof := verifyOriginalEmailChange(t, client, account, startResp.Ticket, originalCode, account.Password)
		if originalProof == "" {
			t.Fatalf("expected original_proof in verify-original response")
		}
	})
}
